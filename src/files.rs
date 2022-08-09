use crate::items::{Config, Expr};
use crate::parse::TokenStream;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn find_rebar_config_dir() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    while !dir.join("rebar.config").exists() {
        dir = dir.parent()?.to_path_buf();
    }
    Some(dir)
}

pub fn load_rebar_config<P: AsRef<Path>>(path: P) -> anyhow::Result<Vec<RebarConfigValue>> {
    let text = std::fs::read_to_string(&path)?;
    let mut tokenizer = erl_tokenize::Tokenizer::new(text);
    tokenizer.set_filepath(path);
    let mut ts = TokenStream::new(tokenizer);
    let config: Config = ts.parse()?;

    let mut values = Vec::new();
    for expr in config.exprs() {
        if let Some(x) = RebarConfigValue::from_expr(expr, &ts.text()) {
            values.push(x);
        }
    }
    Ok(values)
}

#[derive(Debug, Clone)]
pub enum RebarConfigValue {
    Atom(String),
    String(String),
    Integer(u32),
    List(Vec<Self>),
    Tuple(Vec<Self>),
}

impl RebarConfigValue {
    pub fn as_kv_tuple(&self) -> Option<(&str, &Self)> {
        if let Self::Tuple(kv) = self {
            if kv.len() == 2 {
                if let Self::Atom(k) = &kv[0] {
                    return Some((k.as_str(), &kv[1]));
                }
            }
        }
        None
    }
}

impl RebarConfigValue {
    fn from_expr(expr: &Expr, text: &str) -> Option<Self> {
        if let Some(x) = expr.as_atom() {
            Some(Self::Atom(x.to_owned()))
        } else if let Some(x) = expr.as_string() {
            Some(Self::String(x.to_owned()))
        } else if let Some(x) = expr.as_u32(text) {
            Some(Self::Integer(x))
        } else if let Some(xs) = expr.as_list() {
            Some(Self::List(
                xs.iter().filter_map(|x| Self::from_expr(x, text)).collect(),
            ))
        } else if let Some((tag, xs)) = expr.as_tuple() {
            Some(Self::Tuple(
                tag.iter()
                    .map(|tag| Self::Atom(tag.value().to_owned()))
                    .chain(xs.iter().filter_map(|x| Self::from_expr(x, text)))
                    .collect(),
            ))
        } else {
            None
        }
    }
}

pub fn collect_default_target_files() -> anyhow::Result<Vec<PathBuf>> {
    let current_dir = std::env::current_dir()?;
    if is_git_repository(&current_dir) {
        collect_files_with_git(is_format_target)
    } else {
        collect_files_without_git(current_dir, is_format_target)
    }
}

fn collect_files_with_git<F>(is_target: F) -> anyhow::Result<Vec<PathBuf>>
where
    F: Fn(&Path) -> bool,
{
    let mut files = Vec::new();
    let args_list = [
        &["ls-files"][..],
        &["ls-files", "--others", "--exclude-standard"][..],
    ];
    for args in args_list {
        let output = Command::new("git").args(args).output()?;
        anyhow::ensure!(
            output.status.success(),
            "Failed to execute `$ git {}` command.\n{}",
            Vec::from(args).join(" "),
            String::from_utf8_lossy(&output.stderr)
        );
        for file in String::from_utf8(output.stdout)?.split('\n') {
            if file.is_empty() {
                continue;
            }

            let path = PathBuf::from(file);
            if !is_target(&path) {
                continue;
            }
            files.push(path);
        }
    }
    Ok(files)
}

fn collect_files_without_git<P: AsRef<Path>, F>(
    root_dir: P,
    is_target: F,
) -> anyhow::Result<Vec<PathBuf>>
where
    F: Fn(&Path) -> bool,
{
    let mut files = Vec::new();
    let mut stack = vec![root_dir.as_ref().to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                stack.push(entry.path());
            } else if entry.file_type()?.is_file() {
                let path = entry.path();
                if is_target(&path) {
                    files.push(path);
                }
            }
        }
    }
    Ok(files)
}

fn is_format_target(path: &Path) -> bool {
    path.file_name()
        .and_then(|n| n.to_str())
        .map_or(false, |n| {
            n == "rebar.config"
                || n.ends_with(".erl")
                || n.ends_with(".hrl")
                || n.ends_with(".app.src")
        })
}

fn is_git_repository<P: AsRef<Path>>(dir: P) -> bool {
    let mut dir = dir.as_ref();
    while !dir.join(".git/").exists() {
        if let Some(parent) = dir.parent() {
            dir = parent;
        } else {
            return false;
        }
    }
    log::debug!("Found `.git` in {:?}", dir);
    true
}
