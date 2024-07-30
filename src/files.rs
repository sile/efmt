use efmt_core::items::{Config, Expr};
use efmt_core::parse::{TokenStream, Tokenizer};
use ignore::Walk;
use std::path::{Path, PathBuf};

pub fn find_rebar_config_dir() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    while !dir.join("rebar.config").exists() {
        dir = dir.parent()?.to_path_buf();
    }
    Some(dir)
}

pub fn load_rebar_config<P: AsRef<Path>>(path: P) -> anyhow::Result<Vec<RebarConfigValue>> {
    let text = std::fs::read_to_string(&path)?;
    let mut tokenizer = Tokenizer::new(text);
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
    let mut files = Vec::new();
    for result in Walk::new("./") {
        let entry = result?;
        if entry.file_type().map_or(false, |t| t.is_file()) {
            let path = entry.path();
            if is_format_target(path) {
                let path = path.strip_prefix("./").unwrap_or(path).to_path_buf();
                files.push(path);
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
