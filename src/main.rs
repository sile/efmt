use anyhow::Context;
use clap::{CommandFactory as _, Parser};
use efmt::files::RebarConfigValue;
use efmt_core::items::ModuleOrConfig;
use env_logger::Env;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator};
use regex::Regex;
use std::io::Read as _;
use std::io::Write as _;
use std::path::{Path, PathBuf};

/// Erlang Code Formatter.
#[derive(Debug, Parser)]
#[clap(about, version)]
struct Opt {
    /// Checks if input is formatted correctly.
    ///
    /// If so, exits with 0. Otherwise, exits with 1 and shows a diff.
    #[clap(long, short)]
    check: bool,

    /// Overwrites input file with the formatted text.
    #[clap(long, short, conflicts_with = "check")]
    write: bool,

    /// Shows the target input files.
    ///
    /// You can use this flag to exclude some files from the default target, e.g., `$ efmt $(efmt --show-files | grep -v rebar.config)`.
    #[clap(long, conflicts_with = "check", conflicts_with = "write")]
    show_files: bool,

    /// Excludes files that matches the specified regexs from the default target file list.
    #[clap(short, long = "exclude-file")]
    exclude_files: Vec<regex::Regex>,

    /// Outputs debug log messages.
    #[clap(long)]
    verbose: bool,

    /// Format target files.
    ///
    /// `-` means the standard input.
    /// If no files are specified and any of `-c`, `-w` or `--show-files` options is specified,
    /// All of the files named `**.{hrl,erl,app.src}` and `**/rebar.config` are used as the default
    /// (note that files specified by `.gitignore` will be ignored).
    files: Vec<PathBuf>,

    /// Executes formatting in parallel.
    #[clap(long)]
    parallel: bool,

    /// Disables formatting by default.
    /// efmt behaves as if there is a "% @efmt:off" comment at the head of the each target file.
    #[clap(long)]
    default_off: bool,

    /// Don't assume that the target project is built using rebar3.
    #[clap(long)]
    disable_rebar3_mode: bool,

    /// Don't raise an error even if the input contains wrong Erlang code.
    /// `efmt` tries to continue formatting the remaining part of the code as much as possible.
    #[clap(long)]
    allow_partial_failure: bool,

    /// Show colored diff. Only applies when `--check` is given.
    #[clap(long)]
    color: bool,
}

impl Opt {
    fn collect_default_files_if_need(&mut self) -> anyhow::Result<()> {
        if !self.files.is_empty() || !(self.check || self.write || self.show_files) {
            return Ok(());
        }

        self.files = efmt::files::collect_default_target_files()?
            .into_iter()
            .filter(|path| {
                let path = path.to_string_lossy();
                self.exclude_files
                    .iter()
                    .all(|regex| !regex.is_match(&path))
            })
            .collect::<Vec<_>>();
        if !self.files.is_empty() && !self.show_files {
            log::info!(
                "The following files were added as the default input files:\n{}",
                self.files
                    .iter()
                    .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
        Ok(())
    }

    fn to_format_options(&self) -> efmt::Options {
        let mut format_options = efmt::Options::new();
        if self.default_off {
            format_options = format_options.default_off();
        }
        format_options
    }

    fn enable_rebar3_mode(&mut self, rebar_config_dir: PathBuf) -> anyhow::Result<()> {
        // rebar.config
        let rebar_config_path = rebar_config_dir.join("rebar.config");
        for value in efmt::files::load_rebar_config(&rebar_config_path)
            .with_context(|| format!("failed to load rebar.config file: {rebar_config_path:?}"))?
        {
            if let RebarConfigValue::Tuple(item) = value {
                if item.len() != 2 {
                    continue;
                }
                match (&item[0], &item[1]) {
                    (RebarConfigValue::Atom(key), RebarConfigValue::List(items))
                        if key == "efmt" =>
                    {
                        self.handle_rebar_config_efmt(items);
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    fn handle_rebar_config_efmt(&mut self, items: &[RebarConfigValue]) {
        for item in items {
            log::debug!("found an efmt option in rebar.config: {item:?}");
            if let RebarConfigValue::Atom(k) = item {
                match k.as_str() {
                    "parallel" => {
                        self.parallel = true;
                        continue;
                    }
                    "default_off" => {
                        self.default_off = true;
                        continue;
                    }
                    "allow_partial_failure" => {
                        self.allow_partial_failure = true;
                        continue;
                    }
                    _ => {}
                }
            } else if let Some((k, v)) = item.as_kv_tuple() {
                if k == "exclude_file" {
                    if let RebarConfigValue::String(v) = v {
                        match Regex::new(v) {
                            Ok(regex) => {
                                self.exclude_files.push(regex);
                            }
                            Err(e) => {
                                log::warn!("{v:?} is not a valid regex: {e}");
                            }
                        }
                        continue;
                    }
                }
            }
            log::warn!("found an unhandled efmt option in rebar.config: {item:?}");
        }
    }
}

fn main() -> anyhow::Result<()> {
    let mut opt = Opt::parse();

    let loglevel = if opt.verbose { "debug" } else { "info" };
    env_logger::Builder::from_env(Env::default().default_filter_or(loglevel)).init();

    if !opt.disable_rebar3_mode {
        if let Some(rebar_config_dir) = efmt::files::find_rebar_config_dir() {
            log::debug!("rebar.config file found: dir={rebar_config_dir:?}");
            opt.enable_rebar3_mode(rebar_config_dir)?;
        } else {
            log::debug!("rebar.config file not found");
        }
    }

    opt.collect_default_files_if_need()?;
    if opt.files.is_empty() {
        Opt::command().print_help()?;
        println!();
        std::process::exit(1);
    }

    if opt.show_files {
        for file in opt.files {
            if let Some(file) = file.to_str() {
                println!("{file}");
            }
        }
        Ok(())
    } else if opt.check {
        check_files(&opt)
    } else {
        format_files(&opt)
    }
}

fn format_file<P: AsRef<Path>>(
    format_options: &efmt::Options,
    path: P,
    allow_partial_failure: bool,
) -> anyhow::Result<(String, String)> {
    let original = std::fs::read_to_string(&path)?;
    let opt = format_options.clone();
    let formatted = if allow_partial_failure {
        opt.format_file::<ModuleOrConfig<true>, _>(path)?
    } else {
        opt.format_file::<ModuleOrConfig<false>, _>(path)?
    };
    Ok((original, formatted))
}

fn format_stdin(
    format_options: &efmt::Options,
    allow_partial_failure: bool,
) -> anyhow::Result<(String, String)> {
    let mut original = String::new();
    std::io::stdin().lock().read_to_string(&mut original)?;
    let opt = format_options.clone();
    let formatted = if allow_partial_failure {
        opt.format_text::<ModuleOrConfig<true>>(&original)?
    } else {
        opt.format_text::<ModuleOrConfig<false>>(&original)?
    };
    Ok((original, formatted))
}

fn format_file_or_stdin<P: AsRef<Path>>(
    format_options: &efmt::Options,
    path: P,
    allow_partial_failure: bool,
) -> anyhow::Result<(String, String)> {
    let (original, formatted) = if path.as_ref().to_str() == Some("-") {
        format_stdin(format_options, allow_partial_failure)
    } else {
        format_file(format_options, &path, allow_partial_failure)
    }?;
    validate_formatted_text(path, &original, &formatted).context(concat!(
        "Found a token mismatch between the original text ",
        "and the formatted one (maybe efmt bug)"
    ))?;
    Ok((original, formatted))
}

fn format_files(opt: &Opt) -> anyhow::Result<()> {
    let format_options = opt.to_format_options();

    fn do_format(opt: &Opt, format_options: &efmt::Options, file: &Path) -> anyhow::Result<()> {
        match format_file_or_stdin(format_options, file, opt.allow_partial_failure) {
            Err(e) => {
                log::error!("Failed to format {:?}\n{:?}", file, e);
                Err(e)
            }
            Ok((original, formatted)) => {
                if opt.write {
                    if original != formatted {
                        let result = overwrite(file, &formatted);
                        if let Err(e) = &result {
                            log::error!("Failed to write formatted text to {:?}: {:?}", file, e);
                        } else {
                            log::info!("Overwrote {:?}", file);
                        }
                        result
                    } else {
                        Ok(())
                    }
                } else {
                    print!("{formatted}");
                    Ok(())
                }
            }
        }
    }

    let error_files = if opt.parallel {
        opt.files
            .clone()
            .into_par_iter()
            .filter(|file| do_format(opt, &format_options, file).is_err())
            .collect::<Vec<_>>()
    } else {
        opt.files
            .iter()
            .filter(|file| do_format(opt, &format_options, file).is_err())
            .cloned()
            .collect::<Vec<_>>()
    };

    if !error_files.is_empty() {
        if opt.files.len() > 1 {
            eprintln!();
            anyhow::bail!(
                "Failed to format the following files:\n{}",
                error_files
                    .iter()
                    .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                    .collect::<Vec<_>>()
                    .join("\n"),
            );
        } else {
            std::process::exit(1);
        }
    } else if opt.write {
        log::info!("All files were formatted correctly!");
    }

    Ok(())
}

fn check_files(opt: &Opt) -> anyhow::Result<()> {
    let format_options = opt.to_format_options();

    fn do_check(
        format_options: &efmt::Options,
        file: &Path,
        allow_partial_failure: bool,
        color: bool,
    ) -> bool {
        match format_file_or_stdin(format_options, file, allow_partial_failure) {
            Err(e) => {
                log::error!("Failed to format {:?}\n{:?}", file, e);
                false
            }
            Ok((original, formatted)) => {
                if original == formatted {
                    log::info!("{file:?} is already formatted correctly.");
                    true
                } else {
                    if color {
                        efmt::diff::text_color_diff(&original, &formatted, file);
                    } else {
                        efmt::diff::text_diff(&original, &formatted, file);
                    }
                    log::info!("{file:?} is not formatted correctly.");
                    false
                }
            }
        }
    }

    let unformatted_files = if opt.parallel {
        opt.files
            .clone()
            .into_par_iter()
            .filter(|file| !do_check(&format_options, file, opt.allow_partial_failure, opt.color))
            .collect::<Vec<_>>()
    } else {
        opt.files
            .iter()
            .filter(|file| !do_check(&format_options, file, opt.allow_partial_failure, opt.color))
            .cloned()
            .collect::<Vec<_>>()
    };

    if !unformatted_files.is_empty() {
        eprintln!();
        anyhow::bail!(
            "The following files need to be formatted:\n{}",
            unformatted_files
                .iter()
                .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    } else {
        eprintln!("All input files are formatted correctly!");
    }
    Ok(())
}

fn validate_formatted_text<P: AsRef<Path>>(
    path: P,
    original: &str,
    formatted: &str,
) -> anyhow::Result<()> {
    use erl_tokenize::{PositionRange as _, Result, Token, Tokenizer};

    fn is_visible_token(t: &Result<Token>) -> bool {
        !matches!(t, Ok(Token::Whitespace(_)))
    }

    fn check_extra_token<P: AsRef<Path>>(
        path: P,
        text: &str,
        next_token: Option<Result<Token>>,
    ) -> anyhow::Result<()> {
        let next_position = next_token.map(|r| {
            r.map(|t| t.start_position())
                .unwrap_or_else(|e| e.position().clone())
        });
        if let Some(p) = next_position {
            anyhow::bail!(
                "{}",
                efmt_core::error::generate_error_message(text, Some(path), p.into(), "extra token")
            );
        }
        Ok(())
    }

    fn text(token: &Token) -> &str {
        if let Token::Comment(token) = token {
            token.text().trim_end()
        } else {
            token.text()
        }
    }

    let mut tokens0 = Tokenizer::new(original).filter(is_visible_token);
    let mut tokens1 = Tokenizer::new(formatted).filter(is_visible_token);
    while let Some(t0) = tokens0.next().transpose().expect("unreachable") {
        let t1 = match tokens1.next() {
            Some(Ok(t1)) => t1,
            Some(Err(e)) => {
                let reason = e.to_string();
                let reason_end = reason.find(" (").unwrap_or(reason.len());
                anyhow::bail!(
                    "{}",
                    efmt_core::error::generate_error_message(
                        formatted,
                        Some("<formatted>"),
                        e.position().clone().into(),
                        &reason[..reason_end]
                    )
                );
            }
            None => {
                return check_extra_token(path, original, Some(Ok(t0)));
            }
        };
        anyhow::ensure!(
            text(&t0) == text(&t1),
            "{}\n{}",
            efmt_core::error::generate_error_message(
                original,
                Some(path),
                t0.start_position().into(),
                "expected"
            ),
            efmt_core::error::generate_error_message(
                formatted,
                Some("<formatted>"),
                t1.start_position().into(),
                "actual"
            ),
        );
    }
    check_extra_token("<formatted>", formatted, tokens1.next())
}

fn overwrite<P: AsRef<Path>>(path: P, text: &str) -> anyhow::Result<()> {
    let dir = path
        .as_ref()
        .parent()
        .ok_or_else(|| anyhow::anyhow!("failed to get parent dir: {:?}", path.as_ref()))?;
    let mut temp = tempfile::NamedTempFile::new_in(dir)?;
    temp.write_all(text.as_bytes())?;
    temp.persist(path)?;
    Ok(())
}
