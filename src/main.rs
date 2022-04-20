use anyhow::Context;
use clap::{CommandFactory as _, Parser};
use efmt::items::ModuleOrConfig;
use env_logger::Env;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator};
use std::io::Read as _;
use std::io::Write as _;
use std::path::{Path, PathBuf};

/// Erlang Code Formatter.
#[derive(Debug, Parser)]
#[clap(about, version)]
struct Opt {
    /// Maximum line length.
    ///
    /// Note that this is a soft limit. That is, some lines could exceed the limit after formatting.
    /// Besides, this limit doesn't apply to comments.
    #[clap(long, default_value_t = 120)]
    print_width: usize,

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

    /// Outputs debug log messages.
    #[clap(long)]
    verbose: bool,

    /// Where to search for include files to process Erlang `-include` directives.
    ///
    /// If omitted, "../", "../include/", "../src/" and "../test/" of the target file will be added as the include directories.
    #[clap(short = 'I', long = "include-search-dir")]
    include_dirs: Vec<PathBuf>,

    /// Format target files.
    ///
    /// `-` means the standard input.
    /// If no files are specified and any of `-c`, `-w` or `--show-files` options is specified,
    /// All of the files named `**.{hrl,erl,app.src}` and `**/rebar.config` are used as the default
    /// (note that files spcified by `.gitignore` will be ignored).
    files: Vec<PathBuf>,

    /// Executes formatting in parallel.
    #[clap(long)]
    parallel: bool,

    /// Disables `-include` and `-include_lib` processing.
    /// This could improve formatting speed. All unknown macros will be replaced with `EFMT_DUMMY` atom.
    #[clap(long)]
    disable_include: bool,

    /// Where to save the caches for the macro definitions collected during processing `-include` or `-include_lib` directives.
    #[clap(long, default_value = ".efmt/cache")]
    include_cache_dir: PathBuf,

    /// Disables include cache.
    #[clap(long)]
    disable_include_cache: bool,

    /// Disables formatting by default.
    /// efmt behaves as if there is a "% @efmt:off" comment at the head of the each target file.
    #[clap(long)]
    default_off: bool,

    /// Enable profiling by `pprof`. The profile report will be generated in `flamegraph.svg`.
    #[cfg(feature = "pprof")]
    #[clap(long)]
    profile: bool,
}

impl Opt {
    fn collect_default_files_if_need(&mut self) -> anyhow::Result<()> {
        if !self.files.is_empty() || !(self.check || self.write || self.show_files) {
            return Ok(());
        }

        self.files = efmt::files::collect_default_target_files()?;
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
        let mut format_options = efmt::Options::new()
            .max_columns(self.print_width)
            .include_dirs(self.include_dirs.clone());

        if !self.disable_include_cache {
            format_options = format_options.include_cache_dir(self.include_cache_dir.clone());
        }
        if self.disable_include {
            format_options = format_options.disable_include();
        }
        if self.default_off {
            format_options = format_options.default_off();
        }

        format_options
    }
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::parse();

    let loglevel = if opt.verbose { "debug" } else { "info" };
    env_logger::Builder::from_env(Env::default().default_filter_or(loglevel)).init();

    #[cfg(feature = "pprof")]
    if opt.profile {
        return efmt::profile::with_profile(|| main_with_opt(opt));
    }
    main_with_opt(opt)
}

fn main_with_opt(mut opt: Opt) -> anyhow::Result<()> {
    opt.collect_default_files_if_need()?;
    if opt.files.is_empty() {
        Opt::command().print_help()?;
        println!();
        std::process::exit(1);
    }

    if opt.show_files {
        for file in opt.files {
            if let Some(file) = file.to_str() {
                println!("{}", file);
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
) -> anyhow::Result<(String, String)> {
    let original = std::fs::read_to_string(&path)?;
    let formatted = format_options
        .clone()
        .format_file::<ModuleOrConfig, _>(path)?;
    Ok((original, formatted))
}

fn format_stdin(format_options: &efmt::Options) -> anyhow::Result<(String, String)> {
    let mut original = String::new();
    std::io::stdin().lock().read_to_string(&mut original)?;
    let formatted = format_options
        .clone()
        .format_text::<ModuleOrConfig>(&original)?;
    Ok((original, formatted))
}

fn format_file_or_stdin<P: AsRef<Path>>(
    format_options: &efmt::Options,
    path: P,
) -> anyhow::Result<(String, String)> {
    let (original, formatted) = if path.as_ref().to_str() == Some("-") {
        format_stdin(format_options)
    } else {
        format_file(format_options, &path)
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
        match format_file_or_stdin(format_options, file) {
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
                    print!("{}", formatted);
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
        eprintln!();
        anyhow::bail!(
            "Failed to format the following files:\n{}",
            error_files
                .iter()
                .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    } else if opt.write {
        log::info!("All files were formatted correctly!");
    }

    Ok(())
}

fn check_files(opt: &Opt) -> anyhow::Result<()> {
    let format_options = opt.to_format_options();

    fn do_check(format_options: &efmt::Options, file: &Path) -> bool {
        match format_file_or_stdin(format_options, file) {
            Err(e) => {
                log::error!("Failed to format {:?}\n{:?}", file, e);
                false
            }
            Ok((original, formatted)) => {
                if original == formatted {
                    log::info!("{:?} is already formatted correctly.", file);
                    true
                } else {
                    let diff = efmt::diff::text_diff(&original, &formatted);
                    log::info!("{:?} is not formatted correctly.\n{}", file, diff);
                    false
                }
            }
        }
    }

    let unformatted_files = if opt.parallel {
        opt.files
            .clone()
            .into_par_iter()
            .filter(|file| !do_check(&format_options, file))
            .collect::<Vec<_>>()
    } else {
        opt.files
            .iter()
            .filter(|file| !do_check(&format_options, file))
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
                efmt::error::generate_error_message(text, Some(path), p.into(), "extra token")
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
                    efmt::error::generate_error_message(
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
            efmt::error::generate_error_message(
                original,
                Some(path),
                t0.start_position().into(),
                "expected"
            ),
            efmt::error::generate_error_message(
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
    let mut temp = tempfile::NamedTempFile::new()?;
    temp.write_all(text.as_bytes())?;
    temp.persist(path)?;
    Ok(())
}
