use anyhow::Context;
use efmt::items::ModuleOrConfig;
use env_logger::Env;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use structopt::StructOpt;

/// Erlang Code Formatter.
#[derive(Debug, StructOpt)]
struct Opt {
    /// Maximum line length.
    ///
    /// Note that this is a soft limit. That is, some lines could exceed the limit after formatting.
    /// Besides, this limit doesn't apply to comments.
    #[structopt(long, default_value = "120")]
    print_width: usize,

    /// Checks if input is formatted correctly.
    ///
    /// If so, exits with 0. Otherwise, exits with 1 and shows a diff.
    #[structopt(long, short = "c")]
    check: bool,

    /// Where to search for include files to process Erlang `-include` directives.
    #[structopt(short = "I", long = "include-search-dir")]
    include_dirs: Vec<PathBuf>,

    /// Format target files.
    ///
    /// `-` means the standard input.
    /// If no files are specified and one of `-c`, `-w` or `-o` options is specified,
    /// `{src,include,test}/*.{hrl,erl,app.src}` and `rebar.config` are used as the default.
    files: Vec<PathBuf>,

    // --verbose
    // --parallelism
    // `-disable-include`
    // `-disable-include-cache`
    #[structopt(long, default_value = ".efmt/cache")]
    include_cache_dir: PathBuf,

    #[structopt(long)]
    disable_include_cache: bool,

    /// Enable profiling by `pprof`. The profile report will be generated in `flamegraph.svg`.
    #[cfg(feature = "pprof")]
    #[structopt(long)]
    profile: bool,
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(Env::default().default_filter_or("warn")).init();

    let opt = Opt::from_args();

    #[cfg(feature = "pprof")]
    let guard = if opt.profile {
        Some(pprof::ProfilerGuard::new(100)?)
    } else {
        None
    };

    let result = if opt.check {
        todo!()
    } else {
        format_files(&opt)
    };

    #[cfg(feature = "pprof")]
    if let Some(report) = guard.map(|x| x.report().build()).transpose()? {
        let file = std::fs::File::create("flamegraph.svg")?;
        report.flamegraph(file)?;
        log::info!("Generated profile report: flamegraph.svg");
    };

    result
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

fn format_files(opt: &Opt) -> anyhow::Result<()> {
    if opt.files.is_empty() {
        Opt::clap().print_help()?;
        println!();
        std::process::exit(1);
    }

    let mut format_options = efmt::Options::new()
        .max_columns(opt.print_width)
        .include_dirs(opt.include_dirs.clone());

    if !opt.disable_include_cache {
        format_options = format_options.include_cache_dir(opt.include_cache_dir.clone());
    }

    let mut error_files = Vec::new();
    for file in &opt.files {
        let result: anyhow::Result<_> = (|| {
            let (original, formatted) = if file.to_str() == Some("-") {
                format_stdin(&format_options)?
            } else {
                format_file(&format_options, file)?
            };
            validate_formatted_text(&file, &original, &formatted).context(concat!(
                "Found a token mismatch between the original text ",
                "and the formatted one (maybe efmt bug)"
            ))?;
            Ok(formatted)
        })();
        match result {
            Err(e) => {
                log::error!(
                    "Failed to format {:?}\n{}",
                    file,
                    e.chain()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                );
                error_files.push(file);
            }
            Ok(formatted) => {
                print!("{}", formatted);
            }
        }
    }

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
                let reason_end = reason.find(" (").unwrap_or_else(|| reason.len());
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
