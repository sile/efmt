use efmt::items::module::Module;
use std::io::Read as _;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    file: Option<PathBuf>,
    #[structopt(long, default_value = "100")]
    max_columns: usize,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let format_options = efmt::FormatOptions::<Module>::new().max_columns(opt.max_columns);
    let formatted_text = match opt.file {
        Some(path) => format_options.format_file(path)?,
        None => {
            let mut text = String::new();
            std::io::stdin().lock().read_to_string(&mut text)?;
            format_options.format_text(&text)?
        }
    };
    println!("{}", formatted_text);

    Ok(())
}
