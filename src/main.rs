use std::io::Read as _;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    file: Option<PathBuf>,
    // TODO: config
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let formatted_text = match opt.file {
        Some(path) => efmt::format_file(path)?,
        None => {
            let mut text = String::new();
            std::io::stdin().lock().read_to_string(&mut text)?;
            efmt::format_text(&text)?
        }
    };
    print!("{}", formatted_text);

    Ok(())
}
