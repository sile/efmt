use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Opt {
    Parse(efmt::commands::ParseOpt),
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();
    match opt {
        Opt::Parse(opt) => opt.run()?,
    }
    Ok(())
}
