pub fn with_profile<F, T>(f: F) -> T
where
    F: FnOnce() -> T,
{
    let guard = pprof::ProfilerGuard::new(100).ok();
    let result = f();
    let _ = (|| {
        let report = guard?.report().build().ok()?;
        let file = std::fs::File::create("flamegraph.svg").ok()?;
        report.flamegraph(file).ok()?;
        log::info!("Generated profile report: flamegraph.svg");
        Some(())
    })();
    result
}
