use colored::Colorize as _;
use log::{Level, LevelFilter, Log, Metadata, Record};
use std::io::{IsTerminal as _, Write as _};
use std::sync::{Mutex, Once, OnceLock};

static LOGGER: OnceLock<SimpleLogger> = OnceLock::new();
static INSTALL: Once = Once::new();

pub(crate) fn init(verbose: bool) {
    let max_level = if verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Info
    };

    INSTALL.call_once(|| {
        let use_color = std::io::stderr().is_terminal();
        let logger = LOGGER.get_or_init(|| SimpleLogger::new(use_color));
        let _ = log::set_logger(logger);
    });
    log::set_max_level(max_level);
}

struct SimpleLogger {
    use_color: bool,
    write_lock: Mutex<()>,
}

impl SimpleLogger {
    fn new(use_color: bool) -> Self {
        Self {
            use_color,
            write_lock: Mutex::new(()),
        }
    }

    fn level_field(&self, level: Level) -> String {
        let label = level.as_str();
        let padding = " ".repeat(5usize.saturating_sub(label.len()));
        if !self.use_color {
            return format!("{label}{padding}");
        }

        let colored = match level {
            Level::Error => label.red().bold(),
            Level::Warn => label.yellow().bold(),
            Level::Info => label.green(),
            Level::Debug => label.blue(),
            Level::Trace => label.magenta(),
        };
        format!("{colored}{padding}")
    }
}

impl Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        metadata.level() <= log::max_level()
    }

    fn log(&self, record: &Record<'_>) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let _guard = self.write_lock.lock().unwrap_or_else(|e| e.into_inner());
        let mut stderr = std::io::stderr().lock();
        let _ = writeln!(
            stderr,
            "[{} {} {}] {}",
            format_utc_timestamp_now(),
            self.level_field(record.level()),
            record.target(),
            record.args(),
        );
    }

    fn flush(&self) {
        let _guard = self.write_lock.lock().unwrap_or_else(|e| e.into_inner());
        let _ = std::io::stderr().lock().flush();
    }
}

fn format_utc_timestamp_now() -> String {
    let unix_seconds = match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(duration) => duration.as_secs().min(i64::MAX as u64) as i64,
        Err(error) => {
            let duration = error.duration();
            let seconds = duration.as_secs().min(i64::MAX as u64) as i64;
            let seconds = if duration.subsec_nanos() == 0 {
                seconds
            } else {
                seconds.saturating_add(1)
            };
            -seconds
        }
    };
    format_utc_timestamp(unix_seconds)
}

fn format_utc_timestamp(unix_seconds: i64) -> String {
    let days = unix_seconds.div_euclid(86_400);
    let seconds_of_day = unix_seconds.rem_euclid(86_400);

    let (year, month, day) = civil_from_days(days);
    let hour = seconds_of_day / 3_600;
    let minute = (seconds_of_day % 3_600) / 60;
    let second = seconds_of_day % 60;

    format!("{year:04}-{month:02}-{day:02}T{hour:02}:{minute:02}:{second:02}Z")
}

fn civil_from_days(days_since_unix_epoch: i64) -> (i64, u32, u32) {
    let z = days_since_unix_epoch + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let day_of_era = z - era * 146_097;
    let year_of_era =
        (day_of_era - day_of_era / 1_460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let year = year_of_era + era * 400;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_prime = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_prime + 2) / 5 + 1;
    let month = month_prime + if month_prime < 10 { 3 } else { -9 };
    let year = year + if month <= 2 { 1 } else { 0 };
    (year, month as u32, day as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timestamp_format_works() {
        assert_eq!(format_utc_timestamp(0), "1970-01-01T00:00:00Z");
        assert_eq!(format_utc_timestamp(-1), "1969-12-31T23:59:59Z");
        assert_eq!(format_utc_timestamp(1_704_067_200), "2024-01-01T00:00:00Z");
    }

    #[test]
    fn level_field_alignment_works() {
        let logger = SimpleLogger::new(false);
        assert_eq!(logger.level_field(Level::Info), "INFO ");
        assert_eq!(logger.level_field(Level::Warn), "WARN ");
        assert_eq!(logger.level_field(Level::Debug), "DEBUG");
    }
}
