/// Controls how much driver diagnostic output is emitted.
///
/// Level 0 is silent, 1 enables info logs, 2 enables the pass dumps that used
/// to be guarded by the boolean verbose flag, and 3+ additionally enables deep
/// backend diagnostics.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub struct Verbosity {
    level: u8,
}

impl Verbosity {
    pub const fn new(level: u8) -> Self {
        Self { level }
    }

    pub const fn silent() -> Self {
        Self::new(0)
    }

    pub const fn level(self) -> u8 {
        self.level
    }

    pub const fn enables_stage_dumps(self) -> bool {
        self.level >= 2
    }

    pub const fn enables_deep_diagnostics(self) -> bool {
        self.level >= 3
    }

    pub const fn enables_runtime_trace_env(self) -> bool {
        self.level >= 3
    }

    pub fn init_logger(self) {
        let Some(level) = self.log_level() else {
            return;
        };

        let mut builder = env_logger::Builder::new();
        builder.filter_level(level);
        let _ = builder.try_init();
    }

    fn log_level(self) -> Option<log::LevelFilter> {
        match self.level {
            | 0 => None,
            | 1 => Some(log::LevelFilter::Info),
            | _ => Some(log::LevelFilter::Trace),
        }
    }
}

impl From<u8> for Verbosity {
    fn from(level: u8) -> Self {
        Self::new(level)
    }
}
