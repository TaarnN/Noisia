use std::env;
use std::io::{self, IsTerminal};

#[derive(Debug, Clone, Copy)]
pub enum ColorPolicy {
    Auto,
}

#[derive(Debug, Clone)]
pub struct Style {
    enabled: bool,
}

impl Style {
    pub fn plain() -> Self {
        Self { enabled: false }
    }

    pub fn auto_stdout() -> Self {
        Self::from_policy(ColorPolicy::Auto, io::stdout().is_terminal())
    }

    pub fn auto_stderr() -> Self {
        Self::from_policy(ColorPolicy::Auto, io::stderr().is_terminal())
    }

    fn from_policy(policy: ColorPolicy, is_tty: bool) -> Self {
        let mut enabled = match policy {
            ColorPolicy::Auto => is_tty,
        };

        if env::var_os("NO_COLOR").is_some() {
            enabled = false;
        }

        match env::var("TERM") {
            Ok(term) if term.is_empty() || term == "dumb" => enabled = false,
            _ => {}
        }

        Self { enabled }
    }

    pub fn paint(&self, text: &str, codes: &[&str]) -> String {
        if !self.enabled || text.is_empty() {
            return text.to_string();
        }

        let mut code = String::new();
        for (idx, part) in codes.iter().enumerate() {
            if idx > 0 {
                code.push(';');
            }
            code.push_str(part);
        }

        format!("\x1b[{}m{}\x1b[0m", code, text)
    }

    pub fn bold(&self, text: &str) -> String {
        self.paint(text, &["1"])
    }

    pub fn dim(&self, text: &str) -> String {
        self.paint(text, &["2"])
    }

    pub fn underline(&self, text: &str) -> String {
        self.paint(text, &["4"])
    }

    pub fn fg_cyan(&self, text: &str) -> String {
        self.paint(text, &["36"])
    }

    pub fn fg_magenta(&self, text: &str) -> String {
        self.paint(text, &["35"])
    }

    pub fn fg_green(&self, text: &str) -> String {
        self.paint(text, &["32"])
    }

    pub fn fg_yellow(&self, text: &str) -> String {
        self.paint(text, &["33"])
    }

    pub fn fg_red(&self, text: &str) -> String {
        self.paint(text, &["31"])
    }

    pub fn fg_gray(&self, text: &str) -> String {
        self.paint(text, &["90"])
    }
}
