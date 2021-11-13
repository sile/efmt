// #[derive(Debug, Clone, Default)]
// pub struct RegionOptions {
//     newline: bool,
//     multiline_mode: MultilineMode,
//     indent: IndentMode,
//     trailing_item_size: usize,
//     noretry: bool,
// }

// impl RegionOptions {
//     pub fn new() -> Self {
//         Self::default()
//     }

//     pub fn noretry(mut self) -> Self {
//         self.noretry = true;
//         self
//     }

//     pub fn newline(mut self) -> Self {
//         self.newline = true;
//         self
//     }

//     pub fn allow_multiline(mut self) -> Self {
//         self.multiline_mode = MultilineMode::Allow;
//         self
//     }

//     pub fn recommend_multiline(mut self) -> Self {
//         self.multiline_mode = MultilineMode::Recommend;
//         self
//     }

//     pub fn forbid_multiline(mut self) -> Self {
//         self.multiline_mode = MultilineMode::Forbid;
//         self
//     }

//     pub fn indent(mut self, indent: IndentMode) -> Self {
//         self.indent = indent;
//         self
//     }

//     pub fn trailing_item_size(mut self, n: usize) -> Self {
//         self.trailing_item_size = n;
//         self
//     }

//     fn to_transaction_config(&self, fmt: &Formatter) -> TransactionConfig {
//         let indent = match self.indent {
//             IndentMode::CurrentIndent { offset } => fmt.transaction.config().indent + offset,
//             IndentMode::CurrentColumn => {
//                 if fmt.transaction.last_char() == Some('\n') {
//                     fmt.transaction.config().indent
//                 } else {
//                     fmt.current_column()
//                 }
//             }
//         };
//         TransactionConfig {
//             indent,
//             max_columns: fmt
//                 .max_columns()
//                 .checked_sub(self.trailing_item_size)
//                 .expect("TODO"),
//             multiline_mode: self.multiline_mode,
//         }
//     }
// }
