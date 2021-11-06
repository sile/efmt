use std::io::Write;

#[derive(Debug, Clone, Default)]
struct WriterState {
    columns: usize,
    max_columns_exceeded: bool,
    include_newline: bool,
}

#[derive(Debug)]
pub struct Writer<W> {
    inner: W,
    max_columns: usize,
    transactions: Vec<TransactionWriter>,
    state: WriterState,
}

impl<W: Write> Writer<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            max_columns: 100,
            transactions: Vec::new(),
            state: WriterState::default(),
        }
    }

    // TODO: consider transaction
    pub fn set_max_columns(&mut self, max: usize) {
        self.max_columns = max;
    }

    pub fn max_columns(&self) -> usize {
        self.max_columns
    }

    pub fn start_transaction(&mut self) {
        self.transactions.push(TransactionWriter {
            buf: Vec::new(),
            start_state: self.state.clone(),
        });
    }

    pub fn commit(&mut self) -> std::io::Result<()> {
        let w = self.transactions.pop().unwrap(); // TODO
        self.state = w.start_state;
        self.write_all(&w.buf)?;
        Ok(())
    }

    pub fn rollback(&mut self) {
        let w = self.transactions.pop().unwrap();
        self.state = w.start_state;
    }

    pub fn max_columns_exceeded(&self) -> bool {
        self.state.max_columns_exceeded
    }

    pub fn columns(&self) -> usize {
        self.state.columns
    }

    // TODO
    // pub fn include_newline(&self) ->bool {
    //     self.include_newline
    // }
}

impl<W: Write> Write for Writer<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let written = if let Some(w) = self.transactions.last_mut() {
            w.write(buf)?
        } else {
            self.inner.write(buf)?
        };

        let mut buf = &buf[..written];
        while !buf.is_empty() {
            if let Some(i) = buf.iter().position(|&b| b == b'\n') {
                self.state.columns += i;
                if self.state.columns > self.max_columns {
                    self.state.max_columns_exceeded = true;
                }
                self.state.include_newline = true;
                buf = &buf[i + 1..];
                self.state.columns = 0;
            } else {
                self.state.columns += buf.len();
                if self.state.columns > self.max_columns {
                    self.state.max_columns_exceeded = true;
                }
                break;
            }
        }

        // TODO:
        // if self.max_columns_exceeded {
        //     return Err(std::io::Error::new(
        //         std::io::ErrorKind::Other,
        //         "exceeded max columns",
        //     ));
        // }

        Ok(written)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(w) = self.transactions.last_mut() {
            w.flush()
        } else {
            self.inner.flush()
        }
    }
}

#[derive(Debug)]
struct TransactionWriter {
    buf: Vec<u8>,
    start_state: WriterState,
}

impl Write for TransactionWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
