use std::io::{self, Read};

/// Read single characters at a time from an implementation of Read with N character lookahead.
pub struct Stream<R: Read> {
    // When the source end is reached, this will be set to None
    source: Option<R>,
    peek_buffer: Vec<u8>,
}

impl<R: Read> Stream<R> {
    pub fn new(source: R) -> Stream<R> {
        Stream {
            source: Some(source),
            peek_buffer: Vec::new(),
        }
    }

    /// Look at the nth character after the current read position.  `peek(0)` looks at the
    /// immediately next character, `peek(1)` the character after that, and so on.
    pub fn peek(&mut self, skip: u8) -> Result<Option<u8>, io::Error> {
        let skip = skip as usize;
        while self.peek_buffer.len() <= skip as usize {
            if let Some(c) = self.read_char()? {
                self.peek_buffer.push(c);
            } else {
                break;
            }
        }

        Ok(self.peek_buffer.get(skip).cloned())
    }

    /// Advance the read position by the given amount.  `advance(1)` advances the read position by
    ///  1, `advance(0)` does nothing.
    pub fn advance(&mut self, n: u8) -> Result<(), io::Error> {
        let n = n as usize;
        if n < self.peek_buffer.len() {
            self.peek_buffer.drain(0..n);
        } else {
            let plen = self.peek_buffer.len();
            self.peek_buffer.clear();

            for _ in plen..n {
                if self.read_char()?.is_none() {
                    break;
                }
            }
        }

        Ok(())
    }

    /// Stop reading from the source, ending the stream and clearing the peek buffer.
    pub fn stop(&mut self) {
        self.source = None;
        self.peek_buffer.clear();
    }

    pub fn at_end(&self) -> bool {
        self.source.is_none() && self.peek_buffer.is_empty()
    }

    // Read a single character, clearing the source on EOF or Error.
    fn read_char(&mut self) -> Result<Option<u8>, io::Error> {
        let mut at_end = false;
        let mut c = [0];

        let res = if let Some(source) = self.source.as_mut() {
            loop {
                match source.read(&mut c) {
                    Ok(0) => {
                        at_end = true;
                        break Ok(None);
                    }
                    Ok(_) => {
                        break Ok(Some(c[0]));
                    }
                    Err(e) => {
                        if e.kind() != io::ErrorKind::Interrupted {
                            at_end = true;
                            break Err(e);
                        }
                    }
                }
            }
        } else {
            Ok(None)
        };

        if at_end {
            self.source = None;
        }

        res
    }
}
