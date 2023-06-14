use std::io::{self, BufRead, BufReader, Read};

/// Takes an `R: BufRead` and:
///
/// - skips the leading UTF-8 BOM if there is one
/// - skips the unix shebang if there is one (if the first character is a '#', skips everything up
///   until but not including the first '\n')
///
/// This mimics the initial behavior of luaL_loadfile(x). In order to correctly detect and skip the
/// BOM and unix shebang, the internal buffer of the BufRead must be >= 3 bytes.
pub fn skip_prefix<R: BufRead>(r: &mut R) -> Result<(), io::Error> {
    if {
        let buf = r.fill_buf()?;
        buf.len() >= 3 && buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf
    } {
        r.consume(3);
    }

    let buf = r.fill_buf()?;
    let has_shebang = buf.len() >= 1 && buf[0] == b'#';
    if has_shebang {
        r.consume(1);
        loop {
            let to_consume = {
                let buf = r.fill_buf()?;
                let mut i = 0;
                loop {
                    if i >= buf.len() || buf[i] == b'\n' {
                        break i;
                    }
                    i += 1;
                }
            };

            if to_consume == 0 {
                break;
            } else {
                r.consume(to_consume);
            }
        }
    }

    Ok(())
}

/// Reads a Lua script from a `R: Read` and wraps it in a BufReader
///
/// Also calls `skip_prefix` to skip any leading UTF-8 BOM or unix shebang.
pub fn buffered_read<R: Read>(r: R) -> Result<BufReader<R>, io::Error> {
    let mut r = BufReader::new(r);
    skip_prefix(&mut r)?;
    Ok(r)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_prefix() {
        let test_file = [
            0xef, 0xbb, 0xbf, b'#', 0x00, 0x00, 0x00, 0xff, b'\n', 0x1, 0x2, 0x3,
        ];
        let mut reader = BufReader::with_capacity(3, &test_file[..]);

        skip_prefix(&mut reader).unwrap();

        let mut v = Vec::new();
        reader.read_to_end(&mut v).unwrap();
        assert_eq!(v, vec![b'\n', 0x1, 0x2, 0x3]);
    }
}
