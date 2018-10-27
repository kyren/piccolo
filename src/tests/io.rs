use std::io::{BufReader, Read};

use io::skip_prefix;

#[test]
fn test_skip_prefix() {
    let test_file = [
        0xef, 0xbb, 0xbf, b'#', 0x00, 0x00, 0x00, 0xff, b'\n', 0x1, 0x2, 0x3,
    ];
    let mut reader = BufReader::with_capacity(3, &test_file[..]);

    skip_prefix(&mut reader);

    let mut v = Vec::new();
    reader.read_to_end(&mut v);
    assert_eq!(v, vec![b'\n', 0x1, 0x2, 0x3]);
}
