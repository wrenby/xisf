use std::{fs::File, io::{BufWriter, Write}};

use ndarray::Array3;

// basically just somewhere I can throw some code I want to run sometimes, rather than making a new binary in the workspace or something
// run with `cargo test --package xisf-rs --test filegen -- make_files`
#[allow(dead_code)]
// #[test]
fn make_files() {
    // https://stackoverflow.com/a/56762490
    let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
    for ((x, y, z), v) in array.indexed_iter_mut() {
        *v = match z {
            0 => y as u8,
            1 => x as u8,
            2 => 255 - (x as u8).min(y as u8),
            _ => unreachable!(),
        };
    }

    let mut file = BufWriter::new(File::create("tests/files/gradient.bin").unwrap());
    let bytes = bytemuck::cast_slice(array.as_slice().unwrap());
    file.write_all(bytes).unwrap();
}