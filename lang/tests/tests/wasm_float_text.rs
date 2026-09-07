use std::{fmt::Write, process::Command};

#[test]
fn wasm_float_text_matches_rust_for_boundaries_and_sampled_bit_patterns() {
    let mut samples = Vec::new();
    for (width, boundaries) in [
        (32, vec![0, 1, 0x7fffff, 0x800000, 0x3f800000, 0x7f7fffff, 0x7f800000, 0x7fc00000]),
        (
            64,
            vec![
                0,
                1,
                0xfffffffffffff,
                0x10000000000000,
                0x3ff0000000000000,
                0x7fefffffffffffff,
                0x7ff0000000000000,
                0x7ff8000000000000,
            ],
        ),
    ] {
        samples.extend(boundaries.into_iter().flat_map(|bits| {
            [bits, bits | (1_u64 << (width - 1))].into_iter().map(move |bits| (width, bits))
        }));
    }
    for exponent in -324..=308 {
        let value = 10.0_f64.powi(exponent);
        for (width, bits) in [(32, u64::from((value as f32).to_bits())), (64, value.to_bits())] {
            samples.extend((-2..=2).flat_map(|offset| {
                let bits = bits.wrapping_add_signed(offset);
                [bits, bits | (1_u64 << (width - 1))].into_iter().map(move |bits| (width, bits))
            }));
        }
    }
    let mut state = 0x789abcdef0123456_u64;
    for _ in 0..20_000 {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        samples.push((32, u64::from(state as u32)));
        samples.push((64, state));
    }
    let mut input = String::new();
    for (width, bits) in samples {
        let expected = if width == 32 {
            f32::from_bits(bits as u32).to_string()
        } else {
            f64::from_bits(bits).to_string()
        };
        writeln!(input, "{width}\t{bits:x}\t{expected}").unwrap();
    }
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("samples.tsv");
    std::fs::write(&path, input).unwrap();
    let output = Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([
            "--input-type=module",
            "--eval",
            r#"
import fs from 'node:fs';
import { FloatText } from './wasm-numeric.mjs';
const scratch = new DataView(new ArrayBuffer(8));
let failures = 0;
for (const line of fs.readFileSync(process.argv[1], 'utf8').trim().split('\n')) {
  const [widthText, bits, expected] = line.split('\t');
  const width = Number(widthText);
  scratch.setBigUint64(0, BigInt(`0x${bits}`), true);
  const value = width === 32 ? scratch.getFloat32(0, true) : scratch.getFloat64(0, true);
  const actual = FloatText.render(value, width);
  if (actual !== expected) {
    if (failures < 20) console.error(`${width} ${bits}: expected ${expected}, got ${actual}`);
    failures += 1;
  }
}
if (failures) console.error(`${failures} mismatches`);
process.exit(failures ? 1 : 0);
"#,
        ])
        .arg(path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}
