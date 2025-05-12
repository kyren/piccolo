#[cfg(feature = "std")]
pub fn try_powf(a: f64, b: f64) -> Option<f64> {
    Some(f64::powf(a, b))
}

#[cfg(not(feature = "std"))]
pub fn try_powf(a: f64, b: f64) -> Option<f64> {
    // For now, disable powf on #![no_std] platforms.
    // We could use a software implementation like the libm crate, but
    // that will be inefficient; the ideal solution would be to use a
    // proper libm for the platform, but that has portability issues.
    None
}

#[cfg(feature = "std")]
pub fn floor(val: f64) -> f64 {
    val.floor()
}

#[cfg(not(feature = "std"))]
pub fn floor(val: f64) -> f64 {
    floor_manual(val)
}

#[allow(dead_code)]
fn floor_manual(val: f64) -> f64 {
    if !val.is_finite() {
        return val;
    }
    let mut bits = val.to_bits();
    let exp = ((bits >> 52) & 0x7ff) as i32 - 1023;
    if exp >= 0 {
        // abs(val) >= 1
        let mask = (1 << u32::saturating_sub(52, exp.unsigned_abs())) - 1;
        if val.is_sign_negative() {
            bits += mask; // Note: this may overflow into the exponent
        }
        f64::from_bits(bits & !mask)
    } else if val.is_sign_negative() {
        if (bits << 1) == 0 {
            -0.0 // floor(-0) = -0
        } else {
            -1.0 // floor(-1 < val < 0) = -1
        }
    } else {
        0.0 // floor(0 < val < 1) = 0
    }
}

#[test]
#[ignore]
fn floor_f32_exhaustive() {
    for x in 0..u32::MAX {
        let f = f32::from_bits(x) as f64;
        assert_eq!(floor_manual(f).to_bits(), f64::floor(f).to_bits(), "{}", f);
    }
}

#[test]
fn floor_basic() {
    let tests = [
        0.0,
        0.1,
        0.5,
        1.0,
        1.01,
        f64::INFINITY,
        (1u64 << 53) as f64,
        ((1u64 << 53) + 1) as f64,
    ];
    for f in tests {
        assert_eq!(floor_manual(f).to_bits(), f64::floor(f).to_bits(), "{}", f);
        let f = -f;
        assert_eq!(floor_manual(f).to_bits(), f64::floor(f).to_bits(), "{}", f);
    }
}
