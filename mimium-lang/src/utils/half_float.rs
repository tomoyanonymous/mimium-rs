use half::f16;
use std::fmt::Display;
/// Half-Precision floating point type that can be converted from 64bit float with truncation checking.

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct HFloat(f16);

impl Display for HFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<f16> for HFloat {
    fn from(value: f16) -> Self {
        Self(value)
    }
}
impl Into<f64> for HFloat {
    fn into(self) -> f64 {
        self.0.to_f64()
    }
}

impl TryFrom<f64> for HFloat {
    type Error = ();

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        // 64bit :1bit sign, 11bit exponent, 52bit fraction
        // 16bit :1bit sign,  5bit exponent, 11bit fraction
        let exp_bitmask: u64 =
            0b01111110_00000000_00000000_00000000_00000000_00000000_00000000_00000000;
        let frac_bitmask: u64 =
            0b00000000_00001111_11111111_11111111_11111111_11111111_11111000_00000000;
        let v_u64 = value.to_bits();
        let exp_residue = (v_u64 & exp_bitmask);
        let frac_residue = (v_u64 & frac_bitmask);
        if exp_residue == 0 && frac_residue == 0 {
            Ok(Self(f16::from_f64(value)))
        } else {
            Err(())
        }
    }
}
