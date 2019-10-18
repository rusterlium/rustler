use std::ops::RangeInclusive;

#[rustler::nif]
pub fn sum_range(range: RangeInclusive<i64>) -> i64 {
    range.sum()
}
