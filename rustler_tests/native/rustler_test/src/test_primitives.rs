#[rustler::nif]
pub fn add_u32(a: u32, b: u32) -> u32 {
    a + b
}

#[rustler::nif]
pub fn add_i32(a: i32, b: i32) -> i32 {
    a + b
}

#[rustler::nif]
pub fn echo_u8(n: u8) -> u8 {
    n
}

#[rustler::nif]
pub fn option_inc(opt: Option<f64>) -> Option<f64> {
    opt.map(|num| num + 1.0)
}

#[rustler::nif]
pub fn result_to_int(res: Result<bool, &str>) -> Result<usize, String> {
    match res {
        Ok(true) => Ok(1),
        Ok(false) => Ok(0),
        Err(errstr) => Err(format!("{}{}", errstr, errstr)),
    }
}
