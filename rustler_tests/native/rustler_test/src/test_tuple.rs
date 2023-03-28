#[rustler::nif]
pub fn add_from_tuple(items: (i64, i64)) -> i64 {
    items.0 + items.1
}

#[rustler::nif]
pub fn add_one_to_tuple(items: (usize, usize)) -> (usize, usize) {
    (items.0 + 1, items.1 + 1)
}

#[rustler::nif]
pub fn join_tuple_elements(items: (&str, &str)) -> String {
    format!("{}{}", items.0, items.1)
}

#[rustler::nif]
pub fn maybe_add_one_to_tuple(items: Option<(usize, usize)>) -> Option<(usize, usize)> {
    items.map(|tuple| (tuple.0 + 1, tuple.1 + 1))
}

#[rustler::nif]
pub fn add_i32_from_tuple(items: (i32, i32)) -> Result<i32, String> {
    match items.0.checked_add(items.1) {
        Some(number) => Ok(number),
        None => Err(format!(
            "Cannot sum {} + {} because the result is bigger than an i32 number.",
            items.0, items.1
        )),
    }
}

#[rustler::nif]
pub fn greeting_person_from_tuple(age_and_name: (u8, &str)) -> String {
    let (age, name) = age_and_name;

    if age > 18 {
        format!("Hello, {name}! You are allowed in the bar area.")
    } else {
        format!("Hi, {name}! I'm sorry, but you are not allowed in the bar area.")
    }
}
