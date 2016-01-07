/*
 * While the nif_interface module should directly export unsafe nif helper functions,
 * this module should preform validation and make them (reasonably) safe and easy to
 * use from rust. This module should try to be as nonopinionated as possible, and
 * should try to stick as close as possible to the original C api.
 *
 * Making the apis nice to use from rust should be done in the root rustler crate.
 */

pub mod nif_interface;

pub mod tuple;
pub use self::tuple::{ get_tuple };

pub mod map;
pub use self::map::{ get_map_value, get_map_size, map_new, map_put };

pub mod atom;

pub mod exception;

/*macro_rules! wrap_number {
    (
}*/
