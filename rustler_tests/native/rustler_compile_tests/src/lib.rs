pub mod lifetimes {
    use rustler::{Binary, NifMap, NifRecord, NifStruct, NifTuple};

    #[derive(NifMap)]
    pub struct GenericMap<'a, 'b> {
        pub i: Binary<'a>,
        pub j: Binary<'b>,
    }

    #[derive(NifStruct)]
    #[module = "GenericStruct"]
    pub struct GenericStruct<'a, 'b> {
        pub i: Binary<'a>,
        pub j: Binary<'b>,
    }

    #[derive(NifRecord)]
    #[tag = "generic_record"]
    pub struct GenericRecord<'a, 'b> {
        pub i: Binary<'a>,
        pub j: Binary<'b>,
    }

    #[derive(NifTuple)]
    pub struct GenericTuple<'a, 'b>(Binary<'a>, Binary<'b>);

    #[derive(NifMap)]
    pub struct WhereClause<'a, 'b>
    where
        'a: 'b,
    {
        pub i: Binary<'a>,
        pub j: Binary<'b>,
    }

    #[derive(NifMap)]
    pub struct LifetimeBounded<'a, 'b: 'a> {
        pub i: Binary<'a>,
        pub j: Binary<'b>,
    }
}
