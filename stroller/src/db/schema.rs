#![allow(proc_macro_derive_resolution_fallback)]

table! {
    // We have to tell diesel the primary key of the table. stored_events_v2
    // doesn't actually have a primary key, so we use the semi-documented
    // workaround of lying to diesel and naming a random column.  Since this is
    // a lie, we shouldn't use diesel's .find() method, but it shouldn't
    // otherwise get in our way.
    stored_events_v2 (trace_id) {
        canvas_id -> Uuid,
        trace_id -> Uuid,
        timestamp -> Timestamptz,
        module -> Text,
        path -> Text,
        value -> Text,
    }
}
