const std = @import("std");
const erl = @cImport(@cInclude("erl_nif.h"));

//
// FFI
//

export fn hello_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    return erl.enif_make_int(env, 10);
}

export var nif_funcs = [_]erl.ErlNifFunc{.{
    .name = "hello_nif",
    .arity = 0,
    .fptr = hello_nif,
    .flags = 0,
}};

//
// INIT
//

var entry: erl.ErlNifEntry = .{
    .major = erl.ERL_NIF_MAJOR_VERSION,
    .minor = erl.ERL_NIF_MINOR_VERSION,
    .name = "shore_ffi",
    .num_of_funcs = nif_funcs.len,
    .funcs = &nif_funcs,
    .load = null,
    .reload = null,
    .upgrade = null,
    .unload = null,
    .vm_variant = "beam.vanilla",
    .options = 0,
};

export fn nif_init() *const erl.ErlNifEntry {
    return &entry;
}
