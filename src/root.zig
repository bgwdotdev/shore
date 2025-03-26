const std = @import("std");
const erl = @cImport(@cInclude("erl_nif.h"));
const c = @cImport({
    @cInclude("termios.h");
    @cInclude("unistd.h");
});

fn setCbreak() !void {
    const fd = c.STDIN_FILENO;
    var termios: c.struct_termios = undefined;
    const tty = try std.fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
    defer tty.close();
    var term = try std.posix.tcgetattr(tty.handle);
    term.lflag.ECHO = false;
    term.lflag.ICANON = false;
    termios.c_cc[c.VMIN] = 1;
    termios.c_cc[c.VTIME] = 0;
    _ = c.tcsetattr(fd, c.TCSANOW, &termios);
}

fn setsane() !void {
    const fd = c.STDIN_FILENO;
    var termios: c.struct_termios = undefined;
    const tty = try std.fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
    defer tty.close();
    var term = try std.posix.tcgetattr(tty.handle);
    term.lflag.ECHO = true;
    term.lflag.ICANON = true;
    //termios.c_cc[c.VMIN] = 1;
    //termios.c_cc[c.VTIME] = 0;
    _ = c.tcsetattr(fd, c.TCSANOW, &termios);
}
//
// FFI
//
export fn setCbreak_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    var i: i32 = 0;
    setCbreak() catch {
        i = 1;
    };
    return erl.enif_make_int(env, i);
}

export fn setsane_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    var i: i32 = 0;
    setsane() catch {
        i = 1;
    };
    return erl.enif_make_int(env, i);
}

export fn hello_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    return erl.enif_make_int(env, 10);
}

export var nif_funcs = [_]erl.ErlNifFunc{ .{
    .name = "hello_nif",
    .arity = 0,
    .fptr = hello_nif,
    .flags = 0,
}, .{
    .name = "setCbreak_nif",
    .arity = 0,
    .fptr = setCbreak_nif,
    .flags = 0,
}, .{
    .name = "setsane_nif",
    .arity = 0,
    .fptr = setsane_nif,
    .flags = 0,
} };

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
