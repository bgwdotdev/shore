const std = @import("std");
const erl = @cImport(@cInclude("erl_nif.h"));

//
// FUNC
//

fn size() std.posix.winsize {
    var ws: std.posix.winsize = undefined;
    _ = std.os.linux.ioctl(std.os.linux.STDOUT_FILENO, std.os.linux.T.IOCGWINSZ, @intFromPtr(&ws));
    sigwinch();

    return ws;
}

fn sigwinch() void {
    var act: std.posix.Sigaction = .{
        .handler = .{ .handler = sigwinch_handler },
        .mask = std.posix.empty_sigset,
        .flags = 0,
    };
    std.posix.sigaction(std.os.linux.SIG.WINCH, &act, null);
}

fn sigwinch_handler(sig: i32) callconv(.c) void {
    _ = sig;
    std.debug.print("resized!", .{});
}

fn cbreak() !void {
    const tty = try std.fs.openFileAbsolute("/dev/tty", .{ .mode = .read_write });
    defer tty.close();
    var term = try std.posix.tcgetattr(tty.handle);
    term.lflag.ECHO = false;
    term.lflag.ICANON = false;
    term.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    term.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    try std.posix.tcsetattr(tty.handle, std.posix.TCSA.NOW, term);
}

//
// FFI
//

export fn size_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    const ws = size();
    return erl.enif_make_tuple2(env, erl.enif_make_int(env, ws.row), erl.enif_make_int(env, ws.col));
}

export fn cbreak_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    var i: i32 = 0;
    cbreak() catch {
        i = 1;
    };
    return erl.enif_make_int(env, i);
}

export fn hello_nif(env: ?*erl.ErlNifEnv, argc: c_int, argv: [*c]const erl.ERL_NIF_TERM) erl.ERL_NIF_TERM {
    _ = argc;
    _ = argv;
    sigwinch();
    return erl.enif_make_int(env, 10);
}

export var nif_funcs = [_]erl.ErlNifFunc{ .{
    .name = "hello_nif",
    .arity = 0,
    .fptr = hello_nif,
    .flags = 0,
}, .{
    .name = "cbreak_nif",
    .arity = 0,
    .fptr = cbreak_nif,
    .flags = 0,
}, .{
    .name = "size_nif",
    .arity = 0,
    .fptr = size_nif,
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
