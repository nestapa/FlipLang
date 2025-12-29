const std = @import("std");
const builtin = @import("builtin");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;
const cgi = @import("cgi.zig");
const Preprocessor = @import("preprocessor.zig").Preprocessor;
const Server = @import("server.zig").Server;
const Runner = @import("runner.zig").Runner;

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    if (comptime builtin.target.os.tag == .windows) {
        const ws = std.os.windows.ws2_32;
        var wsa_data: ws.WSADATA = undefined;
        _ = ws.WSAStartup(0x0202, &wsa_data);
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var runner = try Runner.init(allocator);
    defer runner.deinit();

    try cgi.populateGlobals(runner.interpreter.arena.allocator(), runner.interpreter.globals);

    if (args.len > 1) {
        const cmd = args[1];

        if (std.mem.eql(u8, cmd, "serve")) {
            var port: u16 = 8080;
            if (args.len > 2) {
                const port_str = args[2];
                port = std.fmt.parseInt(u16, port_str, 10) catch 8080;
            }
            var root_dir: []const u8 = ".";
            if (args.len > 3) {
                root_dir = args[3];
            }

            var server = Server.init(allocator, port, root_dir);
            try server.start();
            return;
        }

        const filepath = cmd;

        if (args.len > 2 and std.mem.eql(u8, args[2], "--debug")) {
            runner.debug_mode = true;
        }

        const output = runner.runFile(filepath) catch |err| {
            std.debug.print("{s}", .{runner.interpreter.getOutput()});
            std.debug.print("Runtime Error: {}\n", .{err});
            return;
        };
        std.debug.print("{s}", .{output});
        return;
    }

    std.debug.print(
        \\╔══════════════════════════════════════╗
        \\║       FlipLang Interpreter v0.1      ║
        \\╚══════════════════════════════════════╝
        \\
        \\Usage: flip <file.flip> [--debug]
        \\
        \\Examples:
        \\  flip serve [port] [dir]
        \\  flip examples/hello.flip
        \\
    , .{});
}

test "runner basic" {
    const allocator = std.testing.allocator;
    var runner = try Runner.init(allocator);
    defer runner.deinit();
    const output = try runner.run("ec \"Hello\";");
    try std.testing.expectEqualStrings("Hello\n", output);
}
