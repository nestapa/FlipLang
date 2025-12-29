const std = @import("std");
const value = @import("value.zig");
const Value = value.Value;
const Object = value.Object;
const Environment = @import("environment.zig").Environment;

pub fn urlDecode(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var output = try std.ArrayListUnmanaged(u8).initCapacity(allocator, input.len);
    errdefer output.deinit(allocator);

    var i: usize = 0;
    while (i < input.len) {
        const c = input[i];
        if (c == '%') {
            if (i + 2 < input.len) {
                const hex = input[i + 1 .. i + 3];
                const char_code = std.fmt.parseInt(u8, hex, 16) catch {
                    try output.append(allocator, '%');
                    i += 1;
                    continue;
                };
                try output.append(allocator, char_code);
                i += 3;
            } else {
                try output.append(allocator, '%');
                i += 1;
            }
        } else if (c == '+') {
            try output.append(allocator, ' ');
            i += 1;
        } else {
            try output.append(allocator, c);
            i += 1;
        }
    }

    return output.toOwnedSlice(allocator);
}

pub fn parseQueryString(allocator: std.mem.Allocator, query: []const u8) !Value {
    const obj_ptr = try allocator.create(Object);
    errdefer allocator.destroy(obj_ptr);
    obj_ptr.* = Object.init();

    if (query.len == 0) return Value{ .object = obj_ptr };

    var it = std.mem.splitSequence(u8, query, "&");
    while (it.next()) |pair| {
        if (pair.len == 0) continue;

        var key_val = std.mem.splitSequence(u8, pair, "=");
        const raw_key = key_val.first();
        const raw_val = key_val.rest();

        const key = try urlDecode(allocator, raw_key);

        var val_str: []const u8 = "";
        if (raw_val.len > 0) {
            val_str = try urlDecode(allocator, raw_val);
        } else {
            val_str = try allocator.dupe(u8, "");
        }

        const val_obj = Value{ .string = val_str };
        try obj_ptr.set(allocator, key, val_obj);
    }

    return Value{ .object = obj_ptr };
}

pub fn populateGlobals(allocator: std.mem.Allocator, globals: *Environment) !void {
    const gateway = std.process.getEnvVarOwned(allocator, "GATEWAY_INTERFACE") catch |err| switch (err) {
        error.EnvironmentVariableNotFound => return,
        else => return err,
    };
    defer allocator.free(gateway);

    var server_obj = Object.init();

    const env_vars = [_][]const u8{ "GATEWAY_INTERFACE", "SERVER_PROTOCOL", "REQUEST_METHOD", "QUERY_STRING", "CONTENT_LENGTH", "CONTENT_TYPE", "SCRIPT_NAME", "REMOTE_ADDR", "HTTP_USER_AGENT" };

    for (env_vars) |key| {
        if (std.process.getEnvVarOwned(allocator, key) catch null) |val| {
            defer allocator.free(val);
            try server_obj.set(allocator, key, Value{ .string = try allocator.dupe(u8, val) });
        }
    }

    try globals.define("!_SERVER", Value{ .object = try createObject(allocator, server_obj) });

    var get_val = Value{ .object = try createObject(allocator, Object.init()) };
    if (std.process.getEnvVarOwned(allocator, "QUERY_STRING") catch null) |qs| {
        defer allocator.free(qs);
        get_val = try parseQueryString(allocator, qs);
    }
    try globals.define("!_GET", get_val);

    var post_val = Value{ .object = try createObject(allocator, Object.init()) };
    const method = std.process.getEnvVarOwned(allocator, "REQUEST_METHOD") catch null;
    if (method) |m| {
        defer allocator.free(m);
        if (std.mem.eql(u8, m, "POST")) {
            const cl_val = std.process.getEnvVarOwned(allocator, "CONTENT_LENGTH") catch null;
            var cl: usize = 0;
            if (cl_val) |s| {
                defer allocator.free(s);
                cl = std.fmt.parseInt(usize, s, 10) catch 0;
            }

            if (cl > 0) {
                const body = try allocator.alloc(u8, cl);
                defer allocator.free(body);
                const stdin = std.fs.File.stdin();
                const bytes_read = try stdin.read(body);
                post_val = try parseQueryString(allocator, body[0..bytes_read]);
            }
        }
    }
    try globals.define("!_POST", post_val);
}

fn createObject(allocator: std.mem.Allocator, obj: Object) !*Object {
    const ptr = try allocator.create(Object);
    ptr.* = obj;
    return ptr;
}

test "url decode" {
    const allocator = std.testing.allocator;
    const decoded = try urlDecode(allocator, "Hello%20World");
    defer allocator.free(decoded);
    try std.testing.expectEqualStrings("Hello World", decoded);
}
