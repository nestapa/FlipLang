const std = @import("std");
const Value = @import("value.zig").Value;

pub const Environment = struct {
    values: std.StringHashMapUnmanaged(Value),
    parent: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .values = .empty,
            .parent = null,
            .allocator = allocator,
        };
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, parent: *Environment) Environment {
        return Environment{
            .values = .empty,
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit(self.allocator);
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(self.allocator, name, value);
    }

    pub fn get(self: *Environment, name: []const u8) ?Value {
        if (self.values.get(name)) |value| {
            return value;
        }
        if (self.parent) |parent| {
            return parent.get(name);
        }
        return null;
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !bool {
        if (self.values.contains(name)) {
            try self.values.put(self.allocator, name, value);
            return true;
        }
        if (self.parent) |parent| {
            return parent.assign(name, value);
        }
        return false;
    }

    pub fn contains(self: *Environment, name: []const u8) bool {
        if (self.values.contains(name)) return true;
        if (self.parent) |parent| return parent.contains(name);
        return false;
    }
};

test "environment" {
    const allocator = std.testing.allocator;
    var env = Environment.init(allocator);
    defer env.deinit();
    try env.define("x", Value{ .number = 42 });
    try std.testing.expect(env.get("x") != null);
}
