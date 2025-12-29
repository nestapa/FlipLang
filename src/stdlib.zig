const std = @import("std");
const Value = @import("value.zig").Value;
const Array = @import("value.zig").Array;
const RuntimeError = @import("value.zig").RuntimeError;

/// FlipLang Standard Library
/// Built-in functions for common operations

// ============ ARRAY FUNCTIONS ============

/// len(!arr) - Get length of array or string
pub fn builtinLen(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    return switch (args[0]) {
        .array => |arr| Value{ .number = @floatFromInt(arr.elements.items.len) },
        .string => |s| Value{ .number = @floatFromInt(s.len) },
        .object => |obj| Value{ .number = @floatFromInt(obj.entries.count()) },
        else => RuntimeError.TypeError,
    };
}

/// push(!arr, !value) - Add element to end of array
pub fn builtinPush(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 2) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .array => |arr| {
            arr.push(allocator, args[1]) catch return RuntimeError.OutOfMemory;
            return Value{ .number = @floatFromInt(arr.elements.items.len) };
        },
        else => return RuntimeError.TypeError,
    }
}

/// pop(!arr) - Remove and return last element
pub fn builtinPop(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .array => |arr| {
            return arr.pop() orelse Value{ .null_val = {} };
        },
        else => return RuntimeError.TypeError,
    }
}

/// first(!arr) - Get first element
pub fn builtinFirst(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .array => |arr| {
            return arr.get(0) orelse Value{ .null_val = {} };
        },
        .string => |s| {
            if (s.len == 0) return Value{ .null_val = {} };
            return Value{ .string = s[0..1] };
        },
        else => return RuntimeError.TypeError,
    }
}

/// last(!arr) - Get last element
pub fn builtinLast(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .array => |arr| {
            if (arr.elements.items.len == 0) return Value{ .null_val = {} };
            return arr.get(arr.elements.items.len - 1) orelse Value{ .null_val = {} };
        },
        .string => |s| {
            if (s.len == 0) return Value{ .null_val = {} };
            return Value{ .string = s[s.len - 1 .. s.len] };
        },
        else => return RuntimeError.TypeError,
    }
}

// ============ STRING FUNCTIONS ============

/// upper(!str) - Convert to uppercase
pub fn builtinUpper(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .string => |s| {
            const result = allocator.alloc(u8, s.len) catch return RuntimeError.OutOfMemory;
            for (s, 0..) |c, i| {
                result[i] = std.ascii.toUpper(c);
            }
            return Value{ .string = result };
        },
        else => return RuntimeError.TypeError,
    }
}

/// lower(!str) - Convert to lowercase
pub fn builtinLower(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .string => |s| {
            const result = allocator.alloc(u8, s.len) catch return RuntimeError.OutOfMemory;
            for (s, 0..) |c, i| {
                result[i] = std.ascii.toLower(c);
            }
            return Value{ .string = result };
        },
        else => return RuntimeError.TypeError,
    }
}

/// trim(!str) - Remove leading/trailing whitespace
pub fn builtinTrim(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .string => |s| {
            const trimmed = std.mem.trim(u8, s, " \t\n\r");
            const result = allocator.dupe(u8, trimmed) catch return RuntimeError.OutOfMemory;
            return Value{ .string = result };
        },
        else => return RuntimeError.TypeError,
    }
}

// ============ TYPE FUNCTIONS ============

/// type(!val) - Get type name as string
pub fn builtinType(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.ArityMismatch;

    const type_name = args[0].typeName();
    const result = allocator.dupe(u8, type_name) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}

/// isNum(!val) - Check if value is number
pub fn builtinIsNum(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .number };
}

/// isStr(!val) - Check if value is string
pub fn builtinIsStr(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .string };
}

/// isArr(!val) - Check if value is array
pub fn builtinIsArr(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .array };
}

// ============ MATH FUNCTIONS ============

/// abs(!num) - Absolute value
pub fn builtinAbs(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .number => |n| return Value{ .number = @abs(n) },
        else => return RuntimeError.TypeError,
    }
}

/// floor(!num) - Round down
pub fn builtinFloor(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .number => |n| return Value{ .number = @floor(n) },
        else => return RuntimeError.TypeError,
    }
}

/// ceil(!num) - Round up
pub fn builtinCeil(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .number => |n| return Value{ .number = @ceil(n) },
        else => return RuntimeError.TypeError,
    }
}

/// round(!num) - Round to nearest integer
pub fn builtinRound(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    switch (args[0]) {
        .number => |n| return Value{ .number = @round(n) },
        else => return RuntimeError.TypeError,
    }
}

/// min(!a, !b) - Get minimum value
pub fn builtinMin(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;

    if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;
    return Value{ .number = @min(args[0].number, args[1].number) };
}

/// max(!a, !b) - Get maximum value
pub fn builtinMax(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;

    if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;
    return Value{ .number = @max(args[0].number, args[1].number) };
}

// ============ CONVERSION FUNCTIONS ============

/// toNum(!val) - Convert to number
pub fn builtinToNum(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    return switch (args[0]) {
        .number => args[0],
        .string => |s| blk: {
            const num = std.fmt.parseFloat(f64, s) catch break :blk Value{ .null_val = {} };
            break :blk Value{ .number = num };
        },
        .boolean => |b| Value{ .number = if (b) 1 else 0 },
        else => Value{ .null_val = {} },
    };
}

/// toStr(!val) - Convert to string
pub fn builtinToStr(args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    if (args.len != 1) return RuntimeError.ArityMismatch;

    const str = args[0].toString(allocator) catch return RuntimeError.OutOfMemory;
    return Value{ .string = str };
}

// ============ STDLIB REGISTRY ============

pub const BuiltinFn = *const fn ([]Value, std.mem.Allocator) RuntimeError!Value;

pub const builtins = std.StaticStringMap(BuiltinFn).initComptime(.{
    // Array
    .{ "len", builtinLen },
    .{ "push", builtinPush },
    .{ "pop", builtinPop },
    .{ "first", builtinFirst },
    .{ "last", builtinLast },
    // String
    .{ "upper", builtinUpper },
    .{ "lower", builtinLower },
    .{ "trim", builtinTrim },
    // Type
    .{ "type", builtinType },
    .{ "isNum", builtinIsNum },
    .{ "isStr", builtinIsStr },
    .{ "isArr", builtinIsArr },
    // Math
    .{ "abs", builtinAbs },
    .{ "floor", builtinFloor },
    .{ "ceil", builtinCeil },
    .{ "round", builtinRound },
    .{ "min", builtinMin },
    .{ "max", builtinMax },
    // Conversion
    .{ "toNum", builtinToNum },
    .{ "toStr", builtinToStr },
});

/// Get a builtin function by name
pub fn getBuiltin(name: []const u8) ?BuiltinFn {
    return builtins.get(name);
}

// ============ TESTS ============

test "stdlib - len" {
    const allocator = std.testing.allocator;

    // String length
    var str_args = [_]Value{Value{ .string = "hello" }};
    const str_result = try builtinLen(&str_args, allocator);
    try std.testing.expectEqual(@as(f64, 5), str_result.number);
}

test "stdlib - upper/lower" {
    const allocator = std.testing.allocator;

    var upper_args = [_]Value{Value{ .string = "hello" }};
    const upper_result = try builtinUpper(&upper_args, allocator);
    defer allocator.free(upper_result.string);
    try std.testing.expectEqualStrings("HELLO", upper_result.string);

    var lower_args = [_]Value{Value{ .string = "WORLD" }};
    const lower_result = try builtinLower(&lower_args, allocator);
    defer allocator.free(lower_result.string);
    try std.testing.expectEqualStrings("world", lower_result.string);
}

test "stdlib - math functions" {
    const allocator = std.testing.allocator;

    // abs
    var abs_args = [_]Value{Value{ .number = -5 }};
    const abs_result = try builtinAbs(&abs_args, allocator);
    try std.testing.expectEqual(@as(f64, 5), abs_result.number);

    // min/max
    var minmax_args = [_]Value{ Value{ .number = 3 }, Value{ .number = 7 } };
    const min_result = try builtinMin(&minmax_args, allocator);
    const max_result = try builtinMax(&minmax_args, allocator);
    try std.testing.expectEqual(@as(f64, 3), min_result.number);
    try std.testing.expectEqual(@as(f64, 7), max_result.number);
}

test "stdlib - type checks" {
    const allocator = std.testing.allocator;

    var num_args = [_]Value{Value{ .number = 42 }};
    const is_num = try builtinIsNum(&num_args, allocator);
    try std.testing.expect(is_num.boolean);

    var str_args = [_]Value{Value{ .string = "test" }};
    const is_str = try builtinIsStr(&str_args, allocator);
    try std.testing.expect(is_str.boolean);
}

test "stdlib - conversions" {
    const allocator = std.testing.allocator;

    // toNum
    var tonum_args = [_]Value{Value{ .string = "42" }};
    const num_result = try builtinToNum(&tonum_args, allocator);
    try std.testing.expectEqual(@as(f64, 42), num_result.number);

    // toStr
    var tostr_args = [_]Value{Value{ .number = 123 }};
    const str_result = try builtinToStr(&tostr_args, allocator);
    defer allocator.free(str_result.string);
    try std.testing.expectEqualStrings("123", str_result.string);
}
