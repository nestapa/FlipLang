const std = @import("std");

/// Runtime value types for FlipLang interpreter
pub const Value = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    null_val: void,
    array: *Array,
    object: *Object,
    function: *Function,
    closure: *Closure,
    native_fn: *const NativeFn,
    class: *Class,
    instance: *Instance,

    const Self = @This();

    /// Check if value is truthy
    pub fn isTruthy(self: Self) bool {
        return switch (self) {
            .null_val => false,
            .boolean => |b| b,
            .number => |n| n != 0,
            .string => |s| s.len > 0,
            .array => true,
            .object => true,
            .function => true,
            .closure => true,
            .native_fn => true,
            .class => true,
            .instance => true,
        };
    }

    /// Check equality
    pub fn isEqual(self: Self, other: Self) bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);

        if (self_tag != other_tag) return false;

        return switch (self) {
            .number => |n| n == other.number,
            .string => |s| std.mem.eql(u8, s, other.string),
            .boolean => |b| b == other.boolean,
            .null_val => true,
            .array => |a| a == other.array, // Reference equality
            .object => |o| o == other.object, // Reference equality
            .function => |f| f == other.function, // Reference equality
            .closure => |c| c == other.closure, // Reference equality
            .native_fn => |n| n == other.native_fn,
            .class => |c| c == other.class,
            .instance => |i| i == other.instance,
        };
    }

    /// Convert value to string for display
    pub fn toString(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .number => |n| blk: {
                var buf: [64]u8 = undefined;
                const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch break :blk "NaN";
                break :blk allocator.dupe(u8, slice) catch break :blk "NaN";
            },
            .string => |s| allocator.dupe(u8, s) catch "",
            .boolean => |b| allocator.dupe(u8, if (b) "true" else "false") catch "",
            .null_val => allocator.dupe(u8, "null") catch "",
            .array => allocator.dupe(u8, "[Array]") catch "",
            .object => allocator.dupe(u8, "[Object]") catch "",
            .function => allocator.dupe(u8, "[Function]") catch "",
            .closure => allocator.dupe(u8, "[Closure]") catch "",
            .native_fn => allocator.dupe(u8, "[NativeFunction]") catch "",
            .class => |c| allocator.dupe(u8, c.name) catch "[Class]",
            .instance => |i| blk: {
                var buf: [128]u8 = undefined;
                const slice = std.fmt.bufPrint(&buf, "[{s} instance]", .{i.class.name}) catch break :blk "[Instance]";
                break :blk allocator.dupe(u8, slice) catch "[Instance]";
            },
        };
    }

    /// Get type name
    pub fn typeName(self: Self) []const u8 {
        return switch (self) {
            .number => "number",
            .string => "string",
            .boolean => "boolean",
            .null_val => "null",
            .array => "array",
            .object => "object",
            .function => "function",
            .closure => "closure",
            .native_fn => "native_function",
            .class => "class",
            .instance => "instance",
        };
    }
};

/// Array type
pub const Array = struct {
    elements: std.ArrayListUnmanaged(Value),

    pub fn init() Array {
        return Array{ .elements = .empty };
    }

    pub fn deinit(self: *Array, allocator: std.mem.Allocator) void {
        self.elements.deinit(allocator);
    }

    pub fn push(self: *Array, allocator: std.mem.Allocator, value: Value) !void {
        try self.elements.append(allocator, value);
    }

    pub fn pop(self: *Array) ?Value {
        if (self.elements.items.len == 0) return null;
        return self.elements.pop();
    }

    pub fn get(self: *Array, index: usize) ?Value {
        if (index >= self.elements.items.len) return null;
        return self.elements.items[index];
    }

    pub fn set(self: *Array, index: usize, value: Value) bool {
        if (index >= self.elements.items.len) return false;
        self.elements.items[index] = value;
        return true;
    }

    pub fn len(self: *Array) usize {
        return self.elements.items.len;
    }
};

/// Object type (key-value pairs)
pub const Object = struct {
    entries: std.StringHashMapUnmanaged(Value),

    pub fn init() Object {
        return Object{ .entries = .empty };
    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }

    pub fn set(self: *Object, allocator: std.mem.Allocator, key: []const u8, value: Value) !void {
        try self.entries.put(allocator, key, value);
    }

    pub fn get(self: *Object, key: []const u8) ?Value {
        return self.entries.get(key);
    }

    pub fn has(self: *Object, key: []const u8) bool {
        return self.entries.contains(key);
    }
};

/// User-defined function
pub const Function = struct {
    name: []const u8,
    params: []const []const u8,
    body: ?*const anyopaque, // Points to ast.Stmt (null for closures with expr body)
    closure: ?*anyopaque, // Points to Environment
    is_arrow: bool,

    pub fn call(self: *const Function, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
        _ = self;
        _ = args;
        _ = allocator;
        // Implementation will be in interpreter
        return Value{ .null_val = {} };
    }
};

/// Closure (arrow function with captured environment)
pub const Closure = struct {
    params: []const []const u8,
    body_expr: ?*const anyopaque, // Points to ast.Expr for arrow functions
    body_stmt: ?*const anyopaque, // Points to ast.Stmt for block body
    environment: ?*anyopaque, // Captured environment
};

/// Class definition
pub const Class = struct {
    name: []const u8,
    init_method: ?*const anyopaque, // Points to ast.FunctionDecl
    methods: std.StringHashMapUnmanaged(*const anyopaque), // method name -> ast.FunctionDecl
};

/// Instance of a class
pub const Instance = struct {
    class: *Class,
    fields: std.StringHashMapUnmanaged(Value),

    pub fn init(class: *Class) Instance {
        return Instance{
            .class = class,
            .fields = .empty,
        };
    }

    pub fn get(self: *Instance, name: []const u8) ?Value {
        return self.fields.get(name);
    }

    pub fn set(self: *Instance, allocator: std.mem.Allocator, name: []const u8, value: Value) !void {
        try self.fields.put(allocator, name, value);
    }
};

/// Native function signature
pub const NativeFn = fn (context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value;

/// Runtime errors
pub const RuntimeError = error{
    TypeError,
    NameError,
    IndexError,
    DivisionByZero,
    InvalidOperand,
    ArityMismatch,
    OutOfMemory,
    ReturnValue, // Used for return statement control flow
    ThrowError, // Used for try-catch error handling
};

// ============ TESTS ============

test "value - truthiness" {
    const num = Value{ .number = 42 };
    const zero = Value{ .number = 0 };
    const str = Value{ .string = "hello" };
    const empty_str = Value{ .string = "" };
    const t = Value{ .boolean = true };
    const f = Value{ .boolean = false };
    const n = Value{ .null_val = {} };

    try std.testing.expect(num.isTruthy());
    try std.testing.expect(!zero.isTruthy());
    try std.testing.expect(str.isTruthy());
    try std.testing.expect(!empty_str.isTruthy());
    try std.testing.expect(t.isTruthy());
    try std.testing.expect(!f.isTruthy());
    try std.testing.expect(!n.isTruthy());
}

test "value - equality" {
    const a = Value{ .number = 42 };
    const b = Value{ .number = 42 };
    const c = Value{ .number = 10 };
    const s1 = Value{ .string = "hello" };
    const s2 = Value{ .string = "hello" };

    try std.testing.expect(a.isEqual(b));
    try std.testing.expect(!a.isEqual(c));
    try std.testing.expect(s1.isEqual(s2));
    try std.testing.expect(!a.isEqual(s1));
}
