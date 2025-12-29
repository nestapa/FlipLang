const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;
const Preprocessor = @import("preprocessor.zig").Preprocessor;

pub const Runner = struct {
    allocator: std.mem.Allocator,
    interpreter: Interpreter,
    debug_mode: bool,

    pub fn init(allocator: std.mem.Allocator) !Runner {
        return Runner{
            .allocator = allocator,
            .interpreter = try Interpreter.init(allocator),
            .debug_mode = false,
        };
    }

    pub fn deinit(self: *Runner) void {
        self.interpreter.deinit();
    }

    pub fn run(self: *Runner, source: []const u8) ![]const u8 {
        var lexer = Lexer.init(source);
        const tokens = try lexer.tokenize(self.allocator);
        defer self.allocator.free(tokens);

        if (self.debug_mode) {
            std.debug.print("[DEBUG] Tokens: {}\n", .{tokens.len});
        }

        var parser = Parser.init(tokens, self.allocator);
        var program = try parser.parse();
        defer program.deinit();

        if (self.debug_mode) {
            std.debug.print("[DEBUG] Statements: {}\n", .{program.statements.len});
        }

        try self.interpreter.run(&program);
        return self.interpreter.getOutput();
    }

    pub fn runFile(self: *Runner, filepath: []const u8) ![]const u8 {
        const file = std.fs.cwd().openFile(filepath, .{}) catch |err| {
            std.debug.print("Error opening file '{s}': {}\n", .{ filepath, err });
            return error.FileNotFound;
        };
        defer file.close();

        const stat = file.stat() catch |err| {
            std.debug.print("Error getting file stat: {}\n", .{err});
            return error.ReadError;
        };

        const source = self.allocator.alloc(u8, stat.size) catch |err| {
            std.debug.print("Error allocating: {}\n", .{err});
            return error.OutOfMemory;
        };
        defer self.allocator.free(source);

        const bytes_read = file.pread(source, 0) catch |err| {
            std.debug.print("Error reading file: {}\n", .{err});
            return error.ReadError;
        };
        _ = bytes_read;

        const processed = try Preprocessor.process(self.interpreter.arena.allocator(), source);
        return self.run(processed);
    }
};
