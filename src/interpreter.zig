const std = @import("std");
const ast = @import("ast.zig");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Array = value_mod.Array;
const Object = value_mod.Object;
const Function = value_mod.Function;
const Closure = value_mod.Closure;
const Class = value_mod.Class;
const Instance = value_mod.Instance;
const RuntimeError = value_mod.RuntimeError;
const environment = @import("environment.zig");
const Environment = environment.Environment;
const MySQL = @import("mysql.zig").MySQL;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

/// FlipLang Interpreter
/// Executes AST nodes and produces results
pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    environment: *Environment,
    globals: *Environment,
    output: std.ArrayListUnmanaged(u8),
    return_value: ?Value = null,
    had_return: bool = false,
    had_break: bool = false,
    had_continue: bool = false,
    current_instance: ?*value_mod.Instance = null,
    mysql_connections: std.AutoHashMap(i32, i64),
    next_mysql_id: i32,

    /// Initialize a new interpreter with allocator
    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const globals = try allocator.create(Environment);
        globals.* = Environment.init(allocator);

        // Register native functions
        try globals.define("read_file", Value{ .native_fn = &nativeReadFile });
        try globals.define("write_file", Value{ .native_fn = &nativeWriteFile });
        try globals.define("append_file", Value{ .native_fn = &nativeAppendFile });
        try globals.define("file_exists", Value{ .native_fn = &nativeFileExists });
        try globals.define("len", Value{ .native_fn = &nativeLen });
        try globals.define("type", Value{ .native_fn = &nativeType });
        try globals.define("str", Value{ .native_fn = &nativeStr });
        try globals.define("num", Value{ .native_fn = &nativeNum });
        try globals.define("get_env", Value{ .native_fn = &nativeGetEnv });
        try globals.define("split", Value{ .native_fn = &nativeSplit });
        try globals.define("join", Value{ .native_fn = &nativeJoin });
        try globals.define("replace", Value{ .native_fn = &nativeReplace });
        try globals.define("find", Value{ .native_fn = &nativeFind });
        try globals.define("print", Value{ .native_fn = &nativePrint });
        try globals.define("ec", Value{ .native_fn = &nativePrint });
        try globals.define("println", Value{ .native_fn = &nativePrintLine });
        try globals.define("render", Value{ .native_fn = &nativeRender });
        try globals.define("push", Value{ .native_fn = &nativePush });
        try globals.define("split_params", Value{ .native_fn = &nativeSplitParams });
        try globals.define("mysql_connect", Value{ .native_fn = &nativeMySQLConnect });
        try globals.define("mysql_query", Value{ .native_fn = &nativeMySQLQuery });
        try globals.define("mysql_close", Value{ .native_fn = &nativeMySQLClose });

        // String functions
        try globals.define("upper", Value{ .native_fn = &nativeUpper });
        try globals.define("lower", Value{ .native_fn = &nativeLower });
        try globals.define("trim", Value{ .native_fn = &nativeTrim });
        try globals.define("substr", Value{ .native_fn = &nativeSubstr });
        try globals.define("contains", Value{ .native_fn = &nativeContains });

        // Math functions
        try globals.define("abs", Value{ .native_fn = &nativeAbs });
        try globals.define("floor", Value{ .native_fn = &nativeFloor });
        try globals.define("ceil", Value{ .native_fn = &nativeCeil });
        try globals.define("round", Value{ .native_fn = &nativeRound });
        try globals.define("min", Value{ .native_fn = &nativeMin });
        try globals.define("max", Value{ .native_fn = &nativeMax });
        try globals.define("sqrt", Value{ .native_fn = &nativeSqrt });
        try globals.define("pow", Value{ .native_fn = &nativePow });
        try globals.define("random", Value{ .native_fn = &nativeRandom });

        // Array functions
        try globals.define("first", Value{ .native_fn = &nativeFirst });
        try globals.define("last", Value{ .native_fn = &nativeLast });
        try globals.define("pop", Value{ .native_fn = &nativePop });
        try globals.define("keys", Value{ .native_fn = &nativeKeys });
        try globals.define("values", Value{ .native_fn = &nativeValues });

        // Type functions
        try globals.define("isNum", Value{ .native_fn = &nativeIsNum });
        try globals.define("isStr", Value{ .native_fn = &nativeIsStr });
        try globals.define("isArr", Value{ .native_fn = &nativeIsArr });
        try globals.define("isObj", Value{ .native_fn = &nativeIsObj });
        try globals.define("isNull", Value{ .native_fn = &nativeIsNull });
        try globals.define("isBool", Value{ .native_fn = &nativeIsBool });

        // JSON functions
        try globals.define("json_encode", Value{ .native_fn = &nativeJsonEncode });
        try globals.define("json_decode", Value{ .native_fn = &nativeJsonDecode });

        // Date/Time functions
        try globals.define("time", Value{ .native_fn = &nativeTime });
        try globals.define("date", Value{ .native_fn = &nativeDate });

        return Interpreter{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .environment = globals, // Global scope is the root environment
            .globals = globals,
            .output = .empty,
            .mysql_connections = std.AutoHashMap(i32, i64).init(allocator),
            .next_mysql_id = 1,
        };
    }

    /// Deinitialize the interpreter
    pub fn deinit(self: *Interpreter) void {
        var it = self.mysql_connections.iterator();
        while (it.next()) |entry| {
            const fd: std.posix.socket_t = if (@typeInfo(std.posix.socket_t) == .int) @as(std.posix.socket_t, @intCast(entry.value_ptr.*)) else @as(std.posix.socket_t, @ptrFromInt(@as(usize, @intCast(entry.value_ptr.*))));
            std.posix.close(fd);
        }
        self.mysql_connections.deinit();
        self.globals.deinit();
        self.allocator.destroy(self.globals);
        self.output.deinit(self.allocator);
        self.arena.deinit();
    }

    /// Run source code string
    pub fn runSource(self: *Interpreter, source: []const u8) !void {
        var lexer = Lexer.init(source);
        const tokens = lexer.tokenize(self.allocator) catch |err| {
            std.debug.print("Lexer error: {}\n", .{err});
            return err;
        };
        defer self.allocator.free(tokens);

        var parser = Parser.init(tokens, self.allocator);
        var program = parser.parse() catch |err| {
            std.debug.print("Parser error: {}\n", .{err});
            return err;
        };
        defer program.deinit();

        try self.run(&program);
    }

    /// Get accumulated output
    pub fn getOutput(self: *Interpreter) []const u8 {
        return self.output.items;
    }

    /// Run a program
    pub fn run(self: *Interpreter, program: *ast.Program) !void {
        for (program.statements) |stmt| {
            try self.executeStmt(stmt);
        }
    }

    fn runtimeAllocator(self: *Interpreter) std.mem.Allocator {
        return self.arena.allocator();
    }

    // ============ STATEMENT EXECUTION ============

    pub const ExecError = RuntimeError || error{OutOfMemory};

    fn executeStmt(self: *Interpreter, stmt: ast.Stmt) ExecError!void {
        switch (stmt) {
            .echo_stmt => |e| try self.executeEcho(e),
            .var_decl => |v| try self.executeVarDecl(v),
            .if_stmt => |i| try self.executeIf(i),
            .while_stmt => |w| try self.executeWhile(w),
            .for_stmt => |f| try self.executeFor(f),
            .foreach_stmt => |fe| try self.executeForEach(fe),
            .block => |b| try self.executeBlock(b),
            .expr_stmt => |e| {
                _ = try self.evaluate(e.expr);
            },
            .function_decl => |fd| try self.executeFunctionDecl(fd),
            .return_stmt => |rs| try self.executeReturn(rs),
            .break_stmt => |_| self.had_break = true,
            .continue_stmt => |_| self.had_continue = true,
            .switch_stmt => |s| try self.executeSwitch(s),
            .class_decl => |cd| try self.executeClassDecl(cd),
            .use_stmt => |u| try self.executeUse(u),
        }
    }

    fn executeSwitch(self: *Interpreter, switch_stmt: *ast.SwitchStmt) !void {
        const value = try self.evaluate(switch_stmt.expr);

        for (switch_stmt.cases) |case| {
            const case_val = try self.evaluate(case.value);
            if (value.isEqual(case_val)) {
                for (case.body) |stmt| {
                    try self.executeStmt(stmt);
                    if (self.had_return) return;
                }
                return;
            }
        }

        if (switch_stmt.default) |default_stmts| {
            for (default_stmts) |stmt| {
                try self.executeStmt(stmt);
                if (self.had_return) return;
            }
        }
    }

    fn executeEcho(self: *Interpreter, echo: *ast.EchoStmt) !void {
        const value = try self.evaluate(echo.expr);
        const str = try value.toString(self.runtimeAllocator());
        // Since we are using an arena for runtime, we don't strictly need to free it here,
        // but it's good practice for when we might switch back to a general allocator.
        // However, with Arena, free() is a no-op.

        self.output.appendSlice(self.allocator, str) catch return RuntimeError.OutOfMemory;
        self.output.append(self.allocator, '\n') catch return RuntimeError.OutOfMemory;
    }

    fn executeVarDecl(self: *Interpreter, decl: *ast.VarDecl) !void {
        var value = Value{ .null_val = {} };
        if (decl.initializer) |initializer_expr| {
            value = try self.evaluate(initializer_expr);
        }
        try self.environment.define(decl.name, value);
    }

    fn executeIf(self: *Interpreter, if_stmt: *ast.IfStmt) !void {
        const condition = try self.evaluate(if_stmt.condition);

        if (condition.isTruthy()) {
            try self.executeStmt(if_stmt.then_branch);
        } else if (if_stmt.else_branch) |else_branch| {
            try self.executeStmt(else_branch);
        }
    }

    fn executeWhile(self: *Interpreter, while_stmt: *ast.WhileStmt) !void {
        while (true) {
            const condition = try self.evaluate(while_stmt.condition);
            if (!condition.isTruthy()) break;

            try self.executeStmt(while_stmt.body);

            if (self.had_break) {
                self.had_break = false;
                break;
            }
            if (self.had_continue) {
                self.had_continue = false;
                continue;
            }
            if (self.had_return) break;
        }
    }

    fn executeFor(self: *Interpreter, for_stmt: *ast.ForStmt) !void {
        // Execute initializer
        if (for_stmt.initializer) |init_stmt| {
            try self.executeStmt(init_stmt);
        }

        while (true) {
            // Check condition
            if (for_stmt.condition) |cond| {
                const result = try self.evaluate(cond);
                if (!result.isTruthy()) break;
            }

            // Execute body
            try self.executeStmt(for_stmt.body);

            if (self.had_break) {
                self.had_break = false;
                break;
            }
            if (self.had_return) break;

            // Continue logic: just proceed to increment
            if (self.had_continue) {
                self.had_continue = false;
            }

            // Execute increment
            if (for_stmt.increment) |inc| {
                _ = try self.evaluate(inc);
            }
        }
    }

    fn executeForEach(self: *Interpreter, foreach: *ast.ForEachStmt) !void {
        const iterable = try self.evaluate(foreach.iterable);

        switch (iterable) {
            .array => |arr| {
                for (arr.elements.items) |item| {
                    try self.environment.define(foreach.item_name, item);
                    try self.executeStmt(foreach.body);

                    if (self.had_break) {
                        self.had_break = false;
                        break;
                    }
                    if (self.had_continue) {
                        self.had_continue = false;
                        continue;
                    }
                    if (self.had_return) break;
                }
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn executeBlock(self: *Interpreter, block: *ast.Block) !void {
        // Create new scope
        var local = Environment.initEnclosed(self.allocator, self.environment);
        defer local.deinit();

        const previous = self.environment;
        self.environment = &local;
        defer self.environment = previous;

        for (block.statements) |stmt| {
            try self.executeStmt(stmt);
            if (self.had_return or self.had_break or self.had_continue) break;
        }
    }

    fn executeFunctionDecl(self: *Interpreter, func_decl: *ast.FunctionDecl) !void {
        // Create a function value and store it in the environment
        const func = self.runtimeAllocator().create(Function) catch return RuntimeError.OutOfMemory;
        func.* = Function{
            .name = func_decl.name,
            .params = func_decl.params,
            .body = @ptrCast(&func_decl.body),
            .closure = @ptrCast(self.environment),
            .is_arrow = false,
        };
        try self.environment.define(func_decl.name, Value{ .function = func });
    }

    fn executeReturn(self: *Interpreter, return_stmt: *ast.ReturnStmt) !void {
        if (return_stmt.value) |val_expr| {
            self.return_value = try self.evaluate(val_expr);
        } else {
            self.return_value = Value{ .null_val = {} };
        }
        self.had_return = true;
    }

    // ============ EXPRESSION EVALUATION ============

    fn evaluate(self: *Interpreter, expr: ast.Expr) RuntimeError!Value {
        return switch (expr) {
            .literal => |lit| self.evalLiteral(lit),
            .variable => |v| self.evalVariable(v),
            .binary => |b| try self.evalBinary(b),
            .unary => |u| try self.evalUnary(u),
            .grouping => |g| try self.evaluate(g.expr),
            .ternary => |t| try self.evalTernary(t),
            .array_literal => |a| try self.evalArrayLiteral(a),
            .object_literal => |o| try self.evalObjectLiteral(o),
            .index => |i| try self.evalIndex(i),
            .member => |m| try self.evalMember(m),
            .assignment => |assign| try self.evalAssignment(assign),
            .call => |c| try self.evalCall(c),
            .arrow_fn => |af| try self.evalArrowFn(af),
            .new_expr => |n| try self.evalNewExpr(n),
            .this_expr => self.evalThisExpr(),
            .member_assignment => |ma| try self.evalMemberAssignment(ma),
        };
    }

    fn evalLiteral(self: *Interpreter, lit: ast.Literal) Value {
        _ = self;
        return switch (lit) {
            .number => |n| Value{ .number = n },
            .string => |s| Value{ .string = s },
            .boolean => |b| Value{ .boolean = b },
            .null_val => Value{ .null_val = {} },
        };
    }

    fn evalVariable(self: *Interpreter, variable: ast.Variable) Value {
        return self.environment.get(variable.name) orelse Value{ .null_val = {} };
    }

    fn evalBinary(self: *Interpreter, binary: *ast.Binary) RuntimeError!Value {
        const left = try self.evaluate(binary.left);
        const right = try self.evaluate(binary.right);

        return switch (binary.operator) {
            // Arithmetic
            .plus => self.evalPlus(left, right),
            .minus => self.evalMinus(left, right),
            .multiply => self.evalMultiply(left, right),
            .divide => self.evalDivide(left, right),
            .modulo => self.evalModulo(left, right),

            // Comparison
            .equal => Value{ .boolean = left.isEqual(right) },
            .not_equal => Value{ .boolean = !left.isEqual(right) },
            .less => self.evalComparison(left, right, .less),
            .less_equal => self.evalComparison(left, right, .less_equal),
            .greater => self.evalComparison(left, right, .greater),
            .greater_equal => self.evalComparison(left, right, .greater_equal),

            // Logical
            .and_op => Value{ .boolean = left.isTruthy() and right.isTruthy() },
            .or_op => Value{ .boolean = left.isTruthy() or right.isTruthy() },

            else => return RuntimeError.InvalidOperand,
        };
    }

    fn evalPlus(self: *Interpreter, left: Value, right: Value) RuntimeError!Value {
        // Number + Number
        if (left == .number and right == .number) {
            return Value{ .number = left.number + right.number };
        }

        // String concatenation
        if (left == .string or right == .string) {
            const left_str = try left.toString(self.runtimeAllocator());
            const right_str = try right.toString(self.runtimeAllocator());

            const result = self.runtimeAllocator().alloc(u8, left_str.len + right_str.len) catch return RuntimeError.OutOfMemory;
            @memcpy(result[0..left_str.len], left_str);
            @memcpy(result[left_str.len..], right_str);

            return Value{ .string = result };
        }

        return RuntimeError.TypeError;
    }

    fn evalMinus(self: *Interpreter, left: Value, right: Value) RuntimeError!Value {
        _ = self;
        if (left == .number and right == .number) {
            return Value{ .number = left.number - right.number };
        }
        return RuntimeError.TypeError;
    }

    fn evalMultiply(self: *Interpreter, left: Value, right: Value) RuntimeError!Value {
        _ = self;
        if (left == .number and right == .number) {
            return Value{ .number = left.number * right.number };
        }
        return RuntimeError.TypeError;
    }

    fn evalDivide(self: *Interpreter, left: Value, right: Value) RuntimeError!Value {
        _ = self;
        if (left == .number and right == .number) {
            if (right.number == 0) return RuntimeError.DivisionByZero;
            return Value{ .number = left.number / right.number };
        }
        return RuntimeError.TypeError;
    }

    fn evalModulo(self: *Interpreter, left: Value, right: Value) RuntimeError!Value {
        _ = self;
        if (left == .number and right == .number) {
            if (right.number == 0) return RuntimeError.DivisionByZero;
            return Value{ .number = @mod(left.number, right.number) };
        }
        return RuntimeError.TypeError;
    }

    fn evalComparison(self: *Interpreter, left: Value, right: Value, op: ast.Operator) RuntimeError!Value {
        _ = self;
        if (left != .number or right != .number) {
            return RuntimeError.TypeError;
        }

        const result = switch (op) {
            .less => left.number < right.number,
            .less_equal => left.number <= right.number,
            .greater => left.number > right.number,
            .greater_equal => left.number >= right.number,
            else => false,
        };

        return Value{ .boolean = result };
    }

    fn evalUnary(self: *Interpreter, unary: *ast.Unary) RuntimeError!Value {
        const operand = try self.evaluate(unary.operand);

        return switch (unary.operator) {
            .not => Value{ .boolean = !operand.isTruthy() },
            .minus => blk: {
                if (operand != .number) return RuntimeError.TypeError;
                break :blk Value{ .number = -operand.number };
            },
            .increment => blk: {
                if (operand != .number) return RuntimeError.TypeError;
                const new_value = Value{ .number = operand.number + 1 };
                // Update the variable if the operand is a variable
                switch (unary.operand) {
                    .variable => |v| {
                        if (self.environment.contains(v.name)) {
                            _ = self.environment.assign(v.name, new_value) catch return RuntimeError.NameError;
                        } else {
                            self.environment.define(v.name, new_value) catch return RuntimeError.OutOfMemory;
                        }
                    },
                    else => {},
                }
                break :blk new_value;
            },
            .decrement => blk: {
                if (operand != .number) return RuntimeError.TypeError;
                const new_value = Value{ .number = operand.number - 1 };
                // Update the variable if the operand is a variable
                switch (unary.operand) {
                    .variable => |v| {
                        if (self.environment.contains(v.name)) {
                            _ = self.environment.assign(v.name, new_value) catch return RuntimeError.NameError;
                        } else {
                            self.environment.define(v.name, new_value) catch return RuntimeError.OutOfMemory;
                        }
                    },
                    else => {},
                }
                break :blk new_value;
            },
            else => return RuntimeError.InvalidOperand,
        };
    }

    fn evalTernary(self: *Interpreter, ternary: *ast.Ternary) RuntimeError!Value {
        const condition = try self.evaluate(ternary.condition);
        if (condition.isTruthy()) {
            return self.evaluate(ternary.then_branch);
        } else {
            return self.evaluate(ternary.else_branch);
        }
    }

    fn evalArrayLiteral(self: *Interpreter, arr: *ast.ArrayLiteral) RuntimeError!Value {
        const array = self.runtimeAllocator().create(Array) catch return RuntimeError.OutOfMemory;
        array.* = Array.init();

        for (arr.elements) |elem| {
            const value = try self.evaluate(elem);
            array.push(self.runtimeAllocator(), value) catch return RuntimeError.OutOfMemory;
        }

        return Value{ .array = array };
    }

    fn evalObjectLiteral(self: *Interpreter, obj: *ast.ObjectLiteral) RuntimeError!Value {
        const object = self.runtimeAllocator().create(Object) catch return RuntimeError.OutOfMemory;
        object.* = Object.init();

        for (obj.keys, 0..) |key, i| {
            const value = try self.evaluate(obj.values[i]);
            object.set(self.runtimeAllocator(), key, value) catch return RuntimeError.OutOfMemory;
        }

        return Value{ .object = object };
    }

    fn evalIndex(self: *Interpreter, index: *ast.Index) RuntimeError!Value {
        const object = try self.evaluate(index.object);
        const idx = try self.evaluate(index.index_expr);

        switch (object) {
            .array => |arr| {
                if (idx != .number) return RuntimeError.TypeError;
                const i: usize = @intFromFloat(idx.number);
                return arr.get(i) orelse Value{ .null_val = {} };
            },
            .string => |s| {
                if (idx != .number) return RuntimeError.TypeError;
                const i: usize = @intFromFloat(idx.number);
                if (i >= s.len) return Value{ .null_val = {} };
                const char = self.runtimeAllocator().alloc(u8, 1) catch return RuntimeError.OutOfMemory;
                char[0] = s[i];
                return Value{ .string = char };
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn evalMember(self: *Interpreter, member: *ast.Member) RuntimeError!Value {
        const object = try self.evaluate(member.object);

        switch (object) {
            .object => |obj| {
                return obj.get(member.name) orelse Value{ .null_val = {} };
            },
            .instance => |inst| {
                // First check fields
                if (inst.get(member.name)) |field_val| {
                    return field_val;
                }
                // Then check methods (return bound method)
                if (inst.class.methods.get(member.name)) |method_ptr| {
                    // Create a bound method (function with instance)
                    const method: *const ast.FunctionDecl = @ptrCast(@alignCast(method_ptr));
                    const func = self.runtimeAllocator().create(Function) catch return RuntimeError.OutOfMemory;
                    func.* = Function{
                        .name = method.name,
                        .params = method.params,
                        .body = @ptrCast(&method.body),
                        .closure = @ptrCast(self.environment),
                        .is_arrow = false,
                    };
                    return Value{ .function = func };
                }
                return Value{ .null_val = {} };
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn evalAssignment(self: *Interpreter, assign: *ast.Assignment) RuntimeError!Value {
        const value = try self.evaluate(assign.value);

        // Try to assign existing variable, if not found, define it
        if (self.environment.contains(assign.name)) {
            _ = self.environment.assign(assign.name, value) catch return RuntimeError.NameError;
        } else {
            self.environment.define(assign.name, value) catch return RuntimeError.OutOfMemory;
        }

        return value;
    }

    fn evalCall(self: *Interpreter, call: *ast.Call) RuntimeError!Value {
        // Check if this is a method call on an instance
        var bound_instance: ?*Instance = null;

        switch (call.callee) {
            .member => |m| {
                const obj = try self.evaluate(m.object);
                if (obj == .instance) {
                    bound_instance = obj.instance;
                }
            },
            else => {},
        }

        const callee = try self.evaluate(call.callee);

        // Evaluate arguments
        var args = std.ArrayListUnmanaged(Value).empty;
        defer args.deinit(self.runtimeAllocator());

        for (call.args) |arg| {
            const val = try self.evaluate(arg);
            args.append(self.runtimeAllocator(), val) catch return RuntimeError.OutOfMemory;
        }

        // Handle function call
        return self.callValue(callee, args.items, bound_instance);
    }

    pub fn callValue(self: *Interpreter, callee: Value, args: []const Value, bound_instance: ?*Instance) RuntimeError!Value {
        switch (callee) {
            .function => |func| {
                // Check arity
                if (args.len != func.params.len) {
                    return RuntimeError.ArityMismatch;
                }

                // Create new environment for function scope
                const closure_env: *Environment = @ptrCast(@alignCast(func.closure));
                var local = Environment.initEnclosed(self.runtimeAllocator(), closure_env);
                defer local.deinit();

                // Bind arguments to parameters
                for (func.params, 0..) |param, i| {
                    local.define(param, args[i]) catch return RuntimeError.OutOfMemory;
                }

                // Save current state
                const previous_env = self.environment;
                const previous_return = self.return_value;
                const previous_had_return = self.had_return;
                const previous_instance = self.current_instance;

                // Set up function execution context
                self.environment = &local;
                self.return_value = null;
                self.had_return = false;

                // Bind 'this' if this is a method call
                if (bound_instance) |inst| {
                    self.current_instance = inst;
                }

                defer {
                    self.environment = previous_env;
                    self.had_return = previous_had_return;
                    self.current_instance = previous_instance;
                }

                // Execute function body
                const body: *const ast.Stmt = @ptrCast(@alignCast(func.body));
                self.executeStmt(body.*) catch |err| {
                    self.return_value = previous_return;
                    return err;
                };

                // Get return value
                const result = self.return_value orelse Value{ .null_val = {} };
                self.return_value = previous_return;
                return result;
            },
            .closure => |closure| {
                // Check arity
                if (args.len != closure.params.len) {
                    return RuntimeError.ArityMismatch;
                }

                // Create new environment
                const closure_env: ?*Environment = if (closure.environment) |env|
                    @ptrCast(@alignCast(env))
                else
                    null;

                var local = if (closure_env) |env|
                    Environment.initEnclosed(self.runtimeAllocator(), env)
                else
                    Environment.init(self.runtimeAllocator());
                defer local.deinit();

                // Bind arguments
                for (closure.params, 0..) |param, i| {
                    local.define(param, args[i]) catch return RuntimeError.OutOfMemory;
                }

                // Save and set environment
                const previous_env = self.environment;
                self.environment = &local;
                defer self.environment = previous_env;

                // Evaluate body expression
                if (closure.body_expr) |body_ptr| {
                    const body: *const ast.Expr = @ptrCast(@alignCast(body_ptr));
                    return self.evaluate(body.*);
                }

                return Value{ .null_val = {} };
            },
            .native_fn => |native| {
                return native(self, @constCast(args), self.runtimeAllocator());
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn evalArrowFn(self: *Interpreter, arrow: *ast.ArrowFn) RuntimeError!Value {
        // Create a closure that captures the current environment
        const closure = self.runtimeAllocator().create(Closure) catch return RuntimeError.OutOfMemory;
        closure.* = Closure{
            .params = arrow.params,
            .body_expr = @ptrCast(&arrow.body),
            .body_stmt = null,
            .environment = @ptrCast(self.environment),
        };
        return Value{ .closure = closure };
    }

    fn executeClassDecl(self: *Interpreter, class_decl: *ast.ClassDecl) !void {
        // Create a Class value
        const class = self.runtimeAllocator().create(Class) catch return RuntimeError.OutOfMemory;
        class.* = Class{
            .name = class_decl.name,
            .init_method = if (class_decl.init_method) |init_decl| @ptrCast(init_decl) else null,
            .methods = .empty,
        };

        // Add methods to class
        for (class_decl.methods) |method| {
            class.methods.put(self.runtimeAllocator(), method.name, @ptrCast(method)) catch return RuntimeError.OutOfMemory;
        }

        // Define class in environment
        try self.environment.define(class_decl.name, Value{ .class = class });
    }

    fn evalNewExpr(self: *Interpreter, new_expr: *ast.NewExpr) RuntimeError!Value {
        // Look up the class
        const class_val = self.environment.get(new_expr.class_name) orelse return RuntimeError.NameError;

        if (class_val != .class) {
            return RuntimeError.TypeError;
        }

        const class = class_val.class;

        // Create a new instance
        const instance = self.runtimeAllocator().create(Instance) catch return RuntimeError.OutOfMemory;
        instance.* = Instance.init(class);

        // Evaluate arguments
        var args = std.ArrayListUnmanaged(Value).empty;
        defer args.deinit(self.runtimeAllocator());

        for (new_expr.args) |arg| {
            const val = try self.evaluate(arg);
            args.append(self.runtimeAllocator(), val) catch return RuntimeError.OutOfMemory;
        }

        // Call init method if exists
        if (class.init_method) |init_ptr| {
            const init_method: *const ast.FunctionDecl = @ptrCast(@alignCast(init_ptr));

            // Check arity
            if (args.items.len != init_method.params.len) {
                return RuntimeError.ArityMismatch;
            }

            // Create environment for init
            var local = Environment.initEnclosed(self.runtimeAllocator(), self.environment);
            defer local.deinit();

            // Bind arguments
            for (init_method.params, 0..) |param, i| {
                local.define(param, args.items[i]) catch return RuntimeError.OutOfMemory;
            }

            // Save state
            const previous_env = self.environment;
            const previous_instance = self.current_instance;

            self.environment = &local;
            self.current_instance = instance;

            defer {
                self.environment = previous_env;
                self.current_instance = previous_instance;
            }

            // Execute init body
            const body: *const ast.Stmt = @ptrCast(@alignCast(&init_method.body));
            try self.executeStmt(body.*);
        }

        return Value{ .instance = instance };
    }

    fn evalThisExpr(self: *Interpreter) Value {
        if (self.current_instance) |inst| {
            return Value{ .instance = inst };
        }
        return Value{ .null_val = {} };
    }

    fn evalMemberAssignment(self: *Interpreter, member_assign: *ast.MemberAssignment) RuntimeError!Value {
        const object = try self.evaluate(member_assign.object);
        const value = try self.evaluate(member_assign.value);

        switch (object) {
            .instance => |inst| {
                inst.set(self.runtimeAllocator(), member_assign.name, value) catch return RuntimeError.OutOfMemory;
                return value;
            },
            .object => |obj| {
                obj.set(self.runtimeAllocator(), member_assign.name, value) catch return RuntimeError.OutOfMemory;
                return value;
            },
            else => return RuntimeError.TypeError,
        }
    }

    fn executeUse(self: *Interpreter, use_stmt: *ast.UseStmt) !void {
        // Read the file
        const file = std.fs.cwd().openFile(use_stmt.path, .{}) catch {
            std.debug.print("Error: Cannot open file '{s}'\n", .{use_stmt.path});
            return RuntimeError.NameError;
        };
        defer file.close();

        // Get file size
        const stat = file.stat() catch {
            std.debug.print("Error: Cannot stat file '{s}'\n", .{use_stmt.path});
            return RuntimeError.OutOfMemory;
        };

        // Allocate buffer (note: intentionally not freeing - lexemes reference this)
        const source = self.allocator.alloc(u8, @intCast(stat.size)) catch {
            std.debug.print("Error: Cannot allocate memory for '{s}'\n", .{use_stmt.path});
            return RuntimeError.OutOfMemory;
        };
        // Note: source is NOT freed as token lexemes point into it

        // Read file
        _ = file.pread(source, 0) catch {
            std.debug.print("Error: Cannot read file '{s}'\n", .{use_stmt.path});
            return RuntimeError.OutOfMemory;
        };

        // Tokenize
        var lexer = Lexer.init(source);
        const tokens = lexer.tokenize(self.allocator) catch return RuntimeError.OutOfMemory;
        // Note: tokens are NOT freed as parser references them

        // Parse
        var parser = Parser.init(tokens, self.allocator);
        var program = parser.parse() catch {
            std.debug.print("Error: Parse error in '{s}'\n", .{use_stmt.path});
            return RuntimeError.TypeError;
        };
        // Note: program is NOT freed as interpreter uses it

        // Execute all statements from the imported file
        for (program.statements) |stmt| {
            try self.executeStmt(stmt);
            if (self.had_return) break;
        }
    }
};

// ============ NATIVE FUNCTIONS ============

fn nativeReadFile(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const path = args[0].string;
    const file = std.fs.cwd().openFile(path, .{}) catch {
        return Value{ .null_val = {} };
    };
    defer file.close();

    const stat = file.stat() catch return RuntimeError.OutOfMemory;
    const content = allocator.alloc(u8, @intCast(stat.size)) catch return RuntimeError.OutOfMemory;
    _ = file.pread(content, 0) catch {
        allocator.free(content);
        return Value{ .null_val = {} };
    };

    return Value{ .string = content };
}

fn nativeWriteFile(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;

    const path = args[0].string;
    const content = args[1].string;

    const file = std.fs.cwd().createFile(path, .{}) catch {
        return Value{ .boolean = false };
    };
    defer file.close();

    _ = file.pwrite(content, 0) catch {
        return Value{ .boolean = false };
    };

    return Value{ .boolean = true };
}

fn nativeAppendFile(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;

    const path = args[0].string;
    const content = args[1].string;

    const file = std.fs.cwd().openFile(path, .{ .mode = .write_only }) catch {
        // File doesn't exist, create it
        const new_file = std.fs.cwd().createFile(path, .{}) catch {
            return Value{ .boolean = false };
        };
        defer new_file.close();
        _ = new_file.pwrite(content, 0) catch {
            return Value{ .boolean = false };
        };
        return Value{ .boolean = true };
    };
    defer file.close();

    // Get current size and append at end
    const stat = file.stat() catch return Value{ .boolean = false };
    _ = file.pwrite(content, stat.size) catch {
        return Value{ .boolean = false };
    };

    return Value{ .boolean = true };
}

fn nativeFileExists(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const path = args[0].string;
    _ = std.fs.cwd().statFile(path) catch {
        return Value{ .boolean = false };
    };

    return Value{ .boolean = true };
}

fn nativeLen(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    return switch (args[0]) {
        .string => |s| Value{ .number = @floatFromInt(s.len) },
        .array => |a| Value{ .number = @floatFromInt(a.elements.items.len) },
        else => RuntimeError.TypeError,
    };
}

fn nativeType(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    const type_name = args[0].typeName();
    return Value{ .string = allocator.dupe(u8, type_name) catch return RuntimeError.OutOfMemory };
}

fn nativeStr(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .string = args[0].toString(allocator) catch return RuntimeError.OutOfMemory };
}

fn nativeNum(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    return switch (args[0]) {
        .number => args[0],
        .string => |s| blk: {
            const n = std.fmt.parseFloat(f64, s) catch {
                break :blk Value{ .null_val = {} };
            };
            break :blk Value{ .number = n };
        },
        .boolean => |b| Value{ .number = if (b) 1.0 else 0.0 },
        else => Value{ .null_val = {} },
    };
}

fn nativeGetEnv(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const name = args[0].string;
    const value = std.process.getEnvVarOwned(allocator, name) catch {
        return Value{ .null_val = {} };
    };

    return Value{ .string = value };
}

fn nativeSplit(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;

    const str = args[0].string;
    const sep = args[1].string;

    const array_ptr = allocator.create(Array) catch return RuntimeError.OutOfMemory;
    array_ptr.* = Array.init();

    var it = std.mem.splitSequence(u8, str, sep);
    while (it.next()) |part| {
        const val = Value{ .string = allocator.dupe(u8, part) catch return RuntimeError.OutOfMemory };
        array_ptr.push(allocator, val) catch return RuntimeError.OutOfMemory;
    }

    return Value{ .array = array_ptr };
}

fn nativeJoin(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .array or args[1] != .string) return RuntimeError.TypeError;

    const arr = args[0].array;
    const sep = args[1].string;

    var list = std.ArrayListUnmanaged(u8).empty;
    defer list.deinit(allocator);

    for (arr.elements.items, 0..) |val, i| {
        if (i > 0) list.appendSlice(allocator, sep) catch return RuntimeError.OutOfMemory;
        const str = val.toString(allocator) catch return RuntimeError.OutOfMemory;
        defer allocator.free(str);
        list.appendSlice(allocator, str) catch return RuntimeError.OutOfMemory;
    }

    return Value{ .string = list.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory };
}

fn nativeReplace(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 3) return RuntimeError.ArityMismatch;
    if (args[0] != .string or args[1] != .string or args[2] != .string) return RuntimeError.TypeError;

    const str = args[0].string;
    const old = args[1].string;
    const new = args[2].string;

    const result = std.mem.replaceOwned(u8, allocator, str, old, new) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}

fn nativeFind(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .string or args[1] != .string) return RuntimeError.TypeError;

    const str = args[0].string;
    const sub = args[1].string;

    if (std.mem.indexOf(u8, str, sub)) |idx| {
        return Value{ .number = @floatFromInt(idx) };
    } else {
        return Value{ .number = -1.0 };
    }
}

fn nativePrint(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const self: *Interpreter = @ptrCast(@alignCast(context));
    for (args) |arg| {
        const str = arg.toString(allocator) catch return RuntimeError.OutOfMemory;
        defer allocator.free(str);
        self.output.appendSlice(self.allocator, str) catch return RuntimeError.OutOfMemory;
    }
    return Value{ .null_val = {} };
}

fn nativePrintLine(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const self: *Interpreter = @ptrCast(@alignCast(context));
    for (args) |arg| {
        const str = arg.toString(allocator) catch return RuntimeError.OutOfMemory;
        defer allocator.free(str);
        self.output.appendSlice(self.allocator, str) catch return RuntimeError.OutOfMemory;
    }
    self.output.append(self.allocator, '\n') catch return RuntimeError.OutOfMemory;
    return Value{ .null_val = {} };
}

fn nativePush(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .array) return RuntimeError.TypeError;

    const arr = args[0].array;
    arr.push(allocator, args[1]) catch return RuntimeError.OutOfMemory;

    return Value{ .number = @floatFromInt(arr.elements.items.len) };
}

fn nativeRender(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const self: *Interpreter = @ptrCast(@alignCast(context));
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const template = args[0].string;
    var transpiled_code = std.ArrayListUnmanaged(u8).empty;
    defer transpiled_code.deinit(allocator);

    var current: usize = 0;
    while (current < template.len) {
        if (std.mem.indexOfPos(u8, template, current, "[")) |start_idx| {
            if (start_idx > current) {
                const text = template[current..start_idx];
                try transpiled_code.appendSlice(allocator, "print(\"");
                for (text) |c| {
                    if (c == '"') {
                        try transpiled_code.appendSlice(allocator, "\\\"");
                    } else if (c == '\n') {
                        try transpiled_code.appendSlice(allocator, "\\n");
                    } else if (c == '\r') {
                        // skip
                    } else {
                        try transpiled_code.append(allocator, c);
                    }
                }
                try transpiled_code.appendSlice(allocator, "\");\n");
            }

            var code_start: usize = start_idx + 1;
            var is_echo = false;
            var closer: []const u8 = "";

            if (std.mem.startsWith(u8, template[code_start..], "f>")) {
                code_start += 2;
                closer = "</]";
            } else if (std.mem.startsWith(u8, template[code_start..], "=>")) {
                code_start += 2;
                is_echo = true;
                closer = "</]";
            } else {
                try transpiled_code.appendSlice(allocator, "print(\"[\");\n");
                current = start_idx + 1;
                continue;
            }

            if (std.mem.indexOfPos(u8, template, code_start, closer)) |end_idx| {
                const code_raw = template[code_start..end_idx];
                if (is_echo) {
                    try transpiled_code.appendSlice(allocator, "print(");
                    try transpiled_code.appendSlice(allocator, code_raw);
                    try transpiled_code.appendSlice(allocator, ");\n");
                } else {
                    try transpiled_code.appendSlice(allocator, code_raw);
                    try transpiled_code.append(allocator, '\n');
                }
                current = end_idx + closer.len;
            } else {
                return RuntimeError.TypeError;
            }
        } else {
            if (current < template.len) {
                const text = template[current..];
                try transpiled_code.appendSlice(allocator, "print(\"");
                for (text) |c| {
                    if (c == '"') {
                        try transpiled_code.appendSlice(allocator, "\\\"");
                    } else if (c == '\n') {
                        try transpiled_code.appendSlice(allocator, "\\n");
                    } else if (c == '\r') {
                        // skip
                    } else {
                        try transpiled_code.append(allocator, c);
                    }
                }
                try transpiled_code.appendSlice(allocator, "\");\n");
            }
            break;
        }
    }

    const previous_output = self.output;
    self.output = std.ArrayListUnmanaged(u8).empty;

    defer {
        self.output.deinit(self.allocator);
        self.output = previous_output;
    }

    const final_source = try transpiled_code.toOwnedSlice(allocator);
    defer allocator.free(final_source);

    self.runSource(final_source) catch |err| {
        std.debug.print("Render error in transpiled code:\n{s}\nError: {}\n", .{ final_source, err });
        return RuntimeError.ThrowError;
    };

    return Value{ .string = self.output.toOwnedSlice(allocator) catch return RuntimeError.OutOfMemory };
}

fn nativeSplitParams(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const query = args[0].string;
    const obj = allocator.create(Object) catch return RuntimeError.OutOfMemory;
    obj.* = Object.init();

    var it = std.mem.tokenizeAny(u8, query, "&;");
    while (it.next()) |pair| {
        var pair_it = std.mem.splitScalar(u8, pair, '=');
        const key = pair_it.next() orelse continue;
        const val = pair_it.next() orelse "";

        obj.set(allocator, key, Value{ .string = allocator.dupe(u8, val) catch return RuntimeError.OutOfMemory }) catch return RuntimeError.OutOfMemory;
    }

    return Value{ .object = obj };
}

// ============ TESTS ============

test "interpreter - echo number" {
    const allocator = std.testing.allocator;

    var interp = try Interpreter.init(allocator);
    defer interp.deinit();

    // Simulate ec 42;
    const echo_stmt = try allocator.create(ast.EchoStmt);
    defer allocator.destroy(echo_stmt);
    echo_stmt.* = ast.EchoStmt{ .expr = ast.Expr{ .literal = ast.Literal{ .number = 42 } } };

    try interp.executeStmt(ast.Stmt{ .echo_stmt = echo_stmt });

    try std.testing.expectEqualStrings("42\n", interp.getOutput());
}

test "interpreter - variable assignment" {
    const allocator = std.testing.allocator;

    var interp = try Interpreter.init(allocator);
    defer interp.deinit();

    // var !x = 10;
    const var_decl = try allocator.create(ast.VarDecl);
    defer allocator.destroy(var_decl);
    var_decl.* = ast.VarDecl{
        .kind = .var_mutable,
        .name = "x",
        .initializer = ast.Expr{ .literal = ast.Literal{ .number = 10 } },
    };

    try interp.executeStmt(ast.Stmt{ .var_decl = var_decl });

    const value = interp.environment.get("x");
    try std.testing.expect(value != null);
    try std.testing.expectEqual(@as(f64, 10), value.?.number);
}
fn nativeMySQLConnect(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const interpreter: *Interpreter = @ptrCast(@alignCast(context));
    _ = allocator; // allocator is not used directly here, but passed to MySQL.connect

    if (args.len != 4) return RuntimeError.ArityMismatch;

    // Check if arguments are strings
    if (args[0] != .string or args[1] != .string or args[2] != .string or args[3] != .string) {
        return RuntimeError.TypeError;
    }

    const host = args[0].string;
    const user = args[1].string;
    const pass = args[2].string;
    const db = args[3].string;

    // Call MySQL.connect
    const fd = MySQL.connect(interpreter.allocator, host, user, pass, db) catch |err| {
        std.debug.print("MySQL Connection Error: {}\n", .{err});
        return Value{ .boolean = false };
    };

    const id = interpreter.next_mysql_id;
    interpreter.next_mysql_id += 1;
    const fd_val: i64 = if (@typeInfo(std.posix.socket_t) == .int) @as(i64, @intCast(fd)) else @as(i64, @intCast(@intFromPtr(fd)));

    interpreter.mysql_connections.put(id, fd_val) catch return RuntimeError.OutOfMemory;

    return Value{ .number = @floatFromInt(id) };
}

fn nativeMySQLQuery(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const interpreter: *Interpreter = @ptrCast(@alignCast(context));
    _ = allocator;

    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    if (args[1] != .string) return RuntimeError.TypeError;

    const id: i32 = @intFromFloat(args[0].number);
    const sql = args[1].string;

    const fd_val = interpreter.mysql_connections.get(id) orelse return RuntimeError.NameError; // Invalid handle
    const fd: std.posix.socket_t = if (@typeInfo(std.posix.socket_t) == .int) @as(std.posix.socket_t, @intCast(fd_val)) else @as(std.posix.socket_t, @ptrFromInt(@as(usize, @intCast(fd_val))));

    const success = MySQL.query(fd, sql) catch |err| {
        std.debug.print("MySQL Query Error: {}\n", .{err});
        return Value{ .boolean = false };
    };
    _ = success;

    // For now return true. Ideally return Resultset as Array of Objects.
    return Value{ .boolean = true };
}

fn nativeMySQLClose(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    const interpreter: *Interpreter = @ptrCast(@alignCast(context));
    _ = allocator;

    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;

    const id: i32 = @intFromFloat(args[0].number);
    if (interpreter.mysql_connections.fetchRemove(id)) |kv| {
        const fd: std.posix.socket_t = if (@typeInfo(std.posix.socket_t) == .int) @as(std.posix.socket_t, @intCast(kv.value)) else @as(std.posix.socket_t, @ptrFromInt(@as(usize, @intCast(kv.value))));
        std.posix.close(fd);
        return Value{ .boolean = true };
    }

    return Value{ .boolean = false };
}

// ============ STRING FUNCTIONS ============

fn nativeUpper(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const s = args[0].string;
    const result = allocator.alloc(u8, s.len) catch return RuntimeError.OutOfMemory;
    for (s, 0..) |c, i| {
        result[i] = std.ascii.toUpper(c);
    }
    return Value{ .string = result };
}

fn nativeLower(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const s = args[0].string;
    const result = allocator.alloc(u8, s.len) catch return RuntimeError.OutOfMemory;
    for (s, 0..) |c, i| {
        result[i] = std.ascii.toLower(c);
    }
    return Value{ .string = result };
}

fn nativeTrim(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    const trimmed = std.mem.trim(u8, args[0].string, " \t\n\r");
    const result = allocator.dupe(u8, trimmed) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}

fn nativeSubstr(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len < 2 or args.len > 3) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;
    if (args[1] != .number) return RuntimeError.TypeError;

    const s = args[0].string;
    const start_f = args[1].number;
    if (start_f < 0) return RuntimeError.TypeError;
    const start: usize = @intFromFloat(start_f);

    if (start >= s.len) return Value{ .string = "" };

    var length: usize = s.len - start;
    if (args.len == 3) {
        if (args[2] != .number) return RuntimeError.TypeError;
        const len_f = args[2].number;
        if (len_f < 0) return RuntimeError.TypeError;
        length = @min(@as(usize, @intFromFloat(len_f)), s.len - start);
    }

    const result = allocator.dupe(u8, s[start .. start + length]) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}

fn nativeContains(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;

    if (args[0] == .string and args[1] == .string) {
        return Value{ .boolean = std.mem.indexOf(u8, args[0].string, args[1].string) != null };
    }

    if (args[0] == .array and args[1] != .null_val) {
        const arr = args[0].array;
        for (arr.elements.items) |item| {
            if (item.isEqual(args[1])) return Value{ .boolean = true };
        }
        return Value{ .boolean = false };
    }

    return RuntimeError.TypeError;
}

// ============ MATH FUNCTIONS ============

fn nativeAbs(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    return Value{ .number = @abs(args[0].number) };
}

fn nativeFloor(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    return Value{ .number = @floor(args[0].number) };
}

fn nativeCeil(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    return Value{ .number = @ceil(args[0].number) };
}

fn nativeRound(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    return Value{ .number = @round(args[0].number) };
}

fn nativeMin(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;
    return Value{ .number = @min(args[0].number, args[1].number) };
}

fn nativeMax(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;
    return Value{ .number = @max(args[0].number, args[1].number) };
}

fn nativeSqrt(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .number) return RuntimeError.TypeError;
    return Value{ .number = @sqrt(args[0].number) };
}

fn nativePow(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 2) return RuntimeError.ArityMismatch;
    if (args[0] != .number or args[1] != .number) return RuntimeError.TypeError;
    return Value{ .number = std.math.pow(f64, args[0].number, args[1].number) };
}

var random_counter: u64 = 12345;
fn nativeRandom(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    _ = args;
    // Simple PRNG without system time
    random_counter = random_counter *% 1103515245 +% 12345;
    const val = @as(f64, @floatFromInt(random_counter & 0x7FFFFFFF)) / @as(f64, 0x7FFFFFFF);
    return Value{ .number = val };
}

// ============ ARRAY FUNCTIONS ============

fn nativeFirst(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    if (args[0] == .array) {
        const arr = args[0].array;
        if (arr.elements.items.len == 0) return Value{ .null_val = {} };
        return arr.elements.items[0];
    }

    if (args[0] == .string) {
        const s = args[0].string;
        if (s.len == 0) return Value{ .null_val = {} };
        return Value{ .string = s[0..1] };
    }

    return RuntimeError.TypeError;
}

fn nativeLast(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    if (args[0] == .array) {
        const arr = args[0].array;
        if (arr.elements.items.len == 0) return Value{ .null_val = {} };
        return arr.elements.items[arr.elements.items.len - 1];
    }

    if (args[0] == .string) {
        const s = args[0].string;
        if (s.len == 0) return Value{ .null_val = {} };
        return Value{ .string = s[s.len - 1 .. s.len] };
    }

    return RuntimeError.TypeError;
}

fn nativePop(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .array) return RuntimeError.TypeError;

    const arr = args[0].array;
    return arr.pop() orelse Value{ .null_val = {} };
}

fn nativeKeys(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .object) return RuntimeError.TypeError;

    const obj = args[0].object;
    const result = allocator.create(value_mod.Array) catch return RuntimeError.OutOfMemory;
    result.* = value_mod.Array.init();

    var it = obj.entries.iterator();
    while (it.next()) |entry| {
        const key_copy = allocator.dupe(u8, entry.key_ptr.*) catch return RuntimeError.OutOfMemory;
        result.push(allocator, Value{ .string = key_copy }) catch return RuntimeError.OutOfMemory;
    }

    return Value{ .array = result };
}

fn nativeValues(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .object) return RuntimeError.TypeError;

    const obj = args[0].object;
    const result = allocator.create(value_mod.Array) catch return RuntimeError.OutOfMemory;
    result.* = value_mod.Array.init();

    var it = obj.entries.iterator();
    while (it.next()) |entry| {
        result.push(allocator, entry.value_ptr.*) catch return RuntimeError.OutOfMemory;
    }

    return Value{ .array = result };
}

// ============ TYPE FUNCTIONS ============

fn nativeIsNum(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .number };
}

fn nativeIsStr(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .string };
}

fn nativeIsArr(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .array };
}

fn nativeIsObj(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .object };
}

fn nativeIsNull(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .null_val };
}

fn nativeIsBool(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    return Value{ .boolean = args[0] == .boolean };
}

// ============ JSON FUNCTIONS ============

fn nativeJsonEncode(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    if (args.len != 1) return RuntimeError.ArityMismatch;

    const result = args[0].toString(allocator) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}

fn nativeJsonDecode(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    if (args.len != 1) return RuntimeError.ArityMismatch;
    if (args[0] != .string) return RuntimeError.TypeError;

    // Simple JSON decode - just return the string for now
    // Full JSON parsing would be more complex
    return args[0];
}

// ============ DATE/TIME FUNCTIONS ============

fn nativeTime(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = allocator;
    _ = args;
    // Return 0 as placeholder - full time API not available in this Zig version
    random_counter +%= 1;
    return Value{ .number = @as(f64, @floatFromInt(random_counter)) };
}

fn nativeDate(context: *anyopaque, args: []Value, allocator: std.mem.Allocator) RuntimeError!Value {
    _ = context;
    _ = args;
    // Return current counter as placeholder
    random_counter +%= 1;
    const result = std.fmt.allocPrint(allocator, "{d}", .{random_counter}) catch return RuntimeError.OutOfMemory;
    return Value{ .string = result };
}
