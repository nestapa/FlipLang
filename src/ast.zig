const std = @import("std");

/// AST Node types for FlipLang
/// These represent the structure of the parsed code

// ============ EXPRESSIONS ============

/// Represents any expression in FlipLang
pub const Expr = union(enum) {
    literal: Literal,
    variable: Variable,
    binary: *Binary,
    unary: *Unary,
    grouping: *Grouping,
    call: *Call,
    array_literal: *ArrayLiteral,
    object_literal: *ObjectLiteral,
    index: *Index,
    member: *Member,
    ternary: *Ternary,
    arrow_fn: *ArrowFn,
    assignment: *Assignment,
    new_expr: *NewExpr,
    this_expr: ThisExpr,
    member_assignment: *MemberAssignment,

    pub fn deinit(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.deinit(allocator);
                b.right.deinit(allocator);
                allocator.destroy(b);
            },
            .unary => |u| {
                u.operand.deinit(allocator);
                allocator.destroy(u);
            },
            .grouping => |g| {
                g.expr.deinit(allocator);
                allocator.destroy(g);
            },
            .call => |c| {
                c.callee.deinit(allocator);
                for (c.args) |*arg| {
                    arg.deinit(allocator);
                }
                allocator.free(c.args);
                allocator.destroy(c);
            },
            .array_literal => |a| {
                for (a.elements) |*elem| {
                    elem.deinit(allocator);
                }
                allocator.free(a.elements);
                allocator.destroy(a);
            },
            .object_literal => |o| {
                for (o.values) |*val| {
                    val.deinit(allocator);
                }
                allocator.free(o.keys);
                allocator.free(o.values);
                allocator.destroy(o);
            },
            .index => |i| {
                i.object.deinit(allocator);
                i.index_expr.deinit(allocator);
                allocator.destroy(i);
            },
            .member => |m| {
                m.object.deinit(allocator);
                allocator.destroy(m);
            },
            .ternary => |t| {
                t.condition.deinit(allocator);
                t.then_branch.deinit(allocator);
                t.else_branch.deinit(allocator);
                allocator.destroy(t);
            },
            .arrow_fn => |f| {
                f.body.deinit(allocator);
                allocator.destroy(f);
            },
            .assignment => |a| {
                a.value.deinit(allocator);
                allocator.destroy(a);
            },
            .new_expr => |n| {
                for (n.args) |*arg| {
                    arg.deinit(allocator);
                }
                allocator.free(n.args);
                allocator.destroy(n);
            },
            .member_assignment => |ma| {
                ma.object.deinit(allocator);
                ma.value.deinit(allocator);
                allocator.destroy(ma);
            },
            .literal, .variable, .this_expr => {},
        }
    }
};

/// Literal values: strings, numbers, booleans, null
pub const Literal = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    null_val: void,
};

/// Variable reference: !nama
pub const Variable = struct {
    name: []const u8,
};

/// Binary expression: left op right (e.g., !a + !b)
pub const Binary = struct {
    left: Expr,
    operator: Operator,
    right: Expr,
};

/// Unary expression: op operand (e.g., -!x, !!flag)
pub const Unary = struct {
    operator: Operator,
    operand: Expr,
};

/// Grouping: (expr)
pub const Grouping = struct {
    expr: Expr,
};

/// Function call: func(args)
pub const Call = struct {
    callee: Expr,
    args: []Expr,
};

/// Array literal: [1, 2, 3]
pub const ArrayLiteral = struct {
    elements: []Expr,
};

/// Object literal: {key: value}
pub const ObjectLiteral = struct {
    keys: [][]const u8,
    values: []Expr,
};

/// Index access: arr[0]
pub const Index = struct {
    object: Expr,
    index_expr: Expr,
};

/// Member access: obj.property
pub const Member = struct {
    object: Expr,
    name: []const u8,
};

/// Ternary: condition ? then : else
pub const Ternary = struct {
    condition: Expr,
    then_branch: Expr,
    else_branch: Expr,
};

/// Arrow function: fn(!a, !b) => !a + !b
pub const ArrowFn = struct {
    params: [][]const u8,
    body: Expr,
};

/// Assignment expression: !x = value
pub const Assignment = struct {
    name: []const u8,
    value: Expr,
};

/// New expression for class instantiation: new ClassName(args)
pub const NewExpr = struct {
    class_name: []const u8,
    args: []Expr,
};

/// This expression: this (reference to current instance)
pub const ThisExpr = struct {};

/// Member assignment expression: this.property = value
pub const MemberAssignment = struct {
    object: Expr,
    name: []const u8,
    value: Expr,
};

/// Operators for binary/unary expressions
pub const Operator = enum {
    // Arithmetic
    plus,
    minus,
    multiply,
    divide,
    modulo,

    // Comparison
    equal,
    not_equal,
    less,
    less_equal,
    greater,
    greater_equal,

    // Logical
    and_op,
    or_op,
    not,

    // Increment/Decrement
    increment,
    decrement,
};

// ============ STATEMENTS ============

/// Represents any statement in FlipLang
pub const Stmt = union(enum) {
    expr_stmt: *ExprStmt,
    echo_stmt: *EchoStmt,
    var_decl: *VarDecl,
    if_stmt: *IfStmt,
    while_stmt: *WhileStmt,
    for_stmt: *ForStmt,
    foreach_stmt: *ForEachStmt,
    function_decl: *FunctionDecl,
    return_stmt: *ReturnStmt,
    block: *Block,
    switch_stmt: *SwitchStmt,
    class_decl: *ClassDecl,
    use_stmt: *UseStmt,
    break_stmt: *BreakStmt,
    continue_stmt: *ContinueStmt,

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .break_stmt => |b| allocator.destroy(b),
            .continue_stmt => |c| allocator.destroy(c),
            .expr_stmt => |e| {
                e.expr.deinit(allocator);
                allocator.destroy(e);
            },
            .echo_stmt => |e| {
                e.expr.deinit(allocator);
                allocator.destroy(e);
            },
            .var_decl => |v| {
                if (v.initializer) |*init| {
                    init.deinit(allocator);
                }
                allocator.destroy(v);
            },
            .if_stmt => |i| {
                i.condition.deinit(allocator);
                i.then_branch.deinit(allocator);
                if (i.else_branch) |*eb| {
                    eb.deinit(allocator);
                }
                allocator.destroy(i);
            },
            .while_stmt => |w| {
                w.condition.deinit(allocator);
                w.body.deinit(allocator);
                allocator.destroy(w);
            },
            .for_stmt => |f| {
                if (f.initializer) |*init| {
                    init.deinit(allocator);
                }
                if (f.condition) |*cond| {
                    cond.deinit(allocator);
                }
                if (f.increment) |*inc| {
                    inc.deinit(allocator);
                }
                f.body.deinit(allocator);
                allocator.destroy(f);
            },
            .foreach_stmt => |fe| {
                fe.iterable.deinit(allocator);
                fe.body.deinit(allocator);
                allocator.destroy(fe);
            },
            .function_decl => |fd| {
                fd.body.deinit(allocator);
                allocator.destroy(fd);
            },
            .return_stmt => |r| {
                if (r.value) |*v| {
                    v.deinit(allocator);
                }
                allocator.destroy(r);
            },
            .block => |b| {
                for (b.statements) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(b.statements);
                allocator.destroy(b);
            },
            .switch_stmt => |s| {
                s.expr.deinit(allocator);
                for (s.cases) |*case| {
                    case.value.deinit(allocator);
                    for (case.body) |*stmt| {
                        stmt.deinit(allocator);
                    }
                    allocator.free(case.body);
                }
                allocator.free(s.cases);
                if (s.default) |default_stmts| {
                    for (default_stmts) |*stmt| {
                        stmt.deinit(allocator);
                    }
                    allocator.free(default_stmts);
                }
                allocator.destroy(s);
            },
            .class_decl => |cd| {
                if (cd.init_method) |init| {
                    var init_stmt = Stmt{ .function_decl = init };
                    init_stmt.deinit(allocator);
                }
                for (cd.methods) |method| {
                    var method_stmt = Stmt{ .function_decl = method };
                    method_stmt.deinit(allocator);
                }
                allocator.free(cd.methods);
                allocator.destroy(cd);
            },
            .use_stmt => |u| {
                allocator.destroy(u);
            },
        }
    }
};

/// Expression statement: expr;
pub const ExprStmt = struct {
    expr: Expr,
};

/// Echo statement: ec expr;
pub const EchoStmt = struct {
    expr: Expr,
};

/// Variable declaration: var/let/const !name = expr;
pub const VarDecl = struct {
    kind: VarKind,
    name: []const u8,
    initializer: ?Expr,
};

pub const VarKind = enum {
    var_mutable, // var
    let_mutable, // let (block-scoped)
    const_immutable, // const
};

/// If statement: if condition: then_branch else: else_branch
pub const IfStmt = struct {
    condition: Expr,
    then_branch: Stmt,
    else_branch: ?Stmt,
};

/// While statement: while condition: body
pub const WhileStmt = struct {
    condition: Expr,
    body: Stmt,
};

/// For statement: for init; cond; inc: body
pub const ForStmt = struct {
    initializer: ?Stmt,
    condition: ?Expr,
    increment: ?Expr,
    body: Stmt,
};

/// ForEach statement: foreach array as item: body
pub const ForEachStmt = struct {
    iterable: Expr,
    item_name: []const u8,
    body: Stmt,
};

/// Function declaration: function name(params) { body }
pub const FunctionDecl = struct {
    name: []const u8,
    params: [][]const u8,
    body: Stmt,
};

/// Return statement: return expr;
pub const ReturnStmt = struct {
    value: ?Expr,
};

/// Block of statements: { stmts }
pub const Block = struct {
    statements: []Stmt,
};

/// Switch statement
pub const SwitchStmt = struct {
    expr: Expr,
    cases: []Case,
    default: ?[]Stmt,
};

pub const Case = struct {
    value: Expr,
    body: []Stmt,
};

/// Class declaration: class ClassName { init(...) { } function method() { } }
pub const ClassDecl = struct {
    name: []const u8,
    init_method: ?*FunctionDecl,
    methods: []*FunctionDecl,
};

/// Use statement: use "path/to/file.flip";
pub const UseStmt = struct {
    path: []const u8,
};

/// Break statement
pub const BreakStmt = struct {};

/// Continue statement
pub const ContinueStmt = struct {};

// ============ PROGRAM ============

/// Root of the AST - a program is a list of statements
pub const Program = struct {
    statements: []Stmt,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Program) void {
        for (self.statements) |*stmt| {
            stmt.deinit(self.allocator);
        }
        self.allocator.free(self.statements);
    }
};

// ============ TESTS ============

test "literal creation" {
    const num_lit = Literal{ .number = 42 };
    try std.testing.expectEqual(@as(f64, 42), num_lit.number);

    const str_lit = Literal{ .string = "hello" };
    try std.testing.expectEqualStrings("hello", str_lit.string);

    const bool_lit = Literal{ .boolean = true };
    try std.testing.expect(bool_lit.boolean);
}

test "variable creation" {
    const v = Variable{ .name = "nama" };
    try std.testing.expectEqualStrings("nama", v.name);
}

test "operator enum" {
    const op = Operator.plus;
    try std.testing.expectEqual(Operator.plus, op);
}
