const std = @import("std");
const token_mod = @import("token.zig");
const ast = @import("ast.zig");
const Token = token_mod.Token;
const TokenType = token_mod.TokenType;

/// FlipLang Parser
/// Converts tokens into an Abstract Syntax Tree (AST)
pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: std.mem.Allocator,

    /// Error types for parsing
    pub const ParseError = error{
        UnexpectedToken,
        ExpectedExpression,
        ExpectedSemicolon,
        ExpectedColon,
        ExpectedIdentifier,
        ExpectedVariable,
        ExpectedRightParen,
        ExpectedRightBrace,
        ExpectedRightBracket,
        UnterminatedString,
        InvalidAssignmentTarget,
        OutOfMemory,
    };

    /// Initialize a new parser
    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    /// Parse the token stream into a program
    pub fn parse(self: *Parser) ParseError!ast.Program {
        var statements: std.ArrayListUnmanaged(ast.Stmt) = .empty;
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit(self.allocator);
        }

        while (!self.isAtEnd()) {
            const stmt = try self.declaration();
            statements.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        return ast.Program{
            .statements = statements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .allocator = self.allocator,
        };
    }

    // ============ DECLARATIONS ============

    fn declaration(self: *Parser) ParseError!ast.Stmt {
        if (self.match(&.{.kw_var})) return self.varDeclaration(.var_mutable);
        if (self.match(&.{.kw_let})) return self.varDeclaration(.let_mutable);
        if (self.match(&.{.kw_const})) return self.varDeclaration(.const_immutable);
        if (self.match(&.{.kw_function})) return self.functionDeclaration();
        if (self.match(&.{.kw_class})) return self.classDeclaration();
        if (self.match(&.{.kw_switch})) return self.switchStatement();
        return self.statement();
    }

    fn switchStatement(self: *Parser) ParseError!ast.Stmt {
        const expr = try self.expression();

        if (!self.match(&.{.colon})) {
            return ParseError.ExpectedColon;
        }

        if (!self.match(&.{.left_brace})) {
            return ParseError.UnexpectedToken;
        }

        var cases: std.ArrayListUnmanaged(ast.Case) = .empty;
        errdefer {
            for (cases.items) |*case| {
                case.value.deinit(self.allocator);
                for (case.body) |*stmt| {
                    stmt.deinit(self.allocator);
                }
                self.allocator.free(case.body);
            }
            cases.deinit(self.allocator);
        }

        var default_body: ?[]ast.Stmt = null;
        errdefer if (default_body) |stmts| {
            for (stmts) |*stmt| {
                stmt.deinit(self.allocator);
            }
            self.allocator.free(stmts);
        };

        while (!self.check(.right_brace) and !self.isAtEnd()) {
            if (self.match(&.{.kw_case})) {
                const case_value = try self.expression();

                if (!self.match(&.{.colon})) {
                    return ParseError.ExpectedColon;
                }

                var param_stmts: std.ArrayListUnmanaged(ast.Stmt) = .empty;
                // Parse statements until next case, default, or }
                while (!self.check(.kw_case) and !self.check(.kw_default) and !self.check(.right_brace) and !self.isAtEnd()) {
                    const stmt = try self.declaration();
                    param_stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
                }

                cases.append(self.allocator, ast.Case{
                    .value = case_value,
                    .body = param_stmts.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
                }) catch return ParseError.OutOfMemory;
            } else if (self.match(&.{.kw_default})) {
                if (!self.match(&.{.colon})) {
                    return ParseError.ExpectedColon;
                }

                var param_stmts: std.ArrayListUnmanaged(ast.Stmt) = .empty;
                while (!self.check(.kw_case) and !self.check(.kw_default) and !self.check(.right_brace) and !self.isAtEnd()) {
                    const stmt = try self.declaration();
                    param_stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
                }

                default_body = param_stmts.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory;
            } else {
                return ParseError.UnexpectedToken;
            }
        }

        if (!self.match(&.{.right_brace})) {
            return ParseError.ExpectedRightBrace;
        }

        const switch_stmt = self.allocator.create(ast.SwitchStmt) catch return ParseError.OutOfMemory;
        switch_stmt.* = ast.SwitchStmt{
            .expr = expr,
            .cases = cases.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .default = default_body,
        };

        return ast.Stmt{ .switch_stmt = switch_stmt };
    }

    fn varDeclaration(self: *Parser, kind: ast.VarKind) ParseError!ast.Stmt {
        // Expect !variable
        if (!self.check(.variable)) {
            return ParseError.ExpectedVariable;
        }
        const name_token = self.advance();
        // Remove the ! prefix from the variable name
        const name = if (name_token.lexeme.len > 0 and name_token.lexeme[0] == '!')
            name_token.lexeme[1..]
        else
            name_token.lexeme;

        var initializer: ?ast.Expr = null;
        if (self.match(&.{.equal})) {
            initializer = try self.expression();
        }

        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        const var_decl = self.allocator.create(ast.VarDecl) catch return ParseError.OutOfMemory;
        var_decl.* = ast.VarDecl{
            .kind = kind,
            .name = name,
            .initializer = initializer,
        };

        return ast.Stmt{ .var_decl = var_decl };
    }

    fn functionDeclaration(self: *Parser) ParseError!ast.Stmt {
        // Expect function name
        if (!self.check(.identifier)) {
            return ParseError.ExpectedIdentifier;
        }
        const name = self.advance().lexeme;

        // Expect (
        if (!self.match(&.{.left_paren})) {
            return ParseError.UnexpectedToken;
        }

        var params: std.ArrayListUnmanaged([]const u8) = .empty;
        errdefer params.deinit(self.allocator);

        if (!self.check(.right_paren)) {
            while (true) {
                if (self.check(.variable)) {
                    const param_token = self.advance();
                    const param_name = if (param_token.lexeme.len > 0 and param_token.lexeme[0] == '!')
                        param_token.lexeme[1..]
                    else
                        param_token.lexeme;
                    params.append(self.allocator, param_name) catch return ParseError.OutOfMemory;
                } else {
                    return ParseError.ExpectedVariable;
                }

                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_paren})) {
            return ParseError.ExpectedRightParen;
        }

        // Expect { body }
        if (!self.match(&.{.left_brace})) {
            return ParseError.UnexpectedToken;
        }

        const body = try self.block();

        const func_decl = self.allocator.create(ast.FunctionDecl) catch return ParseError.OutOfMemory;
        func_decl.* = ast.FunctionDecl{
            .name = name,
            .params = params.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .body = body,
        };

        return ast.Stmt{ .function_decl = func_decl };
    }

    fn classDeclaration(self: *Parser) ParseError!ast.Stmt {
        // Expect class name
        if (!self.check(.identifier)) {
            return ParseError.ExpectedIdentifier;
        }
        const class_name = self.advance().lexeme;

        // Expect {
        if (!self.match(&.{.left_brace})) {
            return ParseError.UnexpectedToken;
        }

        var init_method: ?*ast.FunctionDecl = null;
        var methods: std.ArrayListUnmanaged(*ast.FunctionDecl) = .empty;
        errdefer methods.deinit(self.allocator);

        // Parse class body
        while (!self.check(.right_brace) and !self.isAtEnd()) {
            if (self.match(&.{.kw_init})) {
                // Parse init method (constructor)
                if (!self.match(&.{.left_paren})) {
                    return ParseError.UnexpectedToken;
                }

                var params: std.ArrayListUnmanaged([]const u8) = .empty;
                errdefer params.deinit(self.allocator);

                if (!self.check(.right_paren)) {
                    while (true) {
                        if (self.check(.variable)) {
                            const param_token = self.advance();
                            const param_name = if (param_token.lexeme.len > 0 and param_token.lexeme[0] == '!')
                                param_token.lexeme[1..]
                            else
                                param_token.lexeme;
                            params.append(self.allocator, param_name) catch return ParseError.OutOfMemory;
                        } else {
                            return ParseError.ExpectedVariable;
                        }
                        if (!self.match(&.{.comma})) break;
                    }
                }

                if (!self.match(&.{.right_paren})) {
                    return ParseError.ExpectedRightParen;
                }

                if (!self.match(&.{.left_brace})) {
                    return ParseError.UnexpectedToken;
                }

                const body = try self.block();

                const init_decl = self.allocator.create(ast.FunctionDecl) catch return ParseError.OutOfMemory;
                init_decl.* = ast.FunctionDecl{
                    .name = "init",
                    .params = params.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
                    .body = body,
                };
                init_method = init_decl;
            } else if (self.match(&.{.kw_function})) {
                // Parse method
                if (!self.check(.identifier)) {
                    return ParseError.ExpectedIdentifier;
                }
                const method_name = self.advance().lexeme;

                if (!self.match(&.{.left_paren})) {
                    return ParseError.UnexpectedToken;
                }

                var params: std.ArrayListUnmanaged([]const u8) = .empty;
                errdefer params.deinit(self.allocator);

                if (!self.check(.right_paren)) {
                    while (true) {
                        if (self.check(.variable)) {
                            const param_token = self.advance();
                            const param_name = if (param_token.lexeme.len > 0 and param_token.lexeme[0] == '!')
                                param_token.lexeme[1..]
                            else
                                param_token.lexeme;
                            params.append(self.allocator, param_name) catch return ParseError.OutOfMemory;
                        } else {
                            return ParseError.ExpectedVariable;
                        }
                        if (!self.match(&.{.comma})) break;
                    }
                }

                if (!self.match(&.{.right_paren})) {
                    return ParseError.ExpectedRightParen;
                }

                if (!self.match(&.{.left_brace})) {
                    return ParseError.UnexpectedToken;
                }

                const body = try self.block();

                const method_decl = self.allocator.create(ast.FunctionDecl) catch return ParseError.OutOfMemory;
                method_decl.* = ast.FunctionDecl{
                    .name = method_name,
                    .params = params.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
                    .body = body,
                };
                methods.append(self.allocator, method_decl) catch return ParseError.OutOfMemory;
            } else {
                return ParseError.UnexpectedToken;
            }
        }

        if (!self.match(&.{.right_brace})) {
            return ParseError.ExpectedRightBrace;
        }

        const class_decl = self.allocator.create(ast.ClassDecl) catch return ParseError.OutOfMemory;
        class_decl.* = ast.ClassDecl{
            .name = class_name,
            .init_method = init_method,
            .methods = methods.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Stmt{ .class_decl = class_decl };
    }

    fn newExpression(self: *Parser) ParseError!ast.Expr {
        // Expect class name
        if (!self.check(.identifier)) {
            return ParseError.ExpectedIdentifier;
        }
        const class_name = self.advance().lexeme;

        // Expect (
        if (!self.match(&.{.left_paren})) {
            return ParseError.UnexpectedToken;
        }

        var args: std.ArrayListUnmanaged(ast.Expr) = .empty;
        errdefer {
            for (args.items) |*arg| {
                arg.deinit(self.allocator);
            }
            args.deinit(self.allocator);
        }

        if (!self.check(.right_paren)) {
            while (true) {
                const arg = try self.expression();
                args.append(self.allocator, arg) catch return ParseError.OutOfMemory;
                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_paren})) {
            return ParseError.ExpectedRightParen;
        }

        const new_expr = self.allocator.create(ast.NewExpr) catch return ParseError.OutOfMemory;
        new_expr.* = ast.NewExpr{
            .class_name = class_name,
            .args = args.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Expr{ .new_expr = new_expr };
    }

    // ============ STATEMENTS ============

    fn statement(self: *Parser) ParseError!ast.Stmt {
        if (self.match(&.{.kw_ec})) return self.echoStatement();
        if (self.match(&.{.kw_if})) return self.ifStatement();
        if (self.match(&.{.kw_while})) return self.whileStatement();
        if (self.match(&.{.kw_for})) return self.forStatement();
        if (self.match(&.{.kw_foreach})) return self.forEachStatement();
        if (self.match(&.{.kw_return})) return self.returnStatement();
        if (self.match(&.{.kw_break})) return self.breakStatement();
        if (self.match(&.{.kw_continue})) return self.continueStatement();
        if (self.match(&.{.left_brace})) return self.block();
        if (self.match(&.{.kw_use})) return self.useStatement();

        return self.expressionStatement();
    }

    fn breakStatement(self: *Parser) ParseError!ast.Stmt {
        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }
        const stmt = self.allocator.create(ast.BreakStmt) catch return ParseError.OutOfMemory;
        stmt.* = ast.BreakStmt{};
        return ast.Stmt{ .break_stmt = stmt };
    }

    fn continueStatement(self: *Parser) ParseError!ast.Stmt {
        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }
        const stmt = self.allocator.create(ast.ContinueStmt) catch return ParseError.OutOfMemory;
        stmt.* = ast.ContinueStmt{};
        return ast.Stmt{ .continue_stmt = stmt };
    }

    fn useStatement(self: *Parser) ParseError!ast.Stmt {
        // Expect string literal for path
        if (!self.check(.string)) {
            return ParseError.ExpectedExpression;
        }
        const path_token = self.advance();

        // Remove quotes from path
        var path = path_token.lexeme;
        if (path.len >= 2) {
            path = path[1 .. path.len - 1]; // Remove surrounding quotes
        }

        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        const use_stmt = self.allocator.create(ast.UseStmt) catch return ParseError.OutOfMemory;
        use_stmt.* = ast.UseStmt{ .path = path };

        return ast.Stmt{ .use_stmt = use_stmt };
    }

    fn echoStatement(self: *Parser) ParseError!ast.Stmt {
        const expr = try self.expression();

        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        const echo_stmt = self.allocator.create(ast.EchoStmt) catch return ParseError.OutOfMemory;
        echo_stmt.* = ast.EchoStmt{ .expr = expr };

        return ast.Stmt{ .echo_stmt = echo_stmt };
    }

    fn ifStatement(self: *Parser) ParseError!ast.Stmt {
        const condition = try self.expression();

        if (!self.match(&.{.colon})) {
            return ParseError.ExpectedColon;
        }

        const then_branch = try self.statement();

        var else_branch: ?ast.Stmt = null;
        if (self.match(&.{.kw_else})) {
            if (self.match(&.{.colon})) {
                else_branch = try self.statement();
            } else if (self.match(&.{.kw_if})) {
                else_branch = try self.ifStatement();
            } else {
                return ParseError.UnexpectedToken;
            }
        }

        const if_stmt = self.allocator.create(ast.IfStmt) catch return ParseError.OutOfMemory;
        if_stmt.* = ast.IfStmt{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };

        return ast.Stmt{ .if_stmt = if_stmt };
    }

    fn whileStatement(self: *Parser) ParseError!ast.Stmt {
        const condition = try self.expression();

        if (!self.match(&.{.colon})) {
            return ParseError.ExpectedColon;
        }

        const body = try self.statement();

        const while_stmt = self.allocator.create(ast.WhileStmt) catch return ParseError.OutOfMemory;
        while_stmt.* = ast.WhileStmt{
            .condition = condition,
            .body = body,
        };

        return ast.Stmt{ .while_stmt = while_stmt };
    }

    fn forStatement(self: *Parser) ParseError!ast.Stmt {
        // for !i=0; !i<5; !i++:

        // Initializer
        var initializer: ?ast.Stmt = null;
        if (!self.check(.semicolon)) {
            initializer = try self.declaration();
        } else {
            _ = self.advance(); // consume ;
        }

        // Condition
        var condition: ?ast.Expr = null;
        if (!self.check(.semicolon)) {
            condition = try self.expression();
        }
        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        // Increment
        var increment: ?ast.Expr = null;
        if (!self.check(.colon)) {
            increment = try self.expression();
        }

        if (!self.match(&.{.colon})) {
            return ParseError.ExpectedColon;
        }

        const body = try self.statement();

        const for_stmt = self.allocator.create(ast.ForStmt) catch return ParseError.OutOfMemory;
        for_stmt.* = ast.ForStmt{
            .initializer = initializer,
            .condition = condition,
            .increment = increment,
            .body = body,
        };

        return ast.Stmt{ .for_stmt = for_stmt };
    }

    fn forEachStatement(self: *Parser) ParseError!ast.Stmt {
        // foreach !arr as !item:
        const iterable = try self.expression();

        if (!self.match(&.{.kw_as})) {
            return ParseError.UnexpectedToken;
        }

        if (!self.check(.variable)) {
            return ParseError.ExpectedVariable;
        }
        const item_token = self.advance();
        const item_name = if (item_token.lexeme.len > 0 and item_token.lexeme[0] == '!')
            item_token.lexeme[1..]
        else
            item_token.lexeme;

        if (!self.match(&.{.colon})) {
            return ParseError.ExpectedColon;
        }

        const body = try self.statement();

        const foreach_stmt = self.allocator.create(ast.ForEachStmt) catch return ParseError.OutOfMemory;
        foreach_stmt.* = ast.ForEachStmt{
            .iterable = iterable,
            .item_name = item_name,
            .body = body,
        };

        return ast.Stmt{ .foreach_stmt = foreach_stmt };
    }

    fn returnStatement(self: *Parser) ParseError!ast.Stmt {
        var value: ?ast.Expr = null;

        if (!self.check(.semicolon)) {
            value = try self.expression();
        }

        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        const return_stmt = self.allocator.create(ast.ReturnStmt) catch return ParseError.OutOfMemory;
        return_stmt.* = ast.ReturnStmt{ .value = value };

        return ast.Stmt{ .return_stmt = return_stmt };
    }

    fn block(self: *Parser) ParseError!ast.Stmt {
        var statements: std.ArrayListUnmanaged(ast.Stmt) = .empty;
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit(self.allocator);
        }

        while (!self.check(.right_brace) and !self.isAtEnd()) {
            const stmt = try self.declaration();
            statements.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        if (!self.match(&.{.right_brace})) {
            return ParseError.ExpectedRightBrace;
        }

        const blk = self.allocator.create(ast.Block) catch return ParseError.OutOfMemory;
        blk.* = ast.Block{
            .statements = statements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Stmt{ .block = blk };
    }

    fn expressionStatement(self: *Parser) ParseError!ast.Stmt {
        const expr = try self.expression();

        if (!self.match(&.{.semicolon})) {
            return ParseError.ExpectedSemicolon;
        }

        const expr_stmt = self.allocator.create(ast.ExprStmt) catch return ParseError.OutOfMemory;
        expr_stmt.* = ast.ExprStmt{ .expr = expr };

        return ast.Stmt{ .expr_stmt = expr_stmt };
    }

    // ============ EXPRESSIONS ============

    fn expression(self: *Parser) ParseError!ast.Expr {
        return self.assignment();
    }

    fn assignment(self: *Parser) ParseError!ast.Expr {
        const expr = try self.ternary();

        if (self.match(&.{ .equal, .plus_equal, .minus_equal, .star_equal, .slash_equal })) {
            const operator = self.previous();
            const value = try self.assignment();

            // Check if it's a variable or member access
            switch (expr) {
                .variable => |v| {
                    // For compound assignment (+=, -=, etc.), create binary + assignment
                    if (operator.type != .equal) {
                        const op = switch (operator.type) {
                            .plus_equal => ast.Operator.plus,
                            .minus_equal => ast.Operator.minus,
                            .star_equal => ast.Operator.multiply,
                            .slash_equal => ast.Operator.divide,
                            else => unreachable,
                        };

                        // Create binary: !x + value
                        const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
                        binary.* = ast.Binary{
                            .left = expr,
                            .operator = op,
                            .right = value,
                        };

                        // Create assignment: !x = (!x + value)
                        const assign = self.allocator.create(ast.Assignment) catch return ParseError.OutOfMemory;
                        assign.* = ast.Assignment{
                            .name = v.name,
                            .value = ast.Expr{ .binary = binary },
                        };

                        return ast.Expr{ .assignment = assign };
                    }

                    // Simple assignment !x = value
                    const assign = self.allocator.create(ast.Assignment) catch return ParseError.OutOfMemory;
                    assign.* = ast.Assignment{
                        .name = v.name,
                        .value = value,
                    };

                    return ast.Expr{ .assignment = assign };
                },
                .member => |m| {
                    // Member assignment: this.prop = value
                    if (operator.type != .equal) {
                        return ParseError.InvalidAssignmentTarget;
                    }

                    const member_assign = self.allocator.create(ast.MemberAssignment) catch return ParseError.OutOfMemory;
                    member_assign.* = ast.MemberAssignment{
                        .object = m.object,
                        .name = m.name,
                        .value = value,
                    };

                    return ast.Expr{ .member_assignment = member_assign };
                },
                else => return ParseError.InvalidAssignmentTarget,
            }
        }

        return expr;
    }

    fn ternary(self: *Parser) ParseError!ast.Expr {
        var expr = try self.orExpr();

        if (self.match(&.{.question})) {
            const then_branch = try self.expression();
            if (!self.match(&.{.colon})) {
                return ParseError.ExpectedColon;
            }
            const else_branch = try self.ternary();

            const tern = self.allocator.create(ast.Ternary) catch return ParseError.OutOfMemory;
            tern.* = ast.Ternary{
                .condition = expr,
                .then_branch = then_branch,
                .else_branch = else_branch,
            };

            expr = ast.Expr{ .ternary = tern };
        }

        return expr;
    }

    fn orExpr(self: *Parser) ParseError!ast.Expr {
        var expr = try self.andExpr();

        while (self.match(&.{.pipe_pipe})) {
            const right = try self.andExpr();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = .or_op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn andExpr(self: *Parser) ParseError!ast.Expr {
        var expr = try self.equality();

        while (self.match(&.{.ampersand_ampersand})) {
            const right = try self.equality();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = .and_op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn equality(self: *Parser) ParseError!ast.Expr {
        var expr = try self.comparison();

        while (self.match(&.{ .equal_equal, .bang_equal })) {
            const operator_token = self.previous();
            const op: ast.Operator = if (operator_token.type == .equal_equal) .equal else .not_equal;
            const right = try self.comparison();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn comparison(self: *Parser) ParseError!ast.Expr {
        var expr = try self.term();

        while (self.match(&.{ .greater, .greater_equal, .less, .less_equal })) {
            const operator_token = self.previous();
            const op: ast.Operator = switch (operator_token.type) {
                .greater => .greater,
                .greater_equal => .greater_equal,
                .less => .less,
                .less_equal => .less_equal,
                else => unreachable,
            };
            const right = try self.term();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn term(self: *Parser) ParseError!ast.Expr {
        var expr = try self.factor();

        while (self.match(&.{ .plus, .minus })) {
            const operator_token = self.previous();
            const op: ast.Operator = if (operator_token.type == .plus) .plus else .minus;
            const right = try self.factor();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn factor(self: *Parser) ParseError!ast.Expr {
        var expr = try self.unary();

        while (self.match(&.{ .star, .slash, .percent })) {
            const operator_token = self.previous();
            const op: ast.Operator = switch (operator_token.type) {
                .star => .multiply,
                .slash => .divide,
                .percent => .modulo,
                else => unreachable,
            };
            const right = try self.unary();
            const binary = self.allocator.create(ast.Binary) catch return ParseError.OutOfMemory;
            binary.* = ast.Binary{
                .left = expr,
                .operator = op,
                .right = right,
            };
            expr = ast.Expr{ .binary = binary };
        }

        return expr;
    }

    fn unary(self: *Parser) ParseError!ast.Expr {
        if (self.match(&.{ .bang, .minus })) {
            const operator_token = self.previous();
            const op: ast.Operator = if (operator_token.type == .bang) .not else .minus;
            const operand = try self.unary();
            const un = self.allocator.create(ast.Unary) catch return ParseError.OutOfMemory;
            un.* = ast.Unary{
                .operator = op,
                .operand = operand,
            };
            return ast.Expr{ .unary = un };
        }

        return self.postfix();
    }

    fn postfix(self: *Parser) ParseError!ast.Expr {
        var expr = try self.call();

        while (self.match(&.{ .plus_plus, .minus_minus })) {
            const operator_token = self.previous();
            const op: ast.Operator = if (operator_token.type == .plus_plus) .increment else .decrement;
            const un = self.allocator.create(ast.Unary) catch return ParseError.OutOfMemory;
            un.* = ast.Unary{
                .operator = op,
                .operand = expr,
            };
            expr = ast.Expr{ .unary = un };
        }

        return expr;
    }

    fn call(self: *Parser) ParseError!ast.Expr {
        var expr = try self.primary();

        while (true) {
            if (self.match(&.{.left_paren})) {
                expr = try self.finishCall(expr);
            } else if (self.match(&.{.left_bracket})) {
                const index_expr = try self.expression();
                if (!self.match(&.{.right_bracket})) {
                    return ParseError.ExpectedRightBracket;
                }
                const idx = self.allocator.create(ast.Index) catch return ParseError.OutOfMemory;
                idx.* = ast.Index{
                    .object = expr,
                    .index_expr = index_expr,
                };
                expr = ast.Expr{ .index = idx };
            } else if (self.match(&.{.dot})) {
                if (!self.check(.identifier)) {
                    return ParseError.ExpectedIdentifier;
                }
                const name = self.advance().lexeme;
                const member = self.allocator.create(ast.Member) catch return ParseError.OutOfMemory;
                member.* = ast.Member{
                    .object = expr,
                    .name = name,
                };
                expr = ast.Expr{ .member = member };
            } else {
                break;
            }
        }

        return expr;
    }

    fn finishCall(self: *Parser, callee: ast.Expr) ParseError!ast.Expr {
        var args: std.ArrayListUnmanaged(ast.Expr) = .empty;
        errdefer {
            for (args.items) |*arg| {
                arg.deinit(self.allocator);
            }
            args.deinit(self.allocator);
        }

        if (!self.check(.right_paren)) {
            while (true) {
                const arg = try self.expression();
                args.append(self.allocator, arg) catch return ParseError.OutOfMemory;
                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_paren})) {
            return ParseError.ExpectedRightParen;
        }

        const call_node = self.allocator.create(ast.Call) catch return ParseError.OutOfMemory;
        call_node.* = ast.Call{
            .callee = callee,
            .args = args.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Expr{ .call = call_node };
    }

    fn primary(self: *Parser) ParseError!ast.Expr {
        // Boolean literals
        if (self.match(&.{.true_literal})) {
            return ast.Expr{ .literal = ast.Literal{ .boolean = true } };
        }
        if (self.match(&.{.false_literal})) {
            return ast.Expr{ .literal = ast.Literal{ .boolean = false } };
        }
        if (self.match(&.{.null_literal})) {
            return ast.Expr{ .literal = ast.Literal{ .null_val = {} } };
        }

        // Number literal
        if (self.match(&.{.number})) {
            const lexeme = self.previous().lexeme;
            const num = std.fmt.parseFloat(f64, lexeme) catch 0;
            return ast.Expr{ .literal = ast.Literal{ .number = num } };
        }

        // String literal
        if (self.match(&.{.string})) {
            const lexeme = self.previous().lexeme;
            // Remove quotes and unescape
            const str_content = if (lexeme.len >= 2) lexeme[1 .. lexeme.len - 1] else lexeme;
            const str = try self.unescapeString(str_content);
            return ast.Expr{ .literal = ast.Literal{ .string = str } };
        }

        // Variable (!identifier)
        if (self.match(&.{.variable})) {
            const lexeme = self.previous().lexeme;
            const name = if (lexeme.len > 0 and lexeme[0] == '!') lexeme[1..] else lexeme;
            return ast.Expr{ .variable = ast.Variable{ .name = name } };
        }

        // Identifier (function name, etc.)
        if (self.match(&.{.identifier})) {
            const name = self.previous().lexeme;
            return ast.Expr{ .variable = ast.Variable{ .name = name } };
        }

        // Array literal
        if (self.match(&.{.left_bracket})) {
            return self.arrayLiteral();
        }

        // Object literal
        if (self.match(&.{.left_brace})) {
            return self.objectLiteral();
        }

        // Grouping
        if (self.match(&.{.left_paren})) {
            const expr = try self.expression();
            if (!self.match(&.{.right_paren})) {
                return ParseError.ExpectedRightParen;
            }
            const grp = self.allocator.create(ast.Grouping) catch return ParseError.OutOfMemory;
            grp.* = ast.Grouping{ .expr = expr };
            return ast.Expr{ .grouping = grp };
        }

        // Arrow function: fn(!a, !b) => expr
        if (self.match(&.{.kw_fn})) {
            return self.arrowFunction();
        }

        // New expression: new ClassName(args)
        if (self.match(&.{.kw_new})) {
            return self.newExpression();
        }

        // This expression
        if (self.match(&.{.kw_this})) {
            return ast.Expr{ .this_expr = ast.ThisExpr{} };
        }

        return ParseError.ExpectedExpression;
    }

    fn arrayLiteral(self: *Parser) ParseError!ast.Expr {
        var elements: std.ArrayListUnmanaged(ast.Expr) = .empty;
        errdefer {
            for (elements.items) |*elem| {
                elem.deinit(self.allocator);
            }
            elements.deinit(self.allocator);
        }

        if (!self.check(.right_bracket)) {
            while (true) {
                const elem = try self.expression();
                elements.append(self.allocator, elem) catch return ParseError.OutOfMemory;
                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_bracket})) {
            return ParseError.ExpectedRightBracket;
        }

        const arr = self.allocator.create(ast.ArrayLiteral) catch return ParseError.OutOfMemory;
        arr.* = ast.ArrayLiteral{
            .elements = elements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Expr{ .array_literal = arr };
    }

    fn objectLiteral(self: *Parser) ParseError!ast.Expr {
        var keys: std.ArrayListUnmanaged([]const u8) = .empty;
        var values: std.ArrayListUnmanaged(ast.Expr) = .empty;
        errdefer {
            keys.deinit(self.allocator);
            for (values.items) |*val| {
                val.deinit(self.allocator);
            }
            values.deinit(self.allocator);
        }

        if (!self.check(.right_brace)) {
            while (true) {
                // Key can be identifier or string
                var key: []const u8 = undefined;
                if (self.match(&.{.identifier})) {
                    key = self.previous().lexeme;
                } else if (self.match(&.{.string})) {
                    const lexeme = self.previous().lexeme;
                    key = if (lexeme.len >= 2) lexeme[1 .. lexeme.len - 1] else lexeme;
                } else {
                    return ParseError.ExpectedIdentifier;
                }

                if (!self.match(&.{.colon})) {
                    return ParseError.ExpectedColon;
                }

                const value = try self.expression();

                keys.append(self.allocator, key) catch return ParseError.OutOfMemory;
                values.append(self.allocator, value) catch return ParseError.OutOfMemory;

                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_brace})) {
            return ParseError.ExpectedRightBrace;
        }

        const obj = self.allocator.create(ast.ObjectLiteral) catch return ParseError.OutOfMemory;
        obj.* = ast.ObjectLiteral{
            .keys = keys.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .values = values.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
        };

        return ast.Expr{ .object_literal = obj };
    }

    fn arrowFunction(self: *Parser) ParseError!ast.Expr {
        // fn(!a, !b) => expr
        if (!self.match(&.{.left_paren})) {
            return ParseError.UnexpectedToken;
        }

        var params: std.ArrayListUnmanaged([]const u8) = .empty;
        errdefer params.deinit(self.allocator);

        if (!self.check(.right_paren)) {
            while (true) {
                if (self.check(.variable)) {
                    const param_token = self.advance();
                    const param_name = if (param_token.lexeme.len > 0 and param_token.lexeme[0] == '!')
                        param_token.lexeme[1..]
                    else
                        param_token.lexeme;
                    params.append(self.allocator, param_name) catch return ParseError.OutOfMemory;
                } else {
                    return ParseError.ExpectedVariable;
                }
                if (!self.match(&.{.comma})) break;
            }
        }

        if (!self.match(&.{.right_paren})) {
            return ParseError.ExpectedRightParen;
        }

        if (!self.match(&.{.arrow})) {
            return ParseError.UnexpectedToken;
        }

        const body = try self.expression();

        const arrow = self.allocator.create(ast.ArrowFn) catch return ParseError.OutOfMemory;
        arrow.* = ast.ArrowFn{
            .params = params.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory,
            .body = body,
        };

        return ast.Expr{ .arrow_fn = arrow };
    }

    // ============ HELPERS ============

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
    fn unescapeString(self: *Parser, input: []const u8) ParseError![]const u8 {
        var needs_unescaping = false;
        for (input) |c| {
            if (c == '\\') {
                needs_unescaping = true;
                break;
            }
        }

        if (!needs_unescaping) return input;

        var output = std.ArrayListUnmanaged(u8){};

        var i: usize = 0;
        while (i < input.len) {
            const c = input[i];
            if (c == '\\') {
                if (i + 1 < input.len) {
                    const next = input[i + 1];
                    switch (next) {
                        'n' => try output.append(self.allocator, '\n'),
                        'r' => try output.append(self.allocator, '\r'),
                        't' => try output.append(self.allocator, '\t'),
                        '\\' => try output.append(self.allocator, '\\'),
                        '"' => try output.append(self.allocator, '"'),
                        '\'' => try output.append(self.allocator, '\''),
                        else => {
                            try output.append(self.allocator, next);
                        },
                    }
                    i += 2;
                } else {
                    try output.append(self.allocator, '\\');
                    i += 1;
                }
            } else {
                try output.append(self.allocator, c);
                i += 1;
            }
        }

        return output.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory;
    }
};

// ============ TESTS ============

test "parser - simple echo" {
    const Lexer = @import("lexer.zig").Lexer;
    const allocator = std.testing.allocator;

    const source = "ec \"Hello\";";
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    var program = try parser.parse();
    defer program.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
}

test "parser - variable declaration" {
    const Lexer = @import("lexer.zig").Lexer;
    const allocator = std.testing.allocator;

    const source = "var !nama = \"Amal\";";
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    var program = try parser.parse();
    defer program.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    const stmt = program.statements[0];
    switch (stmt) {
        .var_decl => |v| {
            try std.testing.expectEqualStrings("nama", v.name);
            try std.testing.expectEqual(ast.VarKind.var_mutable, v.kind);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "parser - arithmetic expression" {
    const Lexer = @import("lexer.zig").Lexer;
    const allocator = std.testing.allocator;

    const source = "ec 1 + 2 * 3;";
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    var program = try parser.parse();
    defer program.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
}

test "parser - if statement" {
    const Lexer = @import("lexer.zig").Lexer;
    const allocator = std.testing.allocator;

    const source = "if !x < 10: ec \"kecil\";";
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    var program = try parser.parse();
    defer program.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    const stmt = program.statements[0];
    switch (stmt) {
        .if_stmt => |i| {
            try std.testing.expect(i.else_branch == null);
        },
        else => return error.TestUnexpectedResult,
    }
}
