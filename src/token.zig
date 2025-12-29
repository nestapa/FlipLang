const std = @import("std");

pub const TokenType = enum {
    number,
    string,
    true_literal,
    false_literal,
    null_literal,
    identifier,
    variable,
    kw_ec,
    kw_if,
    kw_else,
    kw_for,
    kw_while,
    kw_foreach,
    kw_as,
    kw_function,
    kw_fn,
    kw_return,
    kw_var,
    kw_let,
    kw_const,
    kw_switch,
    kw_case,
    kw_break,
    kw_continue,
    kw_default,
    kw_try,
    kw_catch,
    kw_push,
    kw_pop,
    kw_map,
    kw_filter,
    kw_class,
    kw_new,
    kw_init,
    kw_this,
    kw_use,
    plus,
    minus,
    star,
    slash,
    percent,
    equal,
    plus_equal,
    minus_equal,
    star_equal,
    slash_equal,
    equal_equal,
    bang_equal,
    greater,
    less,
    greater_equal,
    less_equal,
    ampersand_ampersand,
    pipe_pipe,
    bang,
    plus_plus,
    minus_minus,
    question,
    arrow,
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    left_bracket,
    right_bracket,
    semicolon,
    colon,
    comma,
    dot,
    template_if_open,
    template_expr_open,
    template_close,
    eof,
    invalid,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, line: usize, column: usize) Token {
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .line = line,
            .column = column,
        };
    }

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Token({s}, \"{s}\", {}:{})", .{
            @tagName(self.type),
            self.lexeme,
            self.line,
            self.column,
        });
    }
};

pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "ec", .kw_ec },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "for", .kw_for },
    .{ "while", .kw_while },
    .{ "foreach", .kw_foreach },
    .{ "as", .kw_as },
    .{ "function", .kw_function },
    .{ "fn", .kw_fn },
    .{ "return", .kw_return },
    .{ "var", .kw_var },
    .{ "let", .kw_let },
    .{ "const", .kw_const },
    .{ "switch", .kw_switch },
    .{ "case", .kw_case },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "default", .kw_default },
    .{ "try", .kw_try },
    .{ "catch", .kw_catch },
    .{ "class", .kw_class },
    .{ "new", .kw_new },
    .{ "init", .kw_init },
    .{ "this", .kw_this },
    .{ "use", .kw_use },
    .{ "true", .true_literal },
    .{ "false", .false_literal },
    .{ "null", .null_literal },
});

pub fn lookupKeyword(ident: []const u8) TokenType {
    return keywords.get(ident) orelse .identifier;
}

test "token" {
    const tok = Token.init(.kw_ec, "ec", 1, 5);
    try std.testing.expectEqual(TokenType.kw_ec, tok.type);
}
