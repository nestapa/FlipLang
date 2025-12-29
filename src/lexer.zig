const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

/// FlipLang Lexer
/// Converts source code into a stream of tokens
pub const Lexer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    line_start: usize,
    start_line: usize,
    start_col: usize,

    /// Initialize a new lexer with source code
    pub fn init(source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .column = 1,
            .line_start = 0,
            .start_line = 1,
            .start_col = 1,
        };
    }

    /// Check if we've reached the end of source
    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    /// Get the current character without advancing
    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    /// Get the next character without advancing
    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    /// Advance and return the current character
    fn advance(self: *Lexer) u8 {
        const c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        return c;
    }

    /// Check if current char matches expected, then advance
    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        self.column += 1;
        return true;
    }

    /// Skip whitespace and comments
    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    self.line_start = self.current + 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // Single-line comment
                        while (!self.isAtEnd() and self.peek() != '\n') {
                            _ = self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        // Multi-line comment
                        _ = self.advance(); // consume /
                        _ = self.advance(); // consume *
                        while (!self.isAtEnd()) {
                            if (self.peek() == '*' and self.peekNext() == '/') {
                                _ = self.advance(); // consume *
                                _ = self.advance(); // consume /
                                break;
                            }
                            if (self.peek() == '\n') {
                                self.line += 1;
                                self.column = 0;
                                self.line_start = self.current + 1;
                            }
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    /// Create a token with the current lexeme
    fn makeToken(self: *Lexer, token_type: TokenType) Token {
        const lexeme = self.source[self.start..self.current];
        return Token.init(token_type, lexeme, self.start_line, self.start_col);
    }

    /// Scan a string literal
    fn scanString(self: *Lexer, quote: u8) Token {
        while (!self.isAtEnd() and self.peek() != quote) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
                self.line_start = self.current + 1;
            }
            // Handle escape sequences
            if (self.peek() == '\\' and !self.isAtEnd()) {
                _ = self.advance(); // skip backslash
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeToken(.invalid); // Unterminated string
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.string);
    }

    /// Scan a number literal
    fn scanNumber(self: *Lexer) Token {
        while (!self.isAtEnd() and isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for decimal part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume .
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.number);
    }

    /// Scan an identifier or keyword
    fn scanIdentifier(self: *Lexer) Token {
        while (!self.isAtEnd() and (isAlphaNumeric(self.peek()))) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];
        const token_type = token.lookupKeyword(text);
        return self.makeToken(token_type);
    }

    /// Scan a FlipLang variable (!identifier)
    fn scanVariable(self: *Lexer) Token {
        // Skip the ! and scan the identifier
        while (!self.isAtEnd() and isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.variable);
    }

    /// Scan the next token
    pub fn scanToken(self: *Lexer) Token {
        self.skipWhitespace();

        self.start = self.current;
        self.start_line = self.line;
        self.start_col = self.column;

        if (self.isAtEnd()) return self.makeToken(.eof);

        const c = self.advance();

        // Check for FlipLang variable (!identifier)
        if (c == '!' and isAlpha(self.peek())) {
            return self.scanVariable();
        }

        // Identifiers and keywords
        if (isAlpha(c)) return self.scanIdentifier();

        // Numbers
        if (isDigit(c)) return self.scanNumber();

        // Single and multi-character tokens
        return switch (c) {
            // Strings
            '"', '\'' => self.scanString(c),

            // Single character tokens
            '(' => self.makeToken(.left_paren),
            ')' => self.makeToken(.right_paren),
            '{' => self.makeToken(.left_brace),
            '}' => self.makeToken(.right_brace),
            '[' => blk: {
                // Check for template syntax
                if (self.peek() == 'f' and self.peekNext() == '>') {
                    _ = self.advance(); // f
                    _ = self.advance(); // >
                    break :blk self.makeToken(.template_if_open);
                } else if (self.peek() == '=' and self.peekNext() == '>') {
                    _ = self.advance(); // =
                    _ = self.advance(); // >
                    break :blk self.makeToken(.template_expr_open);
                }
                break :blk self.makeToken(.left_bracket);
            },
            ']' => self.makeToken(.right_bracket),
            ';' => self.makeToken(.semicolon),
            ':' => self.makeToken(.colon),
            ',' => self.makeToken(.comma),
            '.' => self.makeToken(.dot),
            '?' => self.makeToken(.question),

            // Operators with possible second character
            '+' => blk: {
                if (self.match('+')) break :blk self.makeToken(.plus_plus);
                if (self.match('=')) break :blk self.makeToken(.plus_equal);
                break :blk self.makeToken(.plus);
            },
            '-' => blk: {
                if (self.match('-')) break :blk self.makeToken(.minus_minus);
                if (self.match('=')) break :blk self.makeToken(.minus_equal);
                break :blk self.makeToken(.minus);
            },
            '*' => blk: {
                if (self.match('=')) break :blk self.makeToken(.star_equal);
                break :blk self.makeToken(.star);
            },
            '/' => blk: {
                if (self.match('=')) break :blk self.makeToken(.slash_equal);
                break :blk self.makeToken(.slash);
            },
            '%' => self.makeToken(.percent),

            '=' => blk: {
                if (self.match('=')) break :blk self.makeToken(.equal_equal);
                if (self.match('>')) break :blk self.makeToken(.arrow);
                break :blk self.makeToken(.equal);
            },
            '!' => blk: {
                if (self.match('=')) break :blk self.makeToken(.bang_equal);
                break :blk self.makeToken(.bang);
            },
            '<' => blk: {
                if (self.match('=')) break :blk self.makeToken(.less_equal);
                if (self.match(']')) break :blk self.makeToken(.template_close);
                break :blk self.makeToken(.less);
            },
            '>' => blk: {
                if (self.match('=')) break :blk self.makeToken(.greater_equal);
                break :blk self.makeToken(.greater);
            },
            '&' => blk: {
                if (self.match('&')) break :blk self.makeToken(.ampersand_ampersand);
                break :blk self.makeToken(.invalid);
            },
            '|' => blk: {
                if (self.match('|')) break :blk self.makeToken(.pipe_pipe);
                break :blk self.makeToken(.invalid);
            },

            else => self.makeToken(.invalid),
        };
    }

    /// Tokenize the entire source and return all tokens as a slice
    pub fn tokenize(self: *Lexer, allocator: std.mem.Allocator) ![]Token {
        var tokens: std.ArrayListUnmanaged(Token) = .empty;
        errdefer tokens.deinit(allocator);

        while (true) {
            const tok = self.scanToken();
            try tokens.append(allocator, tok);
            if (tok.type == .eof) break;
        }

        return tokens.toOwnedSlice(allocator);
    }
};

// ============ HELPER FUNCTIONS ============

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

// ============ TESTS ============

test "lexer - simple tokens" {
    var lexer = Lexer.init("+ - * / ;");

    try std.testing.expectEqual(TokenType.plus, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.minus, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.star, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.slash, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.semicolon, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.eof, lexer.scanToken().type);
}

test "lexer - FlipLang variables" {
    var lexer = Lexer.init("!nama !umur !isActive");

    const tok1 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.variable, tok1.type);
    try std.testing.expectEqualStrings("!nama", tok1.lexeme);

    const tok2 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.variable, tok2.type);
    try std.testing.expectEqualStrings("!umur", tok2.lexeme);

    const tok3 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.variable, tok3.type);
    try std.testing.expectEqualStrings("!isActive", tok3.lexeme);
}

test "lexer - keywords" {
    var lexer = Lexer.init("ec if else for while foreach");

    try std.testing.expectEqual(TokenType.kw_ec, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_if, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_else, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_for, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_while, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_foreach, lexer.scanToken().type);
}

test "lexer - strings" {
    var lexer = Lexer.init("\"hello\" 'world'");

    const tok1 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.string, tok1.type);
    try std.testing.expectEqualStrings("\"hello\"", tok1.lexeme);

    const tok2 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.string, tok2.type);
    try std.testing.expectEqualStrings("'world'", tok2.lexeme);
}

test "lexer - numbers" {
    var lexer = Lexer.init("42 3.14 100");

    const tok1 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.number, tok1.type);
    try std.testing.expectEqualStrings("42", tok1.lexeme);

    const tok2 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.number, tok2.type);
    try std.testing.expectEqualStrings("3.14", tok2.lexeme);

    const tok3 = lexer.scanToken();
    try std.testing.expectEqual(TokenType.number, tok3.type);
    try std.testing.expectEqualStrings("100", tok3.lexeme);
}

test "lexer - operators" {
    var lexer = Lexer.init("== != >= <= && || ++ -- => += -=");

    try std.testing.expectEqual(TokenType.equal_equal, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.bang_equal, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.greater_equal, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.less_equal, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.ampersand_ampersand, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.pipe_pipe, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.plus_plus, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.minus_minus, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.arrow, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.plus_equal, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.minus_equal, lexer.scanToken().type);
}

test "lexer - comments" {
    var lexer = Lexer.init(
        \\// this is a comment
        \\ec "hello";
        \\/* multi
        \\   line */
        \\!x
    );

    try std.testing.expectEqual(TokenType.kw_ec, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.string, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.semicolon, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.variable, lexer.scanToken().type);
}

test "lexer - FlipLang statement" {
    const source = "!nama = \"Amal\"; ec !nama;";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(TokenType.variable, lexer.scanToken().type); // !nama
    try std.testing.expectEqual(TokenType.equal, lexer.scanToken().type); // =
    try std.testing.expectEqual(TokenType.string, lexer.scanToken().type); // "Amal"
    try std.testing.expectEqual(TokenType.semicolon, lexer.scanToken().type); // ;
    try std.testing.expectEqual(TokenType.kw_ec, lexer.scanToken().type); // ec
    try std.testing.expectEqual(TokenType.variable, lexer.scanToken().type); // !nama
    try std.testing.expectEqual(TokenType.semicolon, lexer.scanToken().type); // ;
    try std.testing.expectEqual(TokenType.eof, lexer.scanToken().type);
}

test "lexer - template syntax" {
    var lexer = Lexer.init("[f> if !x: <] [=> !name <]");

    try std.testing.expectEqual(TokenType.template_if_open, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.kw_if, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.variable, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.colon, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.template_close, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.template_expr_open, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.variable, lexer.scanToken().type);
    try std.testing.expectEqual(TokenType.template_close, lexer.scanToken().type);
}
