const std = @import("std");

pub const Preprocessor = struct {
    pub fn process(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
        var output = std.ArrayListUnmanaged(u8){};
        defer output.deinit(allocator);

        var i: usize = 0;
        var in_code_block = false;
        var in_echo_block = false;

        while (i < source.len) {
            if (!in_code_block and !in_echo_block) {
                const remaining = source[i..];
                if (std.mem.startsWith(u8, remaining, "[f>")) {
                    in_code_block = true;
                    i += 3;
                    continue;
                } else if (std.mem.startsWith(u8, remaining, "[=>")) {
                    in_echo_block = true;
                    try output.appendSlice(allocator, "ec(");
                    i += 3;
                    continue;
                }

                const start = i;
                var end = i;
                while (end < source.len) {
                    if (std.mem.startsWith(u8, source[end..], "[f>")) break;
                    if (std.mem.startsWith(u8, source[end..], "[=>")) break;
                    end += 1;
                }

                if (end > start) {
                    const text = source[start..end];
                    try output.appendSlice(allocator, "ec(\"");

                    for (text) |c| {
                        switch (c) {
                            '"' => try output.appendSlice(allocator, "\\\""),
                            '\n' => try output.appendSlice(allocator, "\\n"),
                            '\r' => {},
                            else => try output.append(allocator, c),
                        }
                    }
                    try output.appendSlice(allocator, "\");\n");
                }
                i = end;
            } else {
                const remaining = source[i..];
                if (std.mem.startsWith(u8, remaining, "</]")) {
                    if (in_echo_block) {
                        try output.appendSlice(allocator, ");\n");
                        in_echo_block = false;
                    } else {
                        in_code_block = false;
                    }
                    i += 3;
                    continue;
                }

                try output.append(allocator, source[i]);
                i += 1;
            }
        }

        return output.toOwnedSlice(allocator);
    }
};

test "preprocessor" {
    const allocator = std.testing.allocator;
    const result = try Preprocessor.process(allocator, "<h1>Hi</h1>");
    defer allocator.free(result);
    try std.testing.expect(result.len > 0);
}
