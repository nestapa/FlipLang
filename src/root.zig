const std = @import("std");

pub fn bufferedPrint() !void {
    const stdout = std.fs.File.stdout().writer();
    try stdout.print("FlipLang Library\n", .{});
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add" {
    try std.testing.expect(add(3, 7) == 10);
}
