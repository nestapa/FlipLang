const std = @import("std");

pub const MySQL = struct {
    pub const Error = error{
        ConnectionFailed,
        AuthFailed,
        ProtocolError,
        QueryFailed,
        OutOfMemory,
        ReadError,
        WriteError,
    };

    fn connectSocket(address: []const u8, port: u16) !std.os.windows.ws2_32.SOCKET {
        const ws = std.os.windows.ws2_32;
        _ = address;

        const sockfd = ws.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
        if (sockfd == ws.INVALID_SOCKET) return error.ConnectionFailed;
        errdefer _ = ws.closesocket(sockfd);

        const port_be = std.mem.nativeToBig(u16, port);
        var addr = std.posix.sockaddr.in{
            .family = std.posix.AF.INET,
            .port = port_be,
            .addr = 0x0100007F,
        };

        if (ws.connect(sockfd, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in)) == ws.SOCKET_ERROR) {
            return error.ConnectionFailed;
        }
        return sockfd;
    }

    pub fn connect(allocator: std.mem.Allocator, host: []const u8, user: []const u8, pass: []const u8, db: []const u8) !std.os.windows.ws2_32.SOCKET {
        _ = allocator;
        _ = user;
        _ = pass;
        _ = db;
        const fd = try connectSocket(host, 3306);

        var buffer: [1024]u8 = undefined;
        try readFull(fd, buffer[0..4]);
        const packet_len = @as(u32, buffer[0]) | (@as(u32, buffer[1]) << 8) | (@as(u32, buffer[2]) << 16);
        const seq_id = buffer[3];
        _ = seq_id;

        const body = buffer[0..packet_len];
        try readFull(fd, body);

        return fd;
    }

    pub fn query(fd: std.os.windows.ws2_32.SOCKET, sql: []const u8) !void {
        const ws = std.os.windows.ws2_32;
        const len = 1 + sql.len;
        var header: [4]u8 = undefined;
        header[0] = @truncate(len);
        header[1] = @truncate(len >> 8);
        header[2] = @truncate(len >> 16);
        header[3] = 0;

        _ = ws.send(fd, &header, 4, 0);

        const cmd_byte: [1]u8 = .{3};
        _ = ws.send(fd, &cmd_byte, 1, 0);
        _ = ws.send(fd, sql.ptr, @intCast(sql.len), 0);

        var resp_header: [4]u8 = undefined;
        try readFull(fd, &resp_header);

        const resp_len = @as(u32, resp_header[0]) | (@as(u32, resp_header[1]) << 8) | (@as(u32, resp_header[2]) << 16);
        if (resp_len > 0) {
            var remaining = resp_len;
            var junk: [1024]u8 = undefined;
            while (remaining > 0) {
                const to_read = @min(remaining, junk.len);
                try readFull(fd, junk[0..to_read]);
                remaining -= to_read;
            }
        }
    }

    fn readFull(fd: std.os.windows.ws2_32.SOCKET, buf: []u8) !void {
        const ws = std.os.windows.ws2_32;
        var total_read: usize = 0;
        while (total_read < buf.len) {
            const n = ws.recv(fd, buf[total_read..].ptr, @intCast(buf.len - total_read), 0);
            if (n <= 0) return error.ReadError;
            total_read += @intCast(n);
        }
    }
};
