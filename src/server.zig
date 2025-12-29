const std = @import("std");
const Runner = @import("runner.zig").Runner;
const cgi = @import("cgi.zig");

const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Object = value_mod.Object;

pub const Server = struct {
    allocator: std.mem.Allocator,
    port: u16,
    root_dir: []const u8,

    pub fn init(allocator: std.mem.Allocator, port: u16, root_dir: []const u8) Server {
        return Server{
            .allocator = allocator,
            .port = port,
            .root_dir = root_dir,
        };
    }

    pub fn start(self: *Server) !void {
        const ws = std.os.windows.ws2_32;
        const sockfd = ws.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
        if (sockfd == ws.INVALID_SOCKET) return error.ConnectionFailed;
        defer _ = ws.closesocket(sockfd);

        // Allow address reuse
        // Windows socket constants not in Zig 0.16 std library
        const SOL_SOCKET: i32 = 0xffff;
        const SO_REUSEADDR: i32 = 0x0004;
        const on: i32 = 1;
        _ = ws.setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, @ptrCast(&on), @sizeOf(i32));

        var addr = std.posix.sockaddr.in{
            .family = std.posix.AF.INET,
            .port = std.mem.nativeToBig(u16, self.port),
            .addr = 0, // INADDR_ANY
        };

        if (ws.bind(sockfd, @ptrCast(&addr), @sizeOf(std.posix.sockaddr.in)) == ws.SOCKET_ERROR) {
            std.debug.print("Error: Could not bind to port {d}. It might still be in use.\n", .{self.port});
            return error.BindFailed;
        }
        if (ws.listen(sockfd, 128) == ws.SOCKET_ERROR) {
            return error.ListenFailed;
        }

        std.debug.print("FlipLang Server running at http://127.0.0.1:{d}\n", .{self.port});
        std.debug.print("Serving directory: {s}\n", .{self.root_dir});

        while (true) {
            var client_addr: std.posix.sockaddr.in = undefined;
            var client_addr_len: i32 = @sizeOf(std.posix.sockaddr.in);

            const client_fd = ws.accept(sockfd, @ptrCast(&client_addr), &client_addr_len);
            if (client_fd == ws.INVALID_SOCKET) continue;

            self.handleConnection(client_fd) catch |err| {
                std.debug.print("Error handling connection: {}\n", .{err});
            };
        }
    }

    fn handleConnection(self: *Server, fd: std.os.windows.ws2_32.SOCKET) !void {
        const ws = std.os.windows.ws2_32;
        defer _ = ws.closesocket(fd);

        var buffer: [4096]u8 = undefined;
        const bytes_read_i32 = ws.recv(fd, &buffer, @intCast(buffer.len), 0);
        if (bytes_read_i32 <= 0) return;
        const bytes_read = @as(usize, @intCast(bytes_read_i32));

        const request_raw = buffer[0..bytes_read];

        // Simple HTTP Request Parsing
        var line_it = std.mem.splitSequence(u8, request_raw, "\r\n");
        const request_line = line_it.first();

        var req_parts = std.mem.splitSequence(u8, request_line, " ");
        const method = req_parts.next() orelse "GET";
        const path_query = req_parts.next() orelse "/";

        // Split path and query
        var path_it = std.mem.splitScalar(u8, path_query, '?');
        const path = path_it.next() orelse "/";
        const query_string = path_it.next() orelse "";

        // Determine file path
        // Remove leading slash
        const rel_path = if (std.mem.startsWith(u8, path, "/")) path[1..] else path;
        const file_path = if (rel_path.len == 0) "index.flip" else rel_path;

        const full_path = try std.fs.path.join(self.allocator, &.{ self.root_dir, file_path });
        defer self.allocator.free(full_path);

        // Check file exists
        std.fs.cwd().access(full_path, .{}) catch {
            // 404
            const response = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot Found";
            _ = ws.send(fd, response.ptr, @intCast(response.len), 0);
            return;
        };

        // Run the script
        var runner = try Runner.init(self.allocator);
        defer runner.deinit();

        // Populate globals
        try self.populateGlobals(&runner, method, path, query_string, request_raw);

        // Run file
        const output = runner.runFile(full_path) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.allocator, "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\n\r\nError: {}", .{err});
            defer self.allocator.free(err_msg);
            _ = ws.send(fd, err_msg.ptr, @intCast(err_msg.len), 0);
            return;
        };

        // Parse Output Headers (CGI-style)
        const status = "200 OK";
        const content_type = "text/html";

        if (std.mem.indexOf(u8, output, "\r\n\r\n")) |_| {
            // Has headers, just prepend status if needed or assume full response
            const response_head = try std.fmt.allocPrint(self.allocator, "HTTP/1.1 {s}\r\n", .{status});
            defer self.allocator.free(response_head);
            _ = ws.send(fd, response_head.ptr, @intCast(response_head.len), 0);
            _ = ws.send(fd, output.ptr, @intCast(output.len), 0);
        } else {
            // If no double newline, try \n\n
            if (std.mem.indexOf(u8, output, "\n\n")) |_| {
                const response_head = try std.fmt.allocPrint(self.allocator, "HTTP/1.1 {s}\r\n", .{status});
                defer self.allocator.free(response_head);
                _ = ws.send(fd, response_head.ptr, @intCast(response_head.len), 0);
                _ = ws.send(fd, output.ptr, @intCast(output.len), 0);
            } else {
                // No headers found, assume raw body
                const header = try std.fmt.allocPrint(self.allocator, "HTTP/1.1 200 OK\r\nContent-Type: {s}\r\nContent-Length: {d}\r\n\r\n", .{ content_type, output.len });
                defer self.allocator.free(header);
                _ = ws.send(fd, header.ptr, @intCast(header.len), 0);
                _ = ws.send(fd, output.ptr, @intCast(output.len), 0);
            }
        }
    }

    fn populateGlobals(self: *Server, runner: *Runner, method: []const u8, path: []const u8, query: []const u8, request_raw: []const u8) !void {
        _ = self;

        const allocator = runner.interpreter.arena.allocator();
        const globals = runner.interpreter.globals;

        // !_SERVER
        const server_obj = try allocator.create(Object);
        server_obj.* = Object.init();
        try server_obj.set(allocator, "REQUEST_METHOD", Value{ .string = try allocator.dupe(u8, method) });
        try server_obj.set(allocator, "REQUEST_URI", Value{ .string = try allocator.dupe(u8, path) });
        try server_obj.set(allocator, "QUERY_STRING", Value{ .string = try allocator.dupe(u8, query) });
        try globals.define("_SERVER", Value{ .object = server_obj });

        // !_GET
        const get_obj = try allocator.create(Object);
        get_obj.* = Object.init();

        var q_it = std.mem.splitScalar(u8, query, '&');
        while (q_it.next()) |pair| {
            if (pair.len == 0) continue;
            var pair_it = std.mem.splitScalar(u8, pair, '=');
            const key_raw = pair_it.next() orelse continue;
            const val_raw = pair_it.next() orelse "";
            const key = try cgi.urlDecode(allocator, key_raw);
            const val = try cgi.urlDecode(allocator, val_raw);
            try get_obj.set(allocator, key, Value{ .string = val });
        }
        try globals.define("_GET", Value{ .object = get_obj });

        // !_POST (Naive implementation - only x-www-form-urlencoded basic)
        const post_obj = try allocator.create(Object);
        post_obj.* = Object.init();

        if (std.mem.eql(u8, method, "POST")) {
            if (std.mem.indexOf(u8, request_raw, "\r\n\r\n")) |body_start| {
                const body = request_raw[body_start + 4 ..];
                // trim nulls from buffer end if any
                var real_body = body; // simplified
                if (std.mem.indexOfScalar(u8, body, 0)) |null_idx| {
                    real_body = body[0..null_idx];
                }

                var p_it = std.mem.splitScalar(u8, real_body, '&');
                while (p_it.next()) |pair| {
                    if (pair.len == 0) continue;
                    var pair_it = std.mem.splitScalar(u8, pair, '=');
                    const key_raw = pair_it.next() orelse continue;
                    const val_raw = pair_it.next() orelse "";
                    const key = try cgi.urlDecode(allocator, key_raw);
                    const val = try cgi.urlDecode(allocator, val_raw);
                    try post_obj.set(allocator, key, Value{ .string = val });
                }
            }
        }
        try globals.define("_POST", Value{ .object = post_obj });

        // Define !_env for backward compatibility if needed, but not standard PHP
    }
};
