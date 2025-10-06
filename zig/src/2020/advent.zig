//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub fn readFileAsString(allocator: std.mem.Allocator, year: []const u8, day: []const u8, variant: []const u8) ![]u8 {
    const full_name = try std.fs.path.join(allocator, &[_][]const u8{ "src", year, day, variant });
    defer allocator.free(full_name);

    std.debug.print("loading [{s}]\n", .{full_name});
    // Read the entire file at once - much simpler
    return try std.fs.cwd().readFileAlloc(allocator, full_name, std.math.maxInt(usize));
}

pub const DayOfAdvent = struct {
    file: std.fs.File,
    buffered: std.io.BufferedReader(4096, std.fs.File.Reader),

    pub fn init(allocator: std.mem.Allocator, year: []const u8, day: []const u8, variant: []const u8) !DayOfAdvent {
        const full_name = try std.fs.path.join(allocator, &[_][]const u8{ "src", year, day, variant });
        defer allocator.free(full_name);
        std.debug.print("loading [{s}]\n", .{full_name});
        const file = try std.fs.cwd().openFile(full_name, .{});

        return .{
            .file = file,
            .buffered = std.io.bufferedReader(file.reader())
        };
    }

    pub fn deinit(self: *DayOfAdvent) void {
        self.file.close();
    }

    pub fn reader(self: *DayOfAdvent) std.io.BufferedReader(4096, std.fs.File.Reader).Reader {
        return self.buffered.reader();
    }
};
