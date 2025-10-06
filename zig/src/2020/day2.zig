const std = @import("std");
const advent = @import("advent.zig");

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    return try solve(allocator, variant, &validatePasswordV1);
}

pub fn part2(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    return try solve(allocator, variant, &validatePasswordV2);
}

pub fn solve(allocator: std.mem.Allocator, variant: []const u8, validation: *const fn(usize, usize, u8, []const u8) bool) !i32 {
    const input = try advent.readAsString(allocator, "2020", "day2", variant);
    defer allocator.free(input);

    var count: i32 = 0;
    var lines = std.mem.splitSequence(u8, input, "\n");
    while (lines.next()) |line| {
        if (line.len > 0) {
            const minToken = std.mem.sliceTo(line, '-');
            const maxToken = std.mem.sliceTo(line[(minToken.len + 1)..], ' ');
            const char = line[minToken.len + maxToken.len + 2];
            const password = line[minToken.len + maxToken.len + 5 ..];

            const min = try std.fmt.parseInt(usize, minToken, 10);
            const max = try std.fmt.parseInt(usize, maxToken, 10);
            if (validation(min, max, char, password)) {
                // std.debug.print("valid:{s}\n", .{password});
                count += 1;
            }
        }
    }
    return count;
}
fn validatePasswordV1(min: usize, max: usize, char: u8, password: []const u8) bool {
    var count: i32 = 0;
    for (password) |current| {
        if (current == char) {
            if (count == max) {
                return false;
            }
            count += 1;
        }
    }
    return count >= min;
}
fn validatePasswordV2(min: usize, max: usize, char: u8, password: []const u8) bool {
    const firstIsOk: bool = password[min - 1] == char;
    const secondIsOk: bool = password[max - 1] == char;
    return (firstIsOk or secondIsOk) and !(firstIsOk and secondIsOk);
}
test "Solve example" {
    try std.testing.expectEqual(2, part1(std.testing.allocator, "example.txt"));
    try std.testing.expectEqual(1, part2(std.testing.allocator, "example.txt"));
}

test "Solve part1" {
    try std.testing.expectEqual(569, part1(std.testing.allocator, "input.txt"));
}
test "Solve part2" {
    try std.testing.expectEqual(346, part2(std.testing.allocator, "input.txt"));
}
