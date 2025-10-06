const std = @import("std");
const advent = @import("advent.zig");

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !void {
    const result = try advent.readAsString(allocator, "2020", "day1", variant);
    defer allocator.free(result);

    var items = std.mem.splitSequence(u8, result, "\n");
    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    while (items.next()) |line| {
        if (line.len > 0) {
            try numbers.append(try std.fmt.parseInt(i32, line, 10));
        }
    }

    outer: for (numbers.items, 0..) |a, current| {
        for (numbers.items[(current + 1)..]) |b| {
            if (a + b == 2020) {
                std.debug.print("part1) solved a:{} b:{} => [{}]\n", .{a, b, (a * b)});
                break: outer;
            }
        }
    }
}

pub fn part2(allocator: std.mem.Allocator, variant: []const u8) !void {
    const result = try advent.readAsString(allocator, "2020", "day1", variant);
    defer allocator.free(result);

    var items = std.mem.splitSequence(u8, result, "\n");
    var numbers = std.ArrayList(i32).init(allocator);
    defer numbers.deinit();

    while (items.next()) |line| {
        if (line.len > 0) {
            try numbers.append(try std.fmt.parseInt(i32, line, 10));
        }
    }

    const endBIdx = numbers.items.len - 1;
    outer: for (numbers.items, 0..) |a, idxA| {
        for (numbers.items[(idxA + 1)..endBIdx], (idxA + 1)..) |b, idxB| {
            for (numbers.items[(idxB + 1)..]) |c| {
                if (a + b + c == 2020) {
                    std.debug.print("part2) solved a:{} b:{} c:{} => [{}]\n", .{a, b, c, (a * b * c)});
                    break: outer;
                }
            }
        }
    }
}

test "Example test" {
    try part1(std.testing.allocator, "example.txt");
    try part2(std.testing.allocator, "example.txt");
}

test "solve part1" {
    try part1(std.testing.allocator, "part1.txt");
}
test "solve part2" {
    try part2(std.testing.allocator, "part1.txt");
}
