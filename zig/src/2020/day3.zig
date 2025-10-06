const std = @import("std");
const advent = @import("advent.zig");

const Tree = '#';
const Open = '.';

fn isTree(map: [][]const u8, position: @Vector(2, usize)) bool {
    const w = map[position[0]].len;
    const c = map[position[0]][position[1] % w];
    return c == Tree;
}

fn countTreesForSlope(map: [][]const u8, slope: @Vector(2, usize)) i32 {
    var position = @Vector(2, usize){0, 0};

    var trees: i32 = 0;
    while (position[0] < map.len) {
        if (isTree(map, position)) {
            trees += 1;
        }
        position += slope;
    }
    return trees;
}

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    const input = try advent.readAsString(allocator, "2020", "day3", variant);
    defer allocator.free(input);

    var lines = std.mem.splitSequence(u8, input, "\n");

    var map = std.ArrayList([]const u8).init(allocator);
    defer map.deinit();

    // Copy the content into a matrix
    var y: usize = 0;
    while (lines.next()) |line| {
        if (line.len > 0) {
            try map.append(line);
            y += 1;
        }
    }
    return countTreesForSlope(map.items, @Vector(2, usize){1, 3});
}

pub fn part2(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    const input = try advent.readAsString(allocator, "2020", "day3", variant);
    defer allocator.free(input);

    var lines = std.mem.splitSequence(u8, input, "\n");

    var map = std.ArrayList([]const u8).init(allocator);
    defer map.deinit();

    // Copy the content into a matrix
    var y: usize = 0;
    while (lines.next()) |line| {
        if (line.len > 0) {
            try map.append(line);
            y += 1;
        }
    }

    const slopes = [_]@Vector(2, usize){
        .{1, 1},
        .{1, 3},
        .{1, 5},
        .{1, 7},
        .{2, 1},
    };

    var trees: i32 = 1;
    for (slopes) |slope| {
        trees *= countTreesForSlope(map.items, slope);
    }
    return trees;
}

test "Test sample data" {
    try std.testing.expectEqual(7, part1(std.testing.allocator, "sample.txt"));
    try std.testing.expectEqual(336, part2(std.testing.allocator, "sample.txt"));
}
test "Part 1" {
    try std.testing.expectEqual(254, part1(std.testing.allocator, "input.txt"));
}
test "Part 2" {
    try std.testing.expectEqual(1666768320, part2(std.testing.allocator, "input.txt"));
}
