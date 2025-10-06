const std = @import("std");
const advent = @import("advent.zig");

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !u32 {
    var input = try advent.DayOfAdvent.init(allocator, "2020", "day6", variant);
    defer input.deinit();

    var buffer: [512]u8 = undefined;
    var reader = input.reader();

    var results = std.AutoHashMap(u8, void).init(allocator);
    defer results.deinit();

    var count: u32 = 0;
    while (try reader.readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        if (line.len == 0) {
            count += results.count();
            results.clearRetainingCapacity();
            continue;
        }
        for (line[0..]) |c| {
            try results.put(c, {});
        }
    }
    count += results.count();

    return count;
}
pub fn part2(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    var input = try advent.DayOfAdvent.init(allocator, "2020", "day6", variant);
    defer input.deinit();

    var buffer: [512]u8 = undefined;
    var reader = input.reader();

    var result: i32 = 0;

    var groupAnswers = std.AutoHashMap(u8, u8).init(allocator);
    defer groupAnswers.deinit();
    var groupSize: i32 = 0;

    while (try reader.readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        if (line.len == 0) {
            var it = groupAnswers.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.* == groupSize) {
                    result += 1;
                }
            }
            groupAnswers.clearRetainingCapacity();
            groupSize = 0;
            continue;
        }
        groupSize += 1;
        for (line[0..]) |c| {
            const count = groupAnswers.get(c) orelse 0;
            try groupAnswers.put(c, count + 1);
        }
    }
    var it = groupAnswers.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == groupSize) {
            result += 1;
        }
    }

    return result;
}

test "Test example" {
    try std.testing.expectEqual(11, part1(std.testing.allocator, "example.txt"));
    try std.testing.expectEqual(6, part2(std.testing.allocator, "example.txt"));
}

test "part 1" {
    try std.testing.expectEqual(6534, part1(std.testing.allocator, "input.txt"));
}

test "part 2" {
    try std.testing.expectEqual(3402, part2(std.testing.allocator, "input.txt"));
}
