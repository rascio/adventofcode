const std = @import("std");
const advent = @import("advent.zig");

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    var input = try advent.DayOfAdvent.init(allocator, "2020", "day5", variant);
    defer input.deinit();

    var buffer: [512]u8 = undefined;
    var reader = input.reader();

    var higher: i32 = 0;
    while (try reader.readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        const seat = try getSeat(line);
        const id = seat.id();
        if (id > higher) {
            higher = id;
        }
    }
    return higher;
}

pub fn part2(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    var input = try advent.DayOfAdvent.init(allocator, "2020", "day5", variant);
    defer input.deinit();

    var buffer: [512]u8 = undefined;
    var reader = input.reader();

    var seats = std.AutoHashMap(i32, void).init(allocator);
    defer seats.deinit();

    while (try reader.readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        const seat = try getSeat(line);
        const id = seat.id();
        try seats.put(id, {});
    }
    for (1..126) |r| {
        for (0..7) |c| {
            const id: i32 = @intCast(r * 8 + c);
            if (!seats.contains(id) and seats.contains(id - 1) and seats.contains(id + 1)) {
                return id;
            }
        }
    }
    return -1;
}
const Seat = struct {
    row: u8,
    column: u8,
    fn id(self: Seat) i32 {
        return (@as(i32, self.row) * 8) + self.column;
    }
};
fn getSeat(line: []const u8) !Seat {
    if (line.len != 10) {
        return error.InvalidSyntax;
    }
    return .{.row = binarySpace(line[0..7], 'B', 128), .column = binarySpace(line[7..10], 'R', 8)};
}
fn binarySpace(string: []const u8, upper: u8, space: u8) u8 {
    var min: u8 = 0;
    var max: u8 = space;
    for (string[0..]) |c| {
        min, max = binarySpaceStep(c == upper, min, max);
    }
    return min;
}
test "binarySpace" {
    const row = "FBFBBFF";
    var res = binarySpace(row, 'B', 128);
    try std.testing.expectEqual(44, res);
    const col = "RLR";
    res = binarySpace(col, 'R', 8);
    try std.testing.expectEqual(5, res);
}
fn binarySpaceStep(upperBound: bool, min: u8, max: u8) [2]u8 {
    const half = min + ((max - min) / 2);
    if (upperBound) {
        return .{half, max};
    } else {
        return .{min, half};
    }
}

test "Test example" {
    try std.testing.expectEqual(820, part1(std.testing.allocator, "example.txt"));
}

test "part 1" {
    try std.testing.expectEqual(928, part1(std.testing.allocator, "input.txt"));
}
test "part 2" {
    try std.testing.expectEqual(610, part2(std.testing.allocator, "input.txt"));
}
