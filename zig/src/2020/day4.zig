const std = @import("std");
const advent = @import("advent.zig");

const Field = struct { name: []const u8, validation: *const fn (value: []const u8) bool };
const FIELDS = [_] Field{
    .{ .name = "byr", .validation = &validBYR },
    .{ .name = "iyr", .validation = &validIYR },
    .{ .name = "eyr", .validation = &validEYR },
    .{ .name = "hgt", .validation = &validHGT },
    .{ .name = "hcl", .validation = &validHCL },
    .{ .name = "ecl", .validation = &validECL },
    .{ .name = "pid", .validation = &validPID },
};
const VALID_SET = std.bit_set.IntegerBitSet(7).initFull();

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) anyerror!i32 {
    return solve(allocator, variant, &setRequiredField);
}
pub fn part2(allocator: std.mem.Allocator, variant: []const u8) anyerror!i32 {
    return solve(allocator, variant, &setValidField);
}
pub fn solve(
    allocator: std.mem.Allocator,
    variant: []const u8,
    rules: *const fn(validation: *std.bit_set.IntegerBitSet(7), name: []const u8, value: []const u8) void
) anyerror!i32 {
    var day = try advent.DayOfAdvent.init(allocator, "2020", "day4", variant);
    defer day.deinit();

    var buffer: [2048]u8 = undefined;
    var countOfValids: i32 = 0;
    var validity: std.bit_set.IntegerBitSet(7) = std.bit_set.IntegerBitSet(7).initEmpty();

    while (try day.reader().readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        if (line.len == 0) {
            if (validity.eql(VALID_SET)) {
                countOfValids += 1;
            }
            validity.mask = 0;
            continue;
        }
        try setFieldsValidity(&validity, rules, line);
    }
    if (validity.eql(VALID_SET)) {
        countOfValids += 1;
    }
    return countOfValids;
}

test "Test sample data" {
    try std.testing.expectEqual(2, part1(std.testing.allocator, "example.txt"));
    try std.testing.expectEqual(2, part1(std.testing.allocator, "example.txt"));
}
test "Test part 1" {
    try std.testing.expectEqual(210, part1(std.testing.allocator, "input.txt"));
}
test "Test part 2" {
    try std.testing.expectEqual(131, part2(std.testing.allocator, "input.txt"));
}


fn setFieldsValidity(
    validation: *std.bit_set.IntegerBitSet(7),
    rules: *const fn(validation: *std.bit_set.IntegerBitSet(7), name: []const u8, value: []const u8) void,
    line: []const u8
) !void {
    var fields = std.mem.splitScalar(u8, line, ' ');
    while (fields.next()) |field| {
        var kv = std.mem.splitScalar(u8, field, ':');
        const name = kv.next() orelse return error.InvalidSyntax;
        const value = kv.next() orelse return error.InvalidSyntax;
        rules(validation, name, value);
    }
}
fn setRequiredField(validation: *std.bit_set.IntegerBitSet(7), name: []const u8, value: []const u8) void {
    if (value.len == 0) {
        return;
    }
    for (FIELDS[0..], 0..) |f, idx| {
        if (std.mem.eql(u8, f.name, name)) {
            validation.set(idx);
        }
    }
}
fn setValidField(validation: *std.bit_set.IntegerBitSet(7), name: []const u8, value: []const u8) void {
    if (value.len == 0) {
        return;
    }
    for (FIELDS[0..], 0..) |f, idx| {
        if (std.mem.eql(u8, f.name, name) and f.validation(value)) {
            validation.set(idx);
        }
    }
}
fn validYear(min: i32, max: i32, value: []const u8) bool {
    if (value.len != 4) {
        return false;
    }
    const year = std.fmt.parseInt(i32, value, 10) catch return false;
    return year >= min and year <= max;
}
fn validBYR(value: []const u8) bool {
    return validYear(1920, 2002, value);
}
test "valid birth year" {
    try std.testing.expect(!validBYR("1919"));
    try std.testing.expect(validBYR("1920"));
    try std.testing.expect(validBYR("1921"));
    try std.testing.expect(validBYR("2001"));
    try std.testing.expect(validBYR("2002"));
    try std.testing.expect(!validBYR("2003"));
}
fn validIYR(value: []const u8) bool {
    return validYear(2010, 2020, value);
}
fn validEYR(value: []const u8) bool {
    return validYear(2020, 2030, value);
}
fn validHGT(value: []const u8) bool {
    if (value.len <= 3) {
        return false;
    }
    const unit = value[(value.len - 2)..];
    const n = std.fmt.parseInt(i32, value[0..(value.len - 2)], 10) catch return false;
    if (std.mem.eql(u8, unit, "cm")) {
        return n >= 150 and n <= 193;
    } else if (std.mem.eql(u8, unit, "in")) {
        return n >= 59 and n <= 76;
    } else {
        return false;
    }
}
test "valid height" {
    try std.testing.expect(validHGT("60in"));
    try std.testing.expect(validHGT("190cm"));
    try std.testing.expect(!validHGT("190in"));
    try std.testing.expect(!validHGT("190"));
}
fn validHCL(value: []const u8) bool {
    if (value.len != 7 or value[0] != '#') {
        return false;
    }
    _ = std.fmt.parseInt(i32, value[1..], 16) catch return false;
    return true;
}
test "valid hcl" {
    try std.testing.expect(validHCL("#123abc"));
    try std.testing.expect(!validHCL("#123abz"));
    try std.testing.expect(!validHCL("123abc"));
}
fn validECL(value: []const u8) bool {
    return std.mem.eql(u8, value, "amb") or
        std.mem.eql(u8, value, "blu") or
        std.mem.eql(u8, value, "brn") or
        std.mem.eql(u8, value, "gry") or
        std.mem.eql(u8, value, "grn") or
        std.mem.eql(u8, value, "hzl") or
        std.mem.eql(u8, value, "oth");
}
test "valid ecl" {
    try std.testing.expect(validECL("brn"));
    try std.testing.expect(!validECL("wat"));
}
fn validPID(value: []const u8) bool {
    if (value.len != 9) {
        return false;
    }
    _ = std.fmt.parseInt(i32, value, 10) catch return false;
    return true;
}
test "valid pid" {
    try std.testing.expect(validPID("000000001"));
    try std.testing.expect(!validPID("0123456789"));
}

test "Validate missing data" {
    var validation = std.bit_set.IntegerBitSet(7).initEmpty();
    try std.testing.expectEqual(0, validation.mask);

    setRequiredField(&validation, "byr", "");
    try std.testing.expectEqual(0, validation.mask);

    setRequiredField(&validation, "byr", " ");
    try std.testing.expect(validation.isSet(0));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "iyr", " ");
    try std.testing.expect(validation.isSet(1));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "eyr", " ");
    try std.testing.expect(validation.isSet(2));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "hgt", " ");
    try std.testing.expect(validation.isSet(3));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "hcl", " ");
    try std.testing.expect(validation.isSet(4));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "ecl", " ");
    try std.testing.expect(validation.isSet(5));
    try std.testing.expect(!validation.eql(VALID_SET));

    setRequiredField(&validation, "pid", " ");
    try std.testing.expect(validation.isSet(6));
    try std.testing.expectEqual(VALID_SET, validation);
}
