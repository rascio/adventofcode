const std = @import("std");
const advent = @import("advent.zig");

pub fn part1(allocator: std.mem.Allocator, variant: []const u8) !i32 {
    var input = try advent.DayOfAdvent.init(allocator, "2020", "day7", variant);
    defer input.deinit();

    const reader = input.reader();

    var rules = try readRules(allocator, reader);
    defer {
        for (rules.items) |r| r.deinit();
        rules.deinit();
    }
    var parents = std.StringHashMap(std.ArrayList([]const u8)).init(allocator);
    defer {
        var it = parents.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        parents.deinit();
    }
    for (rules.items) |rule| {
        for (rule.contains) |bag| {
            const result = try parents.getOrPut(bag.bag);
            if (!result.found_existing) {
                result.value_ptr.* = std.ArrayList([]const u8).init(allocator);
            }
            try result.value_ptr.append(rule.bag);
        }
    }

    return -1;
}

fn readRules(allocator: std.mem.Allocator, reader: anytype) !std.ArrayList(*const Rule) {
    var rules = std.ArrayList(*const Rule).init(allocator);
    var buffer: [512]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(buffer[0..], '\n')) |line| {
        const rule = try Rule.parse(allocator, line);
        try rules.append(&rule);
    }
    return rules;
}
test "Test example" {
    try std.testing.expectEqual(-1, part1(std.testing.allocator, "example.txt"));
}

const ContainedBags = struct {
    n: u8,
    bag:[] const u8,

    pub fn format(
        self: ContainedBags,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("ContainedBag[.n={d}, .bag={s}]", .{self.n, self.bag});
    }
};
const Rule = struct {
    bag: []const u8,
    contains: []const ContainedBags,
    allocator: std.mem.Allocator,

    fn deinit(self: *const Rule) void {
        for (self.contains) |i| {
            self.allocator.free(i.bag);
        }
        self.allocator.free(self.contains);
        self.allocator.free(self.bag);
    }

    fn parse(allocator: std.mem.Allocator, line: []const u8) !Rule {
        var tokens = std.mem.splitScalar(u8, line, ' ');
        var contains = std.ArrayList(ContainedBags).init(allocator);
        defer contains.deinit();

        const bag_id: []const u8 = parseBagId(allocator, &tokens) catch return error.BagIdParsingError;

        _ = tokens.next() orelse return error.MissingTokenBags;
        _ = tokens.next() orelse return error.MissingTokenContain;

        while (tokens.next()) |number_token| {
            if (std.mem.eql(u8, number_token, "no")) {
                break;
            } else {
                const nested_bag_id = parseBagId(allocator, &tokens) catch return error.NestedBagIdError;
                const n = try std.fmt.parseInt(u8, number_token, 10);
                try contains.append(.{.n = n, .bag = nested_bag_id});
                _ = tokens.next() orelse return error.MissingTokenBags;
            }
        }
        return .{.bag = bag_id, .contains = try contains.toOwnedSlice(), .allocator = allocator};
     }
     fn parseBagId(allocator: std.mem.Allocator, tokens: *std.mem.SplitIterator(u8, std.mem.DelimiterType.scalar)) ![]const u8 {
         const attr_token = tokens.next() orelse return error.MissingBagAttr;
         const color_token = tokens.next() orelse return error.MissingBagColor;
         return std.mem.concat(allocator, u8, &[_][]const u8{
             attr_token,
             color_token
         });
     }
};
