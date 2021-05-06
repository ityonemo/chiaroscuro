const debug = @import("std").debug;
const testing = @import("std").testing;
const mem = @import("std").mem;

const Module = @import("../module.zig").Module;
const ModuleError = @import("../module.zig").ModuleError;

pub const ExptTable = struct {
    // TODO: need to find a way to reify an "maybe entries" to
    // an "actual entries" table.
    const entries_t = []?Expt;
    entries: entries_t = undefined,
    allocator: *mem.Allocator,

    const Expt = struct {
        function: u32,
        arity: u32,
        label: u32,

        fn parse(allocator: *mem.Allocator, entry_ptr: *?Expt, slice_ptr: *[]const u8) !void {
            var slice = slice_ptr.*;

            // is the slice big enough?
            if (slice.len < 12) return ModuleError.TOO_SHORT;

            entry_ptr.* = Expt{
                .function = Module.little_bytes_to_u32(slice[0..4]),
                .arity = Module.little_bytes_to_u32(slice[4..8]),
                .label = Module.little_bytes_to_u32(slice[8..12]),
            };

            // advance the slice pointer.
            slice_ptr.* = slice[12..];
        }
    };

    /// creates an ExptTable by parsing a slice that begins with a binary
    /// chunk that fits the export table format.
    pub fn parse(allocator: *mem.Allocator, source_ptr: *[]const u8) !ExptTable {
        var source = source_ptr.*; // convenience definition
        // SANITY CHECKS
        debug.assert((source.len & 0x3) == 0);
        debug.assert(mem.eql(u8, source[0..4], "ExpT"));
        // SANITY CHECKS

        // source must be at least 12 bytes to accomodate full header
        if (source.len <= 12) return ModuleError.TOO_SHORT;

        // first 4-byte segment is the "total chunk length"
        var chunk_length: usize = Module.little_bytes_to_usize(source[4..8]);

        // verify that this our source is long enough and is aligned well
        if (chunk_length + 8 > source.len) return ModuleError.TOO_SHORT;
        if ((chunk_length & 0x3) != 0) return ModuleError.BAD_ALIGN;

        defer source_ptr.* = source[8 + chunk_length..];

        // next 4-byte segment is the "total number of exports"
        var expt_count: usize = Module.little_bytes_to_usize(source[8..12]);
        if (chunk_length != expt_count * 12 + 4) return ModuleError.MISMATCHED_SIZE;

        // build a basic entries table.
        var entries = try build_entries(allocator, expt_count);
        errdefer clear_entries(allocator, entries);

        // run a parser over the entries.
        // NB: this might fail on allocation, but that's okay, because
        // we are already clearing all entries in the errdefer statement
        // above.
        var expt_source_ptr = source[12..];
        try parser_loop(allocator, entries, &expt_source_ptr);

        return ExptTable{ .entries = entries, .allocator = allocator };
    }

    /// destroys an ExptTable, cleaning up all dependent entries inside
    /// the table itself.
    pub fn destroy(self: *ExptTable) void {
        clear_entries(self.allocator, self.entries);
    }

    fn build_entries(allocator: *mem.Allocator, count: usize) !entries_t {
        var entries = try allocator.alloc(?Expt, count);
        // intialize the entries with null values.
        for (entries) |*entry| {
            entry.* = null;
        }
        return entries;
    }

    // safely clears entries that have been built, whether or not they
    // contain null values.
    fn clear_entries(allocator: *mem.Allocator, entries: entries_t) void {
        allocator.free(entries);
    }

    fn parser_loop(allocator: *mem.Allocator, entries: entries_t, source: *[]const u8) !void {
        for (entries) |*entry| {
            try Expt.parse(test_allocator, entry, source);
        }
    }
};

// //////////////////////////////////////////////////////////////////////////
// TESTING

const test_allocator = @import("std").testing.allocator;
const assert = @import("std").debug.assert;
var runtime_zero: usize = 0;

test "export parser works on a export binary" {
    const foo_export = [_]u8{ 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47 };
    var dest: ?ExptTable.Expt = undefined;
    var source = foo_export[runtime_zero..];

    try ExptTable.Expt.parse(test_allocator, &dest, &source);

    // check that the parser has moved the source slice to the end.
    assert(source.len == 0);
    // check the parsed value
    assert(dest.?.function == 47);
    assert(dest.?.arity == 47);
    assert(dest.?.label == 47);
}

test "export parser can be attached to a for loop for more than one export" {
    const test_exports = [_]u8{
        0, 0, 0, 7,  0, 0, 0, 8,  0, 0, 0, 9,
        0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0, 12,
    };

    var dest = try ExptTable.build_entries(test_allocator, 2);
    defer ExptTable.clear_entries(test_allocator, dest);

    var source = test_exports[runtime_zero..];
    try ExptTable.parser_loop(test_allocator, dest, &source);

    assert(source.len == 0);
    assert(dest.len == 2);

    assert(dest[0].?.function == 7);
    assert(dest[0].?.arity == 8);
    assert(dest[0].?.label == 9);
    assert(dest[1].?.function == 10);
    assert(dest[1].?.arity == 11);
    assert(dest[1].?.label == 12);
}

// FAILURE PATHS
test "expt parser raises if the data are too short" {
    const incomplete_expt = [_]u8{ 0, 0, 7 };
    var dest: ?ExptTable.Expt = undefined;
    var source = incomplete_expt[runtime_zero..];

    _ = ExptTable.Expt.parse(test_allocator, &dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => 42,
        else => unreachable,
    };
}

test "expt parser loop raises if the data are too short on a second go" {
    const incomplete_exports = [_]u8{ 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0 };
    var dest = try ExptTable.build_entries(test_allocator, 2);
    defer ExptTable.clear_entries(test_allocator, dest);

    var source = incomplete_exports[runtime_zero..];
    ExptTable.parser_loop(test_allocator, dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

// //////////////////////////////////////////////////////////////////////
// TABLE TESTS

test "table parser works on one export value" {
    const table = [_]u8{
        'E', 'x', 'p', 'T', // export table
        0, 0, 0, 16, // length of this table
        0, 0, 0, 1, // number of exports
        0, 0, 0, 7,
        0, 0, 0, 8,
        0, 0, 0, 9,
    };

    var slice = table[runtime_zero..];

    var expt_table = try ExptTable.parse(test_allocator, &slice);
    defer ExptTable.destroy(&expt_table);

    // check that ExptTable has the the meats.
    testing.expectEqual(expt_table.entries.len, 1);
    testing.expectEqual(expt_table.entries[0].?.function, 7);
    testing.expectEqual(expt_table.entries[0].?.arity, 8);
    testing.expectEqual(expt_table.entries[0].?.label, 9);

    // check that the slice has been advanced.
    testing.expectEqual(slice.len, 0);
}

test "table parser works on more than one export value" {
    const table = [_]u8{
        'E', 'x', 'p', 'T',
        0, 0, 0, 28, // length of this table
        0, 0, 0, 2, // number of exports
        0, 0, 0, 7,
        0, 0, 0, 8,
        0, 0, 0, 9,
        0, 0, 0, 10,
        0, 0, 0, 11,
        0, 0, 0, 12,
    };

    var slice = table[runtime_zero..];

    var expt_table = try ExptTable.parse(test_allocator, &slice);
    defer ExptTable.destroy(&expt_table);

    // check that ExptTable has the the meats.
    assert(expt_table.entries.len == 2);

    assert(expt_table.entries[0].?.function == 7);
    assert(expt_table.entries[0].?.arity == 8);
    assert(expt_table.entries[0].?.label == 9);

    assert(expt_table.entries[1].?.function == 10);
    assert(expt_table.entries[1].?.arity == 11);
    assert(expt_table.entries[1].?.label == 12);

    // check that the slice has been advanced.
    assert(slice.len == 0);
}

// FAILURE PATHS
//test "incomplete header fails" {
//    const table = [_]u8{
//        'E', 'x', 'p', 'T',
//        0,   0,   0,
//    }; // incomplete header
//
//    var slice = table[runtime_zero..];
//
//    _ = ExptTable.parse(test_allocator, &slice) catch |err| switch (err) {
//        ModuleError.TOO_SHORT => return,
//        else => unreachable,
//    };
//}

test "nonsensical header fails" {
    const table = [_]u8{
        'E', 'x', 'p', 'T',
        0,   0,   0,   4,
        0,   0,   0,   0,
    }; // size too small to be a header

    var slice = table[runtime_zero..];

    _ = ExptTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "incomplete table fails" {
    const table = [_]u8{
        'E', 'x', 'p', 'T',
        0,   0,   0,   16,
        0,   0,   0,   1,
        0,   0,   0,   0,
    }; // simply not enough data

    var slice = table[runtime_zero..];

    _ = ExptTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "table fails on incorrect value/length combo" {
    const table = [_]u8{
        'E', 'x', 'p', 'T',
        0, 0, 0, 16, // funny length for 2
        0, 0, 0, 2, // number of exports
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
    };
    var slice = table[runtime_zero..];
    _ = ExptTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.MISMATCHED_SIZE => return,
        else => unreachable,
    };
}

// //////////////////////////////////////////////////////////////////////
// MODULE INTEGRATION TESTS

test "module can parse export table" {
    const module_table = [_]u8{
        'F', 'O', 'R', '1', // HEADER
        0,   0,   0,   40,
        'B', 'E', 'A', 'M',
        'E', 'x', 'p', 'T',
        0, 0, 0, 28, // length of this table
        0, 0, 0, 2, // number of exports
        0, 0, 0, 7, // export 1: 7-7-7
        0, 0, 0, 7,
        0, 0, 0, 7,
        0, 0, 0, 8,
        0, 0, 0, 8, // export 2: 8-8-8

        0, 0, 0, 8,
    };

    var module_slice = module_table[runtime_zero..];

    var module = try Module.from_slice(test_allocator, module_slice);
    defer Module.destroy(&module);

    var exports = module.expttable.?.entries;
    assert(exports.len == 2);    assert(module.expttable.?.entries[0].?.function == 7);


    assert(module.expttable.?.entries[0].?.arity == 7);
    assert(module.expttable.?.entries[0].?.label == 7);
    assert(module.expttable.?.entries[1].?.arity == 8);
    assert(module.expttable.?.entries[1].?.function == 8);

    assert(module.expttable.?.entries[1].?.label == 8);
}

test "module fails if the export table is too short" {
    const module_table = [_]u8{
        'F', 'O', 'R', '1', // HEADER
        0,   0,   0,   26,
        'E', 'x', 'p', 'T',
        'B', 'E', 'A', 'M',
        0,   0,   0,   1,
        0,   0,   0,   24,
        0,   0,
    };

    var module_slice = module_table[runtime_zero..];

    _ = Module.from_slice(test_allocator, module_slice) catch |err| switch (err) {
        else => unreachable,
        ModuleError.MISMATCHED_SIZE => return
    };
}
