const debug = @import("std").debug;
const mem = @import("std").mem;

const Module = @import("../module.zig").Module;
const ModuleError = @import("../module.zig").ModuleError;

pub const AtomTable = struct {
    const entries_t = []?[]u8;

    entries: entries_t = undefined,
    allocator: *mem.Allocator,

    const Atom = struct {
        fn parse(allocator: *mem.Allocator, entry_ptr: *?[]u8, slice_ptr: *[]const u8) !void {
            var slice = slice_ptr.*;
            // did we run out of atoms?
            if (slice.len == 0) return ModuleError.TOO_SHORT;

            var size: usize = slice[0];
            // verify that the length is correct
            if (size >= slice.len) return ModuleError.TOO_SHORT;

            entry_ptr.* = try allocator.alloc(u8, size);
            mem.copy(u8, entry_ptr.*.?, slice[1 .. 1 + size]);
            // advance the slice pointer.
            slice_ptr.* = slice[1 + size ..];
        }
    };

    /// creates an AtomTable by parsing a slice that begins with a binary
    /// chunk that fits the atom table format.
    pub fn parse(allocator: *mem.Allocator, source_ptr: *[]const u8) !AtomTable {
        var source = source_ptr.*; // convenience definition
        // source must be at least 12 bytes to accomodate full header
        if (source.len <= 12) return ModuleError.TOO_SHORT;

        // TODO: does this no-op in release-fast?
        // double checks that we are in an atom module.
        debug.assert(mem.eql(u8, source[0..4], "AtU8"));

        // first 4-byte segment is the "total chunk length"
        var chunk_length: usize = Module.little_bytes_to_usize(source[4..8]);

        // verify that this our source is long enough and is aligned well
        if (chunk_length > source.len) return ModuleError.TOO_SHORT;
        if ((chunk_length & 0x3) != 0) return ModuleError.BAD_ALIGN;
        defer source_ptr.* = source[chunk_length..];

        // next 4-byte segment is the "total number of atoms"
        var atom_count: usize = Module.little_bytes_to_usize(source[8..12]);

        // build a basic entries table.
        var entries = try build_entries(allocator, atom_count);
        errdefer clear_entries(allocator, entries);

        // run a parser over the entries.
        // NB: this might fail on allocation, but that's okay, because
        // we are already clearing all entries in the errdefer statement
        // above.
        var atom_source_ptr = source[12..];
        try parser_loop(allocator, entries, &atom_source_ptr);

        return AtomTable{ .entries = entries, .allocator = allocator };
    }

    /// destroys an AtomTable, cleaning up all dependent entries inside
    /// the table itself.
    pub fn destroy(self: *AtomTable) void {
        clear_entries(self.allocator, self.entries);
    }

    fn build_entries(allocator: *mem.Allocator, count: usize) !entries_t {
        var entries = try allocator.alloc(?[]u8, count);
        // intialize the entries with null values.
        for (entries) |*entry| {
            entry.* = null;
        }
        return entries;
    }

    // safely clears entries that have been built, whether or not they
    // contain null values.
    fn clear_entries(allocator: *mem.Allocator, entries: entries_t) void {
        for (entries) |entry| {
            if (entry) |safe_entry| {
                allocator.free(safe_entry);
            }
        }
        allocator.free(entries);
    }

    fn parser_loop(allocator: *mem.Allocator, entries: entries_t, source: *[]const u8) !void {
        for (entries) |*entry| {
            try Atom.parse(test_allocator, entry, source);
        }
    }
};

// //////////////////////////////////////////////////////////////////////////
// TESTING

const test_allocator = @import("std").testing.allocator;
const assert = @import("std").debug.assert;
var runtime_zero: usize = 0;

test "atom parser works on a single atom" {
    const foo_atom = [_]u8{ 3, 'f', 'o', 'o' };
    var dest: ?[]u8 = undefined;
    var source = foo_atom[runtime_zero..];

    try AtomTable.Atom.parse(test_allocator, &dest, &source);
    defer test_allocator.free(dest.?);

    // check that the parser has moved the source slice to the end.
    assert(source.len == 0);
    assert(mem.eql(u8, dest.?, "foo"));
}

test "atom parser can be attached to a for loop for more than one atom" {
    const test_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u', 'x' };
    var dest = try AtomTable.build_entries(test_allocator, 2);
    defer AtomTable.clear_entries(test_allocator, dest);

    var source = test_atoms[runtime_zero..];
    try AtomTable.parser_loop(test_allocator, dest, &source);

    assert(mem.eql(u8, dest[0].?, "foo"));
    assert(mem.eql(u8, dest[1].?, "barquux"));
}

// FAILURE PATHS
test "atom parser raises if the data are too short" {
    const incomplete_atom = [_]u8{ 3, 'f', 'o' };
    var dest: ?[]u8 = undefined;
    var source = incomplete_atom[runtime_zero..];

    _ = AtomTable.Atom.parse(test_allocator, &dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => 42,
        else => unreachable,
    };
}

test "atom parser raises if the data are too short" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u' };
    var dest = try AtomTable.build_entries(test_allocator, 2);
    defer AtomTable.clear_entries(test_allocator, dest);

    var source = incomplete_atoms[runtime_zero..];
    AtomTable.parser_loop(test_allocator, dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "atom parser raises if there aren't enough entries" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u', 'x' };
    var dest = try AtomTable.build_entries(test_allocator, 3);
    defer AtomTable.clear_entries(test_allocator, dest);

    var source = incomplete_atoms[runtime_zero..];
    AtomTable.parser_loop(test_allocator, dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "atom parser raises if there aren't enough entries" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 6, 'b', 'a', 'r', 'b', 'a', 'z', 0 };
    var dest = try AtomTable.build_entries(test_allocator, 3);
    defer AtomTable.clear_entries(test_allocator, dest);

    var source = incomplete_atoms[runtime_zero..];
    AtomTable.parser_loop(test_allocator, dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

// //////////////////////////////////////////////////////////////////////
// TABLE TESTS

test "table parser works on one atom value" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 16, // length of this table
        0, 0,   0,   1, // number of atoms
        3, 'f', 'o', 'o',
    }; // atom len + string
    var slice = table[runtime_zero..];

    var atomtable = try AtomTable.parse(test_allocator, &slice);
    defer AtomTable.destroy(&atomtable);

    // check that atomtable has the the meats.
    assert(atomtable.entries.len == 1);
    assert(mem.eql(u8, atomtable.entries[0].?, "foo"));

    // check that the slice has been advanced.
    assert(slice.len == 0);
}

test "table parser works on more than one atom value" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 24, // length of this table
        0,   0,   0,   2, // number of atoms
        3,   'f', 'o', 'o',
        6,   'b', 'a', 'r',
        'b', 'a', 'z', 0,
    }; // atom len + string
    var slice = table[runtime_zero..];

    var atomtable = try AtomTable.parse(test_allocator, &slice);
    defer AtomTable.destroy(&atomtable);

    // check that atomtable has the the meats.
    assert(atomtable.entries.len == 2);
    assert(mem.eql(u8, atomtable.entries[0].?, "foo"));
    assert(mem.eql(u8, atomtable.entries[1].?, "barbaz"));

    // check that the slice has been advanced.
    assert(slice.len == 0);
}

// FAILURE PATHS
test "incomplete table fails" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0,   0,   0,   8,
    }; // incomplete chunk
    var slice = table[runtime_zero..];

    _ = AtomTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "table fails on misaligned values" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 25, // misaligned value
        0,   0,   0,   2, // number of atoms
        3,   'f', 'o', 'o',
        6,   'b', 'a', 'r',
        'b', 'a', 'z', 0,
        0,
    }; // atom len + string
    var slice = table[runtime_zero..];

    _ = AtomTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.BAD_ALIGN => return,
        else => unreachable,
    };
}

// //////////////////////////////////////////////////////////////////////
// MODULE INTEGRATION TESTS

test "module can parse atom table" {
    const module_table = [_]u8{
        'F', 'O', 'R', '1', // HEADER
        0,   0,   0,   28,
        'B', 'E', 'A', 'M',
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 24, // length of this table
        0, 0, 0, 2, // number of atoms
        3, 'f', 'o', 'o', // atom1 len + string
        6,   'b', 'a', 'r', // atom2 + padding
        'b', 'a', 'z', 0,
    };

    var module_slice = module_table[runtime_zero..];

    var module = try Module.from_slice(test_allocator, module_slice);
    defer Module.destroy(&module);

    var atoms = module.atomtable.?.entries;
    assert(atoms.len == 2);
    assert(mem.eql(u8, atoms[0].?, "foo"));
    assert(mem.eql(u8, atoms[1].?, "barbaz"));
}

test "module fails if the table is too short" {
    const module_table = [_]u8{
        'F', 'O', 'R', '1', // HEADER
        0,   0,   0,   28,
        'B', 'E', 'A', 'M',
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 28, // length of this table
        0, 0, 0, 2, // number of atoms
        3, 'f', 'o', 'o', // atom1 len + string
        6,   'b', 'a', 'r', // atom2 + padding
        'b', 'a', 'z', 0,
    };

    var module_slice = module_table[runtime_zero..];

    _ = Module.from_slice(test_allocator, module_slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}
