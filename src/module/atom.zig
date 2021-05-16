const debug = @import("std").debug;
const mem = @import("std").mem;
const testing = @import("std").testing;

const Module = @import("../module.zig").Module;
const ModuleError = @import("../module.zig").ModuleError;

pub const AtomTable = struct {
    const atoms_t = []?[]u8;

    atoms: atoms_t = undefined,
    allocator: *mem.Allocator,

    const Atom = struct {
        fn parse(allocator: *mem.Allocator, atom_ptr: *?[]u8, slice_ptr: *[]const u8) !void {
            var slice = slice_ptr.*;
            // did we run out of atoms?
            if (slice.len == 0) return ModuleError.TOO_SHORT;

            var size: usize = slice[0];
            // verify that the length is correct
            if (size >= slice.len) return ModuleError.TOO_SHORT;

            atom_ptr.* = try allocator.alloc(u8, size);
            mem.copy(u8, atom_ptr.*.?, slice[1 .. 1 + size]);
            // advance the slice pointer.
            slice_ptr.* = slice[1 + size ..];
        }

        fn destroy(allocator: *mem.Allocator, atom_ptr: *?[]u8) void {
            if (atom_ptr.*) |safe_atom_ptr| {
                allocator.free(safe_atom_ptr);
            }
        }
    };

    /// creates an AtomTable by parsing a slice that begins with a binary
    /// chunk that fits the atom table format.
    pub fn parse(allocator: *mem.Allocator, source_ptr: *[]const u8) !AtomTable {
        var source = source_ptr.*; // convenience definition
        // SANITY CHECKS
        debug.assert((source.len & 0x3) == 0);
        debug.assert(mem.eql(u8, source[0..4], "AtU8"));
        // SANITY CHECKS

        // source must be at least 12 bytes to accomodate full header
        if (source.len <= 12) return ModuleError.TOO_SHORT;
        // first 4-byte segment is the "total chunk length"
        var chunk_length: usize = Module.little_bytes_to_usize(source[4..8]);
        // pad the chunk_length if necessary.
        if ((chunk_length & 0x3) != 0) {
            chunk_length += 4 - (chunk_length & 0x3);
        }

        // verify that this our source is long enough and is aligned well
        if (chunk_length > source.len) return ModuleError.TOO_SHORT;
        defer source_ptr.* = source[8 + chunk_length ..];

        // next 4-byte segment is the "total number of atoms"
        var atom_count: usize = Module.little_bytes_to_usize(source[8..12]);

        // build a basic atoms table.
        var atoms = try build_atoms(allocator, atom_count);
        errdefer allocator.free(atoms);

        // run a parser over the atoms.
        // NB: this might fail on allocation, but that's okay, because
        // we are already clearing all atoms in the errdefer statement
        // above.
        var atom_source_ptr = source[12..];
        try parser_loop(allocator, atoms, &atom_source_ptr);

        return AtomTable{ .atoms = atoms, .allocator = allocator };
    }

    fn build_atoms(allocator: *mem.Allocator, count: usize) !atoms_t {
        var atoms = try allocator.alloc(?[]u8, count);
        // intialize the atoms with null values.
        for (atoms) |*atom, index| {
            atom.* = null;
        }
        return atoms;
    }

    fn parser_loop(allocator: *mem.Allocator, atoms: atoms_t, source: *[]const u8) !void {
        errdefer destroy_all(allocator, atoms);
        for (atoms) |*atom, index| {
            try Atom.parse(allocator, atom, source);
        }
    }

    /// destroys an AtomTable, cleaning up all dependent atoms inside
    /// the table itself.
    pub fn destroy(self: *AtomTable) void {
        destroy_all(self.allocator, self.atoms);
        self.allocator.free(self.atoms);
    }

    // safely clears atoms that have been built, whether or not they
    // contain null values.
    fn destroy_all(allocator: *mem.Allocator, atoms: atoms_t) void {
        for (atoms) |*atom| {
            Atom.destroy(allocator, atom);
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
    testing.expectEqual(source.len, 0);
    testing.expectEqualStrings(dest.?, "foo");
}

test "atom parser can be attached to a for loop for more than one atom" {
    const test_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u', 'x' };
    var atoms = try AtomTable.build_atoms(test_allocator, 2);
    defer test_allocator.free(atoms);
    defer AtomTable.destroy_all(test_allocator, atoms);

    var source = test_atoms[runtime_zero..];
    try AtomTable.parser_loop(test_allocator, atoms, &source);

    testing.expectEqualStrings(atoms[0].?, "foo");
    testing.expectEqualStrings(atoms[1].?, "barquux");
}

// FAILURE PATHS
test "zero is an inappopriate length for an atom" {
    const incomplete_atom = [_]u8{0};
    var dest: ?[]u8 = undefined;
    var source = incomplete_atom[runtime_zero..];

    _ = AtomTable.Atom.parse(test_allocator, &dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => 42,
        else => unreachable,
    };
}

test "atom parser raises if the data are too short" {
    const incomplete_atom = [_]u8{ 3, 'f', 'o' };
    var dest: ?[]u8 = undefined;
    var source = incomplete_atom[runtime_zero..];

    _ = AtomTable.Atom.parse(test_allocator, &dest, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => 42,
        else => unreachable,
    };
}

test "atom parser in a loop raises if the data are too short" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u' };
    var atoms = try AtomTable.build_atoms(test_allocator, 2);
    defer test_allocator.free(atoms);

    var source = incomplete_atoms[runtime_zero..];

    // note the parser loop will result in everything being freed.
    AtomTable.parser_loop(test_allocator, atoms, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "atom parser raises if there aren't enough atoms" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'q', 'u', 'u', 'x' };
    var atoms = try AtomTable.build_atoms(test_allocator, 3);
    defer test_allocator.free(atoms);

    var source = incomplete_atoms[runtime_zero..];
    AtomTable.parser_loop(test_allocator, atoms, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

test "atom parser raises if there aren't enough atoms" {
    const incomplete_atoms = [_]u8{ 3, 'f', 'o', 'o', 7, 'b', 'a', 'r', 'b', 'a', 'z', 'z' };
    var atoms = try AtomTable.build_atoms(test_allocator, 3);
    defer test_allocator.free(atoms);

    var source = incomplete_atoms[runtime_zero..];
    AtomTable.parser_loop(test_allocator, atoms, &source) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}

// //////////////////////////////////////////////////////////////////////
// TABLE TESTS

test "table parser works on one atom value" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 8, // length of this table
        0, 0,   0,   1, // number of atoms
        3, 'f', 'o', 'o',
    }; // atom len + string
    var slice = table[runtime_zero..];

    var atomtable = try AtomTable.parse(test_allocator, &slice);
    defer AtomTable.destroy(&atomtable);

    // check that atomtable has the the meats.
    testing.expectEqual(atomtable.atoms.len, 1);
    testing.expectEqualStrings(atomtable.atoms[0].?, "foo");

    // check that the slice has been advanced.
    testing.expectEqual(slice.len, 0);
}

test "table parser works on more than one atom value" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 16, // length of this table
        0,   0,   0,   2, // number of atoms
        3,   'f', 'o', 'o',
        7,   'b', 'a', 'r',
        'b', 'a', 'z', 'z',
    }; // atom len + string
    var slice = table[runtime_zero..];

    var atomtable = try AtomTable.parse(test_allocator, &slice);
    defer AtomTable.destroy(&atomtable);

    // check that atomtable has the the meats.
    testing.expectEqual(atomtable.atoms.len, 2);
    testing.expectEqualStrings(atomtable.atoms[0].?, "foo");
    testing.expectEqualStrings(atomtable.atoms[1].?, "barbazz");

    // check that the slice has been advanced.
    testing.expectEqual(slice.len, 0);
}

test "table parser is ok if the length needs padding" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 15, // length of this table
        0,   0,   0,   2, // number of atoms
        3,   'f', 'o', 'o',
        6,   'b', 'a', 'r',
        'b', 'a', 'z', 0,
    }; // atom len + string
    var slice = table[runtime_zero..];

    var atomtable = try AtomTable.parse(test_allocator, &slice);
    defer AtomTable.destroy(&atomtable);

    // check that atomtable has the the meats.
    testing.expectEqual(atomtable.atoms.len, 2);
    testing.expectEqualStrings(atomtable.atoms[0].?, "foo");
    testing.expectEqualStrings(atomtable.atoms[1].?, "barbaz");

    // check that the slice has been advanced past the padding
    testing.expectEqual(slice.len, 0);
}

// FAILURE PATHS
test "incomplete table fails" {
    const table = [_]u8{
        'A', 't', 'U', '8', // utf-8 atoms
        0,   0,   0,   0,
    }; // incomplete chunk
    var slice = table[runtime_zero..];

    _ = AtomTable.parse(test_allocator, &slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
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
        0, 0, 0, 16, // length of this table
        0, 0, 0, 2, // number of atoms
        3, 'f', 'o', 'o', // atom1 len + string
        6,   'b', 'a', 'r', // atom2 + padding
        'b', 'a', 'z', 0,
    };

    var module_slice = module_table[runtime_zero..];
    var module = try Module.from_slice(test_allocator, module_slice);
    defer Module.destroy(&module);

    var atoms = module.atomtable.?.atoms;
    testing.expectEqual(atoms.len, 2);
    testing.expectEqualStrings(atoms[0].?, "foo");
    testing.expectEqualStrings(atoms[1].?, "barbaz");
}

test "module fails if the table is too short" {
    const module_table = [_]u8{
        'F', 'O', 'R', '1', // HEADER
        0,   0,   0,   28,
        'B', 'E', 'A', 'M',
        'A', 't', 'U', '8', // utf-8 atoms
        0, 0, 0, 16, // length of this table
        0, 0, 0, 2, // number of atoms
        3, 'f', 'o', 'o', // atom1 len + string
        8,   'b', 'a', 'r', // atom2 + padding
        'b', 'a', 'z', 'z',
    };

    var module_slice = module_table[runtime_zero..];

    _ = Module.from_slice(test_allocator, module_slice) catch |err| switch (err) {
        ModuleError.TOO_SHORT => return,
        else => unreachable,
    };
}
