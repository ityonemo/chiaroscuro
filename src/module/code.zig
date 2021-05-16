const debug = @import("std").debug;
const mem = @import("std").mem;

const Module = @import("../module.zig").Module;
const ModuleError = @import("../module.zig").ModuleError;

pub const CodeTable = struct {
    // but this will only have one for the entire table.
    allocator: *mem.Allocator,
    instruction_set: u32,
    max_opcode: u32,
    labels: usize,
    functions: usize,
    code: []u8,

    /// creates an CodeTable by parsing a slice that begins with a binary
    /// chunk that fits the atom table format.
    pub fn parse(allocator: *mem.Allocator, slice_ptr: *[]const u8) !CodeTable {
        var slice = slice_ptr.*; // convenience definition
        // SANITY CHECKS
        debug.assert(mem.eql(u8, slice[0..4], "Code"));
        // SANITY CHECKS

        // slice must be at least 24 bytes to accomodate full header
        if (slice.len <= 24) return ModuleError.TOO_SHORT;
        // first 4-byte segment is the "total chunk length".
        var chunk_length: usize = Module.little_bytes_to_usize(slice[4..8]);
        var subheader_length = Module.little_bytes_to_usize(slice[8..12]);

        var instruction_set = Module.little_bytes_to_u32(slice[12..16]);
        var max_opcode = Module.little_bytes_to_u32(slice[16..20]);
        var labels = Module.little_bytes_to_usize(slice[20..24]);
        var functions = Module.little_bytes_to_usize(slice[24..28]);

        debug.print("\n subheader: {}\n", .{subheader_length});
        debug.print("\n isa: {}\n", .{instruction_set});
        debug.print("\n max_opcode: {}\n", .{max_opcode});
        debug.print("\n labels: {}\n", .{labels});
        debug.print("\n functions: {}\n", .{functions});

        var code_start = 12 + subheader_length;

        var code_len = chunk_length - subheader_length;
        // do we have enough for the subheader
        if (slice.len < code_start + code_len) return ModuleError.TOO_SHORT;

        var code_seg = try allocator.alloc(u8, code_len);
        mem.copy(u8, code_seg, slice[code_start .. code_start + code_len]);

        // now, print out every piece of the code segment
        for (code_seg) |char| {
            debug.print("{}\n", .{char});
        }

        // advance the cursor to past the chunk.
        if (chunk_length & 0x03 != 00) {
            chunk_length += 4 - (chunk_length & 0x03);
        }
        slice_ptr.* = slice[chunk_length..];

        return CodeTable{
            .instruction_set = instruction_set,
            .max_opcode = max_opcode,
            .labels = labels,
            .functions = functions,
            .code = code_seg,
            .allocator = allocator,
        };
    }

    pub fn destroy(table: *CodeTable) void {
        table.allocator.free(table.code);
    }
};

// //////////////////////////////////////////////////////////////////////////
// TESTING
//const Testing = @import("std").testing;
//const test_allocator = Testing.allocator;
//const assert = @import("std").debug.assert;
//var runtime_zero: usize = 0;
//
//test "export parser works on a export binary" {
//    const foo_atom = [_]u8{
//        0, 0, 0, 16, // sub-size
//        0, 0, 0, 47, // instruction set
//        0, 0, 0, 47, // max opcode
//        0, 0, 0, 47, // labels
//        0,   0,   0,   47, // functions
//        'q', 'u', 'u', 'x',
//    };
//    var dest: ?CodeTable.Code = undefined;
//    var source = foo_atom[runtime_zero..];
//
//    try CodeTable.Code.parse(test_allocator, &dest, &source);
//
//    // check that the parser has moved the source slice to the end.
//    Testing.expectEqual(source.len, 0);
//    // check the parsed value
//
//    Testing.expectEqual(@intCast(u32, 47), dest.?.instruction_set);
//    Testing.expectEqual(@intCast(u32, 47), dest.?.max_opcode);
//    Testing.expectEqual(@intCast(usize, 47), dest.?.labels);
//    Testing.expectEqual(@intCast(usize, 47), dest.?.functions);
//    Testing.expectEqualSlices(u8, "quux", dest.?.code);
//
//    test_allocator.free(dest.?.code);
//}

// FAILURE PATHS
//test "expt parser raises if the data are too short" {
//    const incomplete_expt = [_]u8{ 0, 0, 7 };
//    var dest: ?CodeTable.Code = undefined;
//    var source = incomplete_expt[runtime_zero..];
//
//    Testing.expectError(ModuleError.TOO_SHORT, CodeTable.Code.parse(test_allocator, &dest, &source));
//}

//test "expt parser loop raises if the data are too short on a second go" {
//    const incomplete_atoms = [_]u8{0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0};
//    var dest = try CodeTable.build_entries(test_allocator, 2);
//    defer CodeTable.clear_entries(test_allocator, dest);
//
//    var source = incomplete_atoms[runtime_zero..];
//    CodeTable.parser_loop(test_allocator, dest, &source) catch |err| switch (err) {
//        ModuleError.TOO_SHORT => return,
//        else => unreachable,
//    };
//}
//
//// //////////////////////////////////////////////////////////////////////
//// TABLE TESTS
//
//test "table parser works on one atom value" {
//    const table = [_]u8{'E', 'x', 'p', 'T', // export table
//                         0,   0,   0,   24, // length of this table
//                         0,   0,   0,   1,  // number of exports
//                         0,   0,   0,   7,
//                         0,   0,   0,   8,
//                         0,   0,   0,   9};
//
//    var slice = table[runtime_zero..];
//
//    var expt_table = try CodeTable.parse(test_allocator, &slice);
//    defer CodeTable.destroy(&expt_table);
//
//    // check that CodeTable has the the meats.
//    assert(expt_table.entries.len == 1);
//    assert(expt_table.entries[0].?.function == 7);
//    assert(expt_table.entries[0].?.arity    == 8);
//    assert(expt_table.entries[0].?.label    == 9);
//
//    // check that the slice has been advanced.
//    assert(slice.len == 0);
//}
//
//test "table parser works on more than one atom value" {
//    const table = [_]u8{'E', 'x', 'p', 'T',
//                         0,   0,   0,   36, // length of this table
//                         0,   0,   0,   2,  // number of atoms
//                         0,   0,   0,   7,
//                         0,   0,   0,   8,
//                         0,   0,   0,   9,
//                         0,   0,   0,   10,
//                         0,   0,   0,   11,
//                         0,   0,   0,   12};
//
//    var slice = table[runtime_zero..];
//
//    var expt_table = try CodeTable.parse(test_allocator, &slice);
//    defer CodeTable.destroy(&expt_table);
//
//    // check that CodeTable has the the meats.
//    assert(expt_table.entries.len == 2);
//
//    assert(expt_table.entries[0].?.function == 7);
//    assert(expt_table.entries[0].?.arity    == 8);
//    assert(expt_table.entries[0].?.label    == 9);
//
//    assert(expt_table.entries[1].?.function == 10);
//    assert(expt_table.entries[1].?.arity    == 11);
//    assert(expt_table.entries[1].?.label    == 12);
//
//    // check that the slice has been advanced.
//    assert(slice.len == 0);
//}
//
//// FAILURE PATHS
//test "incomplete header fails" {
//    const table = [_]u8{'E', 'x', 'p', 'T',
//                         0, 0, 0};          // incomplete header
//
//    var slice = table[runtime_zero..];
//
//    _ = CodeTable.parse(test_allocator, &slice) catch | err | switch (err) {
//        ModuleError.TOO_SHORT => return,
//        else => unreachable,
//    };
//}
//
//test "nonsensical header fails" {
//    const table = [_]u8{'E', 'x', 'p', 'T',
//                         0, 0, 0, 12,
//                         0, 0, 0, 0};       // size too small to be a header
//
//    var slice = table[runtime_zero..];
//
//    _ = CodeTable.parse(test_allocator, &slice) catch | err | switch (err) {
//        ModuleError.TOO_SHORT => return,
//        else => unreachable,
//    };
//}
//
//test "incomplete table fails" {
//    const table = [_]u8{'E', 'x', 'p', 'T',
//                         0, 0, 0, 24,
//                         0, 0, 0, 1,
//                         0, 0, 0, 0};       // simply not enough data
//
//    var slice = table[runtime_zero..];
//
//    _ = CodeTable.parse(test_allocator, &slice) catch | err | switch (err) {
//        ModuleError.TOO_SHORT => return,
//        else => unreachable,
//    };
//}
//
//test "table fails on incorrect value/length combo" {
//    const table = [_]u8{'E', 'x', 'p', 'T',
//                         0, 0, 0, 24,       // funny length for 2
//                         0, 0, 0, 2,        // number of exports
//                         0, 0, 0, 0,
//                         0, 0, 0, 0,
//                         0, 0, 0, 0};
//
//    var slice = table[runtime_zero..];
//
//    _ = CodeTable.parse(test_allocator, &slice) catch | err | switch (err) {
//        ModuleError.MISMATCHED_SIZE => return,
//        else => unreachable,
//    };
//}
//
//// //////////////////////////////////////////////////////////////////////
//// MODULE INTEGRATION TESTS
//
//test "module can parse export table" {
//    const module_table = [_]u8{'F', 'O', 'R', '1', // HEADER
//                                0, 0, 0, 40,
//                               'B', 'E', 'A', 'M',
//                               'E', 'x', 'p', 'T',
//                                0, 0, 0, 36,       // length of this table
//                                0, 0, 0, 2,        // number of exports
//                                0, 0, 0, 7,        // export 1: 7-7-7
//                                0, 0, 0, 7,
//                                0, 0, 0, 7,
//                                0, 0, 0, 8,        // export 2: 8-8-8
//                                0, 0, 0, 8,
//                                0, 0, 0, 8};
//
//    var module_slice = module_table[runtime_zero..];
//
//    var module = try Module.from_slice(test_allocator, module_slice);
//    defer Module.destroy(&module);
//
//    var exports = module.expttable.?.entries;
//    assert(exports.len == 2);
//
//    assert(module.expttable.?.entries[0].?.function == 7);
//    assert(module.expttable.?.entries[0].?.arity    == 7);
//    assert(module.expttable.?.entries[0].?.label    == 7);
//
//    assert(module.expttable.?.entries[1].?.function == 8);
//    assert(module.expttable.?.entries[1].?.arity    == 8);
//    assert(module.expttable.?.entries[1].?.label    == 8);
//}
//
//test "module fails if the export table is too short" {
//    const module_table = [_]u8{'F', 'O', 'R', '1', // HEADER
//                                0, 0, 0, 26,
//                               'B', 'E', 'A', 'M',
//                               'E', 'x', 'p', 'T',
//                                0, 0, 0, 24,
//                                0, 0, 0, 1,
//                                0, 0};
//
//    var module_slice = module_table[runtime_zero..];
//
//    _ = Module.from_slice(test_allocator, module_slice) catch | err | switch (err) {
//        ModuleError.MISMATCHED_SIZE => return,
//        else => unreachable,
//    };
//}
//
//
