//! implements parsing for the .beam file format.  for more information,
//! refer to the documentation here: http://www.erlang.se/~bjorn/beam_file_format.html
//! test modules are generated in the erlang/ directory.

const mem = @import("std").mem;
const debug = @import("std").debug;

// chunk parsing dependencies
const Form = @import("module/form.zig");
const AtomTable = @import("module/atom.zig").AtomTable;
const ExptTable = @import("module/expt.zig").ExptTable;
const ImptTable = @import("module/impt.zig").ImptTable;
const CodeTable = @import("module/code.zig").CodeTable;

pub const ModuleError = error{
    INVALID_CHUNK,
    BAD_ALIGN,
    TOO_SHORT,
    MISMATCHED_SIZE,
};

pub const Module = struct {
    atomtable: ?AtomTable = null,
    expttable: ?ExptTable = null,
    impttable: ?ImptTable = null,
    codetable: ?CodeTable = null,
    strttable: u0 = 0,
    attrtable: u0 = 0,
    cinftable: u0 = 0,
    locttable: u0 = 0,

    // the module object should hold on to its allocator for
    // self-consistency.
    allocator: *mem.Allocator,

    const chunk_t = enum(u32) {
        ATOM = mem.bytesToValue(u32, "AtU8"),
        EXPT = mem.bytesToValue(u32, "ExpT"),
        IMPT = mem.bytesToValue(u32, "ImpT"),
        CODE = mem.bytesToValue(u32, "Code"),
        STRT = mem.bytesToValue(u32, "StrT"),
        ATTR = mem.bytesToValue(u32, "AttR"),
        CINF = mem.bytesToValue(u32, "CInf"),
        LOCT = mem.bytesToValue(u32, "LocT"),
    };

    pub fn from_slice(allocator: *mem.Allocator, data: []const u8) !Module {
        _ = try Form.validate(data);
        var this_slice = data[12..];
        var module = Module{
            .allocator = allocator,
        };
        //while (this_slice.len >= 0) {
        try parse_slice(&module, &this_slice);
        //}
        return module;
    }

    fn parse_slice(module: *Module, slice: *[]const u8) !void {
        switch (@intToEnum(chunk_t, mem.bytesToValue(u32, slice.*[0..4]))) {
            .ATOM => module.atomtable = try AtomTable.parse(module.allocator, slice),
            .EXPT => module.expttable = try ExptTable.parse(module.allocator, slice),
            .IMPT => module.impttable = try ImptTable.parse(module.allocator, slice),
            .CODE => module.codetable = try CodeTable.parse(module.allocator, slice),
            else => unreachable,
        }
    }

    pub fn destroy(module: *Module) void {
        if (module.atomtable) |*table| {
            AtomTable.destroy(table);
        }
        if (module.expttable) |*table| {
            ExptTable.destroy(table);
        }
        if (module.impttable) |*table| {
            ImptTable.destroy(table);
        }
        if (module.codetable) |*table| {
            CodeTable.destroy(table);
        }
    }

    pub fn dump(mod: Module) void {}

    /// general helper function used everywhere
    pub fn little_bytes_to_usize(src: *const [4]u8) usize {
        return little_bytes_to(usize, src);
    }
    pub fn little_bytes_to_u32(src: *const [4]u8) u32 {
        return little_bytes_to(u32, src);
    }

    pub fn little_bytes_to(comptime t: type, src: *const [4]u8) t {
        var slice = [_]u8{ src[3], src[2], src[1], src[0] };
        return mem.bytesToValue(u32, slice[0..]);
    }
};

test "children" {
    @import("std").testing.refAllDecls(@This());
}
