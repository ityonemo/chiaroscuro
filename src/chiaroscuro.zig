const std = @import("std");
const Module = @import("module.zig").Module;
const js = @import("extern.zig").js;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

fn print(string: []const u8) void {
    js.consoleLogStr(string.ptr, string.len);
}

export fn allocate(size: usize) usize {
    var slab = gpa.allocator.alloc(u8, size) catch unreachable;
    return @ptrToInt(slab.ptr);
}

export fn instantiate(ptr: usize, size: usize) void {
    var raw_module = @intToPtr([*]u8, size);
    var module = raw_module[0..size];
    // actual processing to go here.
    // this isn't complete yet.
    js.consoleLogInt(module[0]);
    js.consoleLogInt(module[1]);
    js.consoleLogInt(module[2]);
    js.consoleLogInt(module[3]);
}

test "children" {
    @import("std").testing.refAllDecls(@This());
}
