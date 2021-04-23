const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

extern fn jsConsoleLogStr(?[*]const u8, u32) void;
extern fn jsConsoleLogInt(u32) void;

fn print(string: []const u8) void {
    jsConsoleLogStr(string.ptr, string.len);
}

export fn allocate(size: u32) u32 {
    var slab = gpa.allocator.alloc(u8, size) catch unreachable;
    return @ptrToInt(slab.ptr);
}

export fn instantiate(ptr: u32, size: u32) void {
    var raw_module = @intToPtr([*]u8, size);
    var module = raw_module[0..size];
    // actual processing to go here.
    // this isn't complete yet.
    jsConsoleLogInt(module[0]);
    jsConsoleLogInt(module[1]);
    jsConsoleLogInt(module[2]);
    jsConsoleLogInt(module[3]);
}