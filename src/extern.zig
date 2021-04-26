//! extern functions that are going to derive from the
//! javascript runtime.

pub const js = if (@import("std").builtin.is_test)
    struct {
        pub fn consoleLogStr(_st: ?[*]const u8, _sz: usize) void {}
        pub fn consoleLogInt(_i: u32) void {}
    }
else
    struct {
        pub extern fn consoleLogStr(?[*]const u8, usize) void;
        pub extern fn consoleLogInt(u32) void;
    };
