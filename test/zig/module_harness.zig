const Module = @import("src/module.zig").Module;
const std = @import("std");
const allocator = std.testing.allocator;

const basic_beam_path = "_build/test/lib/chiaroscuro/ebin/Elixir.Basic.beam";
const mb = 1024 * 1024;

test "module basics" {
    var cwd = std.fs.cwd();
    var file = try cwd.openFile(basic_beam_path, .{.read = true, .write = false});
    var module_bin = try file.readToEndAlloc(allocator, mb);
    defer allocator.free(module_bin);

    var module = try Module.from_slice(allocator, module_bin);
}