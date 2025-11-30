const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Dependencies
    const sqlite_dep = b.dependency("sqlite", .{
        .target = target,
        .optimize = optimize,
    });
    const sqlite_module = sqlite_dep.module("sqlite");

    const exe = b.addExecutable(.{
        .name = "Zig",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("sqlite", sqlite_module);

    // Link C library required for sqlite
    exe.linkLibC();

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addImport("sqlite", sqlite_module);
    unit_tests.linkLibC();

    const model_tests = b.addTest(.{
        .root_source_file = b.path("tests/model_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const integration_tests = b.addTest(.{
        .root_source_file = b.path("tests/integration_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    integration_tests.root_module.addImport("sqlite", sqlite_module);
    integration_tests.linkLibC();

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const run_model_tests = b.addRunArtifact(model_tests);
    const run_integration_tests = b.addRunArtifact(integration_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_model_tests.step);
    test_step.dependOn(&run_integration_tests.step);
}
