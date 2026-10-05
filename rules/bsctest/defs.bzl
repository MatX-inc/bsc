"""Build cached result artifacts for selected semantic tests in a saved plan."""

load("//rules/bluespec:defs.bzl", "BluespecToolchainInfo")

def _bsc_test_impl(ctx):
    toolchain = ctx.attrs.toolchain[BluespecToolchainInfo]
    output = ctx.actions.declare_output("result", dir = True)
    command = cmd_args(
        ctx.attrs.runner,
        "execute",
        ctx.attrs.plan,
        ctx.attrs.identifier,
        "--installation",
        toolchain.installation,
        "--suite",
        ctx.attrs.suite,
        "--output",
        output.as_output(),
        hidden = [toolchain.host_identity],
    )
    # Ordinary test failures are recorded in result.json by the runner, which
    # exits successfully after producing its evidence. Infrastructure errors
    # fail this action. These are build actions, not Buck2 test-cache entries.
    ctx.actions.run(
        command,
        category = "bsc_test",
        local_only = True,
        allow_cache_upload = False,
    )
    return [DefaultInfo(default_output = output)]

bsc_test = rule(
    impl = _bsc_test_impl,
    attrs = {
        "toolchain": attrs.dep(providers = [BluespecToolchainInfo]),
        "runner": attrs.source(),
        "plan": attrs.source(),
        "identifier": attrs.string(),
        "suite": attrs.source(allow_directory = True),
    },
)
