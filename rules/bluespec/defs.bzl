"""A Bluespec installation supplied by the caller, independent of test policy."""

BluespecToolchainInfo = provider(fields = ["installation", "host_identity"])

def _bluespec_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        BluespecToolchainInfo(
            installation = ctx.attrs.installation,
            host_identity = ctx.attrs.host_identity,
        ),
    ]

bluespec_toolchain = rule(
    impl = _bluespec_toolchain_impl,
    attrs = {
        "installation": attrs.source(allow_directory = True),
        "host_identity": attrs.source(),
    },
)
