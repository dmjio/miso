# `nix/js` — the LynxJS toolchain

`rspeedy` (`@lynx-js/rspeedy`, wrapping rspack) builds `main.lynx.bundle` for
the native backend. It is not in nixpkgs, so nix installs it from this repo's
own `package.json` + `bun.lock`.

## Why this exists

`buildNpmPackage` — what this used to use — can only read an **npm** lockfile.
That forced a `package-lock.json` to live alongside `bun.lock`, kept in sync by
hand with nothing checking it. That drift is not hypothetical: pinning three
versions in `package.json` once desynced the two and broke the Android and iOS
CI jobs, which both depend on the bundle.

[bun2nix](https://github.com/nix-community/bun2nix) reads `bun.lock` directly,
so there is one lockfile again.

## `bun.nix` is generated — regenerate it when `bun.lock` changes

```sh
bun run nix:lock
```

(equivalently: `nix run github:nix-community/bun2nix -- -o nix/js/bun.nix`)

Commit the result. CI regenerates it and fails if it differs from what is
committed, so the drift that motivated this change cannot come back silently.

## Bumping bun2nix

The revision is pinned by the `bun2nix` input in `flake.nix`.
`nix/js/default.nix` reads it back out of `flake.lock`, so the flake and the
legacy `nix-build -A …` entry point always agree.

```sh
nix flake update bun2nix
```

## Notes on rspack's native bindings

rspack ships its Rust binding as prebuilt, platform-specific `.node` addons
(`@rspack/binding-<platform>`); `bun.lock` records `os`/`cpu` for each, which is
how the right one gets selected. Two `fetchBunDeps` options matter:

- `useFakeNode = false` — these are real Node addons built against V8, so
  dependency scripts must get `node`, not bun's shim.
- `autoPatchElf` (Linux) — the prebuilt `.node` files are ELF objects linked
  against a host libc/libstdc++ that does not exist in the Nix store.

Note this is a behavioural change from `buildNpmPackage`, which compiled the
binding from source via `rustPlatform`. Consuming the prebuilt binding should
be considerably faster, but it is the thing to check first if a bundle build
misbehaves.
