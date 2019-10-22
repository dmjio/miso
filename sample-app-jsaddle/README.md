## Sample Miso-JSaddle application


It's possible to build miso applications with `ghcid`, `miso` and `jsaddle`. This can enable a faster workflow due to hot reloading of the code.

This application (sample-app-jsaddle) serves as an example of development w/ GHC, and releases with GHCJS.

To take advantage of the hot reload code features, we recommend running the following command (see below) in a shell. (This will invoke `ghcid` for you).

## Dev
```bash
nix-shell --run reload
```

You should now be able to open your browser to `http://localhost:8080` and see your working application. Subsequent edits of the code should cause a live update of the website at that address.

To build the application w/ GHCJS, execute the below command.

## Build w/ GHCJS
```bash
nix-build -A release
```

## Dev with `stack`

In order to build `miso` w/ `jsaddle` support, it is necessary to remove the existing `miso` package first.

```bash
stack exec -- ghc-pkg unregister --force miso
```

Enable the `jsaddle` flag by adding the following to your project's `package.yaml` file, then call `stack build`.

```yaml
flags:
  miso:
    jsaddle: true
```

