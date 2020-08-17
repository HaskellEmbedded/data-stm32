# data-stm32

This is an attempt at parsing vendor-provided data to generate
a complete database of the STM32 devices, which can be used
for code-generation purposes. Main target is to replace original
`ivory-bsp-stm32` library supporting handful of devices with
a generated version supporting most of the STM32 devices.

The package contains
* parsers for SVD files
* parsers for CubeMX database files
* utilities for working with both databases
* utilities for working with peripheral registers
* ivory-bsp-stm32 library generator

## Development environment

 * git clone https://github.com/HaskellEmbedded/data-stm32
 * cd nix
 * nix-shell
 * cd ..
 * cabal repl genstm
 * :l Main
 * db <- loadDatabases


### Cache files

At first run parsed XML files are serialized to cache files
to save time with repeated loads of database
 * `/tmp/data_stm32_cmx_cache`
 * `/tmp/data_stm32_svd_cache`

## Generating device library

`genstm` tool can generate `ivory-tower-stm32-generated` library
with support for most of the STM32 devices.

To run it enter `nix` directory in the repository root. Run `nix-shell`
to obtain data files creating the database needed by `genstm` tool.
This shell has two environment variables set for your convenience -
`DB_PATH` with vendor files
and `TEMPLATES_PATH` pointing to templates used during generation.

```bash
cd nix
nix-shell
```

Then you can cd back to the repo root directory and run the actual generator

```bash
cd ..
cabal run genstm
```

Generated library is now available in `data` directory
and you can build it by entering `data` and another `nix-shell`

```
cd data
nix-shell
```

Depending on [default.nix](data/default.nix) this will either build an environment
for building `ivory-tower-helloworld` (default) or an environment where you can
run `cabal` to build the generated library. This is useful for running
`cabal new-repl` and reloading code instead of recompiling everything with Nix.

### Building `ivory-tower-helloworld` examples

```bash
# in nix-shell from previous step
git clone https://github.com/distrap/ivory-tower-helloworld
cd ivory-tower-helloworld
# checkout data-stm32 branch XXX: this is only temporary
git checkout data-stm32
# adjust test/SimpleBlinkTest.hs
make simpleblink-test
# load and run
make simpleblink-test-run
```

## Haskell overlays

`data/nix/` directory contains overlays for building whole Ivory Tower stack
in compatible and reproducible manner. Files can be adjusted to build from local git checkout
see for example [ivory-tower-drivers.nix](data/nix/ivory-tower-drivers.nix) - swapping `src` and `srcX` will build from
local directory instead of git checkout.

Final overlay is then constructed in [default.nix](data/default.nix) which also contains target to build.

## How?!

First we extract data from `$DB_PATH/db/mcu/families.xml` and corresponding
files from `$DB_PATH/db/mcu/*`, this gives us CubeMX database (cmx) according
to `src/Data/CMX/Types.hs`.

We load all the SVD files found in `$DB_PATH/svds/stm/*.svd`.

Then we match each device with its SVD file according to its name.

### Updating SVD files

Executable `app/gensvds` is a simple SVD downloader / unpacker. To obtain
recent SVD files from vendor run

```bash
cabal new-run gensvds
```

To update the `svds` branch

```bash
mv svds new_svds
git checkout svds
mv new_svds/stm/*.svd svds/stm/
git diff
git commit ...
git push ...
git show # note the new revision
```

Don't forget to update `nix/svd-database.nix`, to obtain `sha256` hash use

```bash
nix-prefetch-git https://github.com/HaskellEmbedded/data-stm32 <NEW_REVISION_HASH>
```
