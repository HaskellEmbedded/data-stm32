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
 * genstm
 * (OR) cabal new-repl genstm
 * :l Main
 * db <- loadDatabases

## How?!

First we extract data from `$DB_PATH/db/mcu/families.xml` and corresponding
files from `$DB_PATH/db/mcu/*`, this gives us CubeMX database (cmx) according
to `src/Data/CMX/Types.hs`.

We load all the SVD files found in `$DB_PATH/svds/stm/*.svd`.

Then we try to match each device with its SVD file
