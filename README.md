## hfmt [![Build Status](https://travis-ci.org/danstiner/hfmt.svg?branch=master)](https://travis-ci.org/danstiner/hfmt)

hfmt is a tool for formatting Haskell programs. Currently it is simply a gofmt style wrapper of the excellent tools [hlint](https://github.com/ndmitchell/hlint/blob/master/README.md), [hindent](https://github.com/chrisdone/hindent#readme), and [stylish-haskell](https://github.com/jaspervdj/stylish-haskell#readme).


## Installation

    $ cabal install hfmt

## Usage

Check all Haskell source under the current directory:

    hfmt

Overwrite any files with formatting changes:

    hfmt -w

## Help text

    λ hfmt --help
    hfmt - format Haskell programs

    Usage: hfmt.exe [-d|--print-diffs] [-s|--print-sources] [-l|--print-paths]
                    [-w|--write-sources] [PATH]
      Operates on Haskell source files, reformatting them by applying suggestions
      from HLint, hindent, and stylish-haskell. Inspired by the gofmt utility.

    Available options:
      -h,--help                Show this help text
      -d,--print-diffs         If a file's formatting is different, print a diff.
      -s,--print-sources       If a file's formatting is different, print its
                               source.
      -l,--print-paths         If a file's formatting is different, print its path.
      -w,--write-sources       If a file's formatting is different, overwrite it.
      PATH                     Explicit paths to process.
                                - A single '-' will process standard input.
                                - Files will be processed directly.
                                - Directories will be recursively searched for
                                  source files to process.
                                - .cabal files will be parsed and all specified
                                  source directories and files processed.
                                - If no paths are given, the current directory will
                                  be searched for .cabal files to process, if none
                                  are found the current directory will be
                                  recursively searched for source files to process.