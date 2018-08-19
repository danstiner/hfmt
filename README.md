hfmt
====

[![Hackage](https://img.shields.io/hackage/v/hfmt.svg)](https://hackage.haskell.org/package/hfmt)
[![license](https://img.shields.io/github/license/danstiner/hfmt.svg)](https://github.com/danstiner/hfmt/blob/master/LICENSE)
[![Build Status](https://travis-ci.org/danstiner/hfmt.svg?branch=master)](https://travis-ci.org/danstiner/hfmt)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdanstiner%2Fhfmt.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdanstiner%2Fhfmt?ref=badge_shield)

hfmt is a tool for formatting Haskell programs. Currently it is simply a gofmt style wrapper of the excellent tools [hlint](https://github.com/ndmitchell/hlint/blob/master/README.md), [hindent](https://github.com/chrisdone/hindent#readme), and [stylish-haskell](https://github.com/jaspervdj/stylish-haskell#readme).


## Installation

with [stack](https://www.haskellstack.org/)

    $ stack install hfmt

with [cabal](https://www.haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package)

    $ cabal install hfmt

## Usage

Check all Haskell source under the current directory:

    hfmt

Overwrite files with formatting suggestions:

    hfmt -w

## Help text

    λ hfmt --help
    hfmt - format Haskell programs

    Usage: hfmt [-d|--print-diffs] [FILE]
      Reformats Haskell source files by applying HLint, hindent, and
      stylish-haskell.

    Available options:
      -h,--help                Show this help text
      -d,--print-diffs         If a file's formatting is different, print a diff.
      -s,--print-sources       If a file's formatting is different, print its
                              source.
      -l,--print-paths         If a file's formatting is different, print its path.
      -w,--write-sources       If a file's formatting is different, overwrite it.
      FILE                     Explicit paths to process.
                                - A single '-' will process standard input.
                                - Files will be processed directly.
                                - Directories will be recursively searched for source files to process.
                                - .cabal files will be parsed and all specified source directories and files processed.
                                - If no paths are given, the current directory will be searched for .cabal files to process, if none are found the current directory will be recursively searched for source files to process.

    Exit Codes:
      0 = No error
      1 = Encountered I/O or other operational error
      2 = Failed to parse a source code file
      3 = Source code was parsed but cannot be formatted properly
      4 = Formatted code differs from existing source (--print-diffs only)


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdanstiner%2Fhfmt.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdanstiner%2Fhfmt?ref=badge_large)