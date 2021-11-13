# Formark - A formatter for Markdown

![Build](https://github.com/dk949/formark/actions/workflows/haskell.yaml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-BSD3-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)



* [Installation](#installation)
  * [From Github release](#from-github-release)
  * [From Source](#from-source)
  * [From Script](#from-script)
* [Usage](#usage)
  * [Basic usage](#basic-usage)
  * [Options](#options)
* [Roadmap](#roadmap)
  * [Features](#features)
  * [Bugs](#bugs)

## Installation

### From Github release

* The latest stable release should be available in the releases tab
* You can download and place it in a PATH directory.

### From Source

* Building the project for the first time from source can take a long time (15+
  min)
* To build and install the program from source, clone the repository, then run

``` sh
stack build && stack install
```

### From Script

* Coming soon

``` sh
curl "$SCRIPT_URL" | sh
```

## Usage

See the [roadmap](#roadmap) for features that are not being worked on currently,
but are planned for the future.

### Basic usage

The following command will format a file called `unformatted.md` and output
result to a file called `formatted.md`

``` shell
formark -f unformatted.md -o formatted.md
```

### Options

For all options that take a value, the value is separated form the option by a
space, e.g. `-f file.md`

* `-f,--input-file`

  * Expected value: file path
  * Default value: None, this flag has to be specified
  * Description: path to the input file which will be formatted

* `-o,--output-file`

  * Expected value: file path
  * Default value: None, this flag has to be specified
  * Description: path to the output file where formatted text will be written
    to.
    * Can be same as `-f`

* `-i,--indentation`

  * Expected value: integer
  * Default value: 2
  * Description: Indentation level specified as number of spaces.

* `-l,--max-line-length`

  * Expected value: integer
  * Default value: 80
  * Description: Maximum length of a line in characters. If not 0, formark will
    try to wrap paragraphs and bullet points as close to (but not going over)
    this value.
    * Wrapping is done as a single line break, so output is not affected.

* `-d,--use-dash`

  * Expected value: none
  * Default value: N/A
  * Description: Should dash be used for bullet points instead of star/asterisk

* `-h`

  * Expected value: none
  * Default value: N/A
  * Description: Print the help message.

## Roadmap

### Features

* Ability to output formatted file to stdout

  * `fomrark -f unformatted.md > formatted.md`
  * This can also serve as preview
  * (Syntax not finalised)

* Ability to take file from stdin and format it, either writing to a file or
  stdout.

  * `cat unformatted.md | formark > formatted.md`
  * `cat unformatted.md | formark -o formatted.md`

* Specifying different extensions

  * Currently github-flavored-markdown is the only (hardcoded) option

### Bugs

* weird number of spaces after bullet point when using bigger indentation values
