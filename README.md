# Formark - A formatter for Markdown

- [Usage](#usage)
  - [Basic usage](#basic-usage)
  - [Options](#options)
- [Roadmap](#roadmap)
  - [Features](#features)
  - [Bugs](#bugs)

## Usage

See the [roadmap](#roadmap) for features that are not being worked on currently,
but are planned for the future.

### Basic usage

The following command will format a file called `unformatted.md` and output
result to a file called `formatted.md`

```shell
formark -f unformatted.md -o formatted.md
```

### Options

For all options that take a value, the value is separated form the option by a
space, e.g. `-f file.md`

- `-f,--input-file`

  - Expected value: file path
  - Default value: None, this flag has to be specified
  - Description: path to the input file which will be formatted

- `-o,--output-file`

  - Expected value: file path
  - Default value: None, this flag has to be specified
  - Description: path to the output file where formatted text will be written
    to.
    - Can be same as `-f`

- `-i,--indentation`

  - Expected value: integer
  - Default value: 2
  - Description: Indentation level specified as number of spaces.

- `-l,--max-line-length`

  - Expected value: integer
  - Default value: 80
  - Description: Maximum length of a line in characters. If not 0, formark will
    try to wrap paragraphs and bullet points as close to (but not going over)
    this value.
    - Wrapping is done as a single line break, so output is not affected.

- `-s,--use-star`

  - Expected value: none
  - Default value: N/A
  - Description: Should star/asterisk (\*) be used for bullet points instead of
    dash (-)

- `-h`

  - Expected value: none
  - Default value: N/A
  - Description: Print the help message.

## Roadmap

### Features

- Ability to output formatted file to stdout

  - `fomrark -f unformatted.md > formatted.md`
  - This can also serve as preview
  - (Syntax not finalised)

- Ability to take file from stdin and format it, either writing to a file or
  stdout.

  - `cat unformatted.md | formark > formatted.md`
  - `cat unformatted.md | formark -o formatted.md`

- Specifying different extensions

  - Currently github-flavored-markdown is the only (hardcoded) option

### Bugs

- weird number of spaces after bullet point when using bigger indentation values
