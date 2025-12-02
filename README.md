# Advent of Code 2025 in Racket

This repo is for Advent of Code solution using Racket Programming language.

## Setup

You need to add your input files to a folder called `input` in the root folder and name them according to the day like `01.txt` for example.

## Installing Dependencies

Run the following command in the root folder to install dependencies declared in the `info.rkt` file.

``` shell
raco pkg install --auto
```

## Running

To run a single day solution:

``` shell
racket days/01.rkt
```

## Testing

To run all tests for all days.

``` shell
raco test days
```

Or for single day run:

``` shell
raco test days/01.rkt
```
