# Brainfuck

Brainfuck written in Haskell.

**Note**: `diverge branch`: a cell will reset to `0` when exceeded `256`.

Compile:

``` hs
ghc Main.hs --make -o bf -O2
```

Run: `./bf` to start interpreter or `./bf hello.bf` to run file.

> Example Brainfuck code from [Brainfuck Visualizer](https://github.com/fatiherikli/brainfuck-visualizer/)
