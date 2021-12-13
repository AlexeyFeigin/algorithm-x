An implementation of [Donald Knuth's Algorithm X](https://en.wikipedia.org/wiki/Knuth%27s_Algorithm_X) (that solves the [exact cover](https://en.wikipedia.org/wiki/Exact_cover) problem). For educational purposes.

## Get dependencies

```bash
$ cabal build
```

## Play around

```bash
$ cd src
$ ghci
```

Load the demo module:

```Haskell
ghci> :l Demo.hs     
[1 of 2] Compiling AlgorithmX       ( AlgorithmX.hs, interpreted )
[2 of 2] Compiling Demo             ( Demo.hs, interpreted )
Ok, two modules loaded.
```

Explore:

```Haskell
ghci> print m1

+---++---+---+---+---+---+---+---+
|   || 1 | 2 | 3 | 4 | 5 | 6 | 7 |
+===++===+===+===+===+===+===+===+
| 0 || 1 | 0 | 0 | 1 | 0 | 0 | 1 |
| 1 || 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| 2 || 0 | 0 | 0 | 1 | 1 | 0 | 1 |
| 3 || 0 | 0 | 1 | 0 | 1 | 1 | 0 |
| 4 || 0 | 1 | 1 | 0 | 0 | 1 | 1 |
| 5 || 0 | 1 | 0 | 0 | 0 | 0 | 1 |
+---++---+---+---+---+---+---+---+

ghci> algoX m1
[[5,3,1]]
```