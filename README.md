# diff_match_patch

A translation of Google's public-domain
[diff_match_patch](https://github.com/google/diff-match-patch) code into pure Elixir.

For information about the original Google project and its application,
see that repository's [wiki pages](https://github.com/google/diff-match-patch/wiki).

References from the Google project:

* diff: [An O(ND) Difference Algorithm and Its Variations (Meyers, 1986)](http://www.xmailserver.org/diff2.pdf)
* match: [Fast Text Searching with Errors (Wu and Manber, 1991)](http://www.club.cc.cmu.edu/~ajo/docs/agrep.pdf)


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `diff_match_patch` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:diff_match_patch, "~> 0.1.0"}
  ]
end
```
Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/cursor>.

## Usage

1. Find a **match** for a pattern inside at text using fuzzy search. The
`match_threshold` option determines the exactness required:

```
loc = Dmp.Match.main(
    "I am the very model of a modern major general.",
    " that berry ",
    5,
     match_threshold: 0.7)
```

The value of `loc` is 4, finding the closest match at the string starting " the very".
If no close match is found, the `loc` returned is -1.

2. Create a **diff** between two texts:

```
diffs = Dmp.Diff.main(
    "The quick brown fox jumps over the lazy dog.",
    "That quick brown fox jumped over a lazy dog.")
```

3. Create a **patch** from the the original text and the diff we just created.
The patch can also be created directly from the two texts. These
two patches are equal:

```
patches1 = Dmp.Patch.make(
    "The quick brown fox jumps over the lazy dog.", diffs)
patches2 = Dmp.Patch.make(
    "The quick brown fox jumps over the lazy dog.",
    "That quick brown fox jumped over a lazy dog.")
```

4. Apply the patch to a third text:

```
{new_text, matched} = Patch.apply(patches, "The quick red rabbit jumps over the tired tiger.")
IO.puts(new_text)
```

The `new_text` prints out as "That quick red rabbit jumped over a tired tiger."
The `matched` value is a list of booleans, showing whether the results of whether
matches were made between the (expanded) list of patches and the third text.
