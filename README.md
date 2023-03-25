Sort w3m bookmarks

Reads from stdin (one or more, concatenated) html with w3m bookmarks, parse them
and merges them joining sections with the same name and removing duplicate
bookmarks for a given section (where two bookmarks are the same if they have
both the same url and text).

Installation

```
cabal install
```

Usage

To just sort the bookmarks:

```
sort-w3m-bookmars < $HOME/.w3m/bookmark.html > sorted.html
```

To merge and sort many bookmarks

```
cat bookmark1.html bookmark2.html | sort-w3m-bookmar > merged.html
```

