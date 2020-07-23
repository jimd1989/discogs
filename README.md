# discogs

A glorified shell script that tags an album's files using the Discogs API. Requires `curl` and `eyeD3`. I found native Haskell libraries for this task to be insufficient, and I'm not wasting any more of my life implementing them.

## Building

    stack build
    stack install

If the compiled binary isn't placed where you want it, just `cp` it to `/usr/local/bin` or wherever.

## Usage

    discogs [-a] {genre} {url} {/path/to/files/*.mp3}

Specify your own genre because Discogs' system is too weird to fit into one ID3 tag. The URL must be a link to a specific edition of an album, not its master release page. The mp3 arguments must be globbed; do not simply pass a directory containing them.

The program will attempt to decipher disc and track numbers for media like vinyl, cassettes, multiple CDs, or even mixes of formats, but there are too many edge cases to handle this perfectly. If you find the track numbering has been mangled, pass the absolute `-a` option, which will number files flatly in the order they were passed.

Song suites like

    A1 The Magpie Suite
      i. Morning Piping
      ii. Hunting for Grubs
      iii. The Flight

are treated as one track: `A1 - The Magpie Suite` for the time being. If your files are split individually, the program won't be able to tag them properly. The option to break down suites should be added eventually.

Discogs tags duplicate artists like `Boris` with numbers, like `Boris (3)`. This script attempts to recognize this and remove these numbers. There is a small chance that a relevant part of the artist name might be excised, but I can't think of any legitimate examples where `Artist (n)` is the canonical name of an act.

There are likely a million other edge cases where this script will fail. Double check output. You can always run `eyeD3 --remove-all *.mp3` to clear your tags.
