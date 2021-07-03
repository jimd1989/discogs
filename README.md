# discogs

A glorified shell script that tags an album's files using the Discogs API. Requires `eyeD3` in order to do the actual tagging. Please install it through your package manager. Maybe one day I will implement ID3 manipulation natively, but this is the best solution for unicode-friendly tagging I could find. Please note that `eyeD3` seems to have gotten stricter lately. If you get any "no audio files found" messages, try running `mp3val -f` (a different program you should download) and see if that fixes anything.

## Building

    stack build
    stack install

If the compiled binary isn't placed where you want it, just `cp` it to `/usr/local/bin` or wherever.

## Usage

    discogs [-a -e] {genre} {url} {/path/to/files/*.mp3}

Specify your own genre because Discogs' system is too weird to fit into one ID3 tag. The URL must be a link to a specific edition of an album, not its master release page. The mp3 arguments must be globbed; do not simply pass a directory containing them.

The program will attempt to decipher disc and track numbers for media like vinyl, cassettes, multiple CDs, or even mixes of formats, but there are too many edge cases to handle this perfectly. Run with the absolute positioning `-a` flag to get around any disc number weirdness. This will simply number your files as they appear in your directory.

Song suites like

    A1 The Magpie Suite
      i. Morning Piping
      ii. Hunting for Grubs
      iii. The Flight

are treated as one track: `A1 - The Magpie Suite` by default. If your files are split individually, use the expand `-e` option to use the three sub tracks instead. Please note that the program is incapable of making track numbers from `i., ii., iii.`, or many other sub track positions, so this flag will also enable `-a` as well.

Discogs tags duplicate artists like `Boris` with numbers, like `Boris (3)`. This script attempts to recognize this and remove these numbers. There is a small chance that a relevant part of the artist name might be excised, but I can't think of any legitimate examples where `Artist (n)` is the canonical name of an act.

There are likely a million other edge cases where this script will fail. Double check output. You can always run `eyeD3 --remove-all *.mp3` to clear your tags.
