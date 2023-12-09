# λ ho. ho ho

This repository contains my Haskell solutions for some of the [Advent of Code](https://adventofcode.com/) puzzles ([2015](https://github.com/ncfavier/aoc/tree/main/src/2015), [2016](https://github.com/ncfavier/aoc/tree/main/src/2016), [2018](https://github.com/ncfavier/aoc/tree/main/src/2018), [2019](https://github.com/ncfavier/aoc/tree/main/src/2019), [2020](https://github.com/ncfavier/aoc/tree/main/src/2020), [2021](https://github.com/ncfavier/aoc/tree/main/src/2021), [2022](https://github.com/ncfavier/aoc/tree/main/src/2022), [2023](https://github.com/ncfavier/aoc/tree/main/src/2023)). The [`AOC`](https://github.com/ncfavier/aoc/blob/main/src/AOC.hs) module defines shared utility functions.

You can run my solutions locally using [Nix](https://nixos.org):

```console
$ git clone https://github.com/ncfavier/aoc
$ cd aoc
$ nix --experimental-features 'nix-command flakes' develop
$ echo {your adventofcode.com session cookie} > .session
$ aoc run {year} {day}
```

I participate in Advent of Code mainly to practice my Haskell and learn things, but also because waking up at 6 a.m. through December to do programming puzzles is [*fun*](http://dwarffortresswiki.org/index.php/DF2014:Losing).

If you want to chat, you can probably find me in the `#adventofcode`, `#adventofcode-spoilers`, `#adventofcode-help` and `#haskell` channels on [Libera](https://libera.chat/), as **ncf**.

Also check out [glguy](https://github.com/glguy?tab=repositories&q=advent&type=source)'s, [jle](https://github.com/mstksg?tab=repositories&q=advent&type=source)'s, [ephemient](https://github.com/ephemient?tab=repositories&q=aoc&type=source)'s and [mniip](https://github.com/mniip?tab=repositories&q=aoc&type=source)'s solutions (all in Haskell).
