# CuteGit

CuteGit is a tiny Git front end written in Haskell. It runs in the terminal, leans on regular `git` commands under the hood, and gives them a softer, friendlier shell.

## Features

- status overview with branch and ahead/behind summary
- recent commit list on the home screen
- quick views for status, log, and branches
- stage all, commit, pull, and push actions
- add a remote without leaving the app
- repo switching from inside the app
- repo initialization when launched in a non-git folder
- prompt editing with backspace support

## Run

```bash
cabal run cutegit
```

If your environment blocks Cabal from writing its usual cache or log paths, compile directly with `ghc`:

```bash
ghc -Wall -o cutegit app/Main.hs
./cutegit
```

You can also build the executable first:

```bash
cabal build
```

Then run:

```bash
./dist-newstyle/build/*/*/cutegit-*/x/cutegit/build/cutegit/cutegit
```

## Notes

CuteGit assumes `git` is installed and available in `PATH`.
