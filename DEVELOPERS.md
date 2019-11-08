## Developing frost

### Development Setup
We use [Stack](https://docs.haskellstack.org/en/stable/README/) for development.
Simply cloning the repo and running `stack test` should be enough to start hacking on the project.

### Known problems
Stack installed via `nix-env` is known to cause problems while building.

## Running frost
`stack run path/to/file.md` runs frost and feeds it with the supplied markdown file. The resulting html is going to be written to `path/to/file.md.html`
