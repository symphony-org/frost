# Frost

## Installation

### Via Stack
Build the latest executable via `stack install`.
After stack has done it's job the executable should be availabe as `frost`

### From hackage
TODO - obtaining from hackage

## Usage
Running `frost examples/git.md` should create a `git.md.html` file in the `examples/` directory.

## Development
See [DEVELOPERS.md](DEVELOPERS.md)

## Plugin prerequisites

### Rholang
TODO - nixify

If you intend to use the Rholang plugin, you'll need to make sure that Frost is able to use the Rholang CLI. Follow the next steps to make this happen:
1. Follow the instructions under https://github.com/rchain/rchain/tree/dev/rholang#building-from-source to build the CLI
2. Copy the resulting jar to your ~/bin directory and rename it to rholangCLI.jar
3. Copy the script in ~/sh/rholang to your ~/bin dir and make it executable
