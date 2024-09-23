# The Shit

**The Shit** is inspired by [*The Fuck*](https://github.com/nvbn/thefuck) but
uses OpenAI to generate corrections. Currently supported shells are Bash and
Zsh.

**Note:** you must have a valid OpenAI API key to be able to use The Shit.

Create an OpenAI account: <https://openai.com/api/>

Generate an OpenAI API key: <https://platform.openai.com/docs/quickstart/create-and-export-an-api-key>

## Usage

If you write an incorrect console command, you simply invoke The Shit by typing
`shit` and it will generate suggestions of commands you may have wished to write.
You can then choose to execute one of these commands, or ignore the suggestions.

**Note:** the suggestions are provided by OpenAI, so they may not always be correct.
Execution of suggested commands is at the caution of the user.

Example usage:

```bash
~ » puthon
zsh: puthon: command not found
~ » shit
python [Enter/↓/Ctrl+C]
python3
Python 3.12.6 (main, Sep  8 2024, 13:18:56) [GCC 14.2.1 20240805] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
...
```

```bash
~ » git brnch
git: 'brnch' is not a git command. See 'git --help'.

The most similar command is
	branch
~ » shit
git branch [Enter/Ctrl+C]
...
```

```bash
~ » pacman -S vim
error: you cannot perform this operation unless you are root.
~ » shit
sudo pacman -S vim [Enter/Ctrl+C]
[sudo] password for elisabeth:
...
```

## Installation

Installation is done by cloning this repository and building using Stack. First,
clone the repository:

```bash
git clone git@github.com:elisabethstenholm/the-shit.git
```

Move to the root of the repository:

```bash
cd the-shit/
```

Then install the program with Stack:

```bash
stack install
```

(If you do not have Stack installed, you can find installation instructions
[here](https://docs.haskellstack.org/en/stable/#how-to-install-stack).)

Finally, add the following to your `.bashrc` or `.zshrc`:

```bash
eval $(the-shit alias)
```

The Shit will be available the next time you start a terminal, or in the current
session by sourcing your `.bashrc` or `.zshrc`:

```bash
source ~/.bashrc
```

## Config

### Custom alias

You can choose your own alias by giving it as an argument to `the-shit alias` in
your `~/.bashrc` like so:

```bash
eval $(the-shit alias --alias-name fuck)
```

This would invoke The Shit by typing `fuck` in the terminal. The default is
`shit`, if no alias name is provided.

### Custom API key environment variable

You can set the name of the environment variable where your OpenAI API key is
stored by giving it as an argument to `the-shit alias` in your `~/.bashrc` like so:

```bash
eval $(the-shit alias --var-name MY_API_KEY)
```

The Shit will then use the contents of the environment variable `MY_API_KEY` as
the API key for the requests. The default is `OPENAI_API_KEY`, if no variable
name is provided.

### Custom temperature

You can set the temperature used in the API request by giving it as an argument
to `the-shit alias` in your `~/.bashrc` like so:

```bash
eval $(the-shit alias --temperature 0.5)
```

The temperature should be a real number between `0` and `2`. The higher the
number the more random the suggestions, and the smaller the number the more
deterministic the suggestions. Any number smaller than `0` will be rounded up to
`0`, and any number larger than `2` will be rounded down to `2`.
