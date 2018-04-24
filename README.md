# Catbot

Ingest cat images from reddit, and supply them in slack when asked.

Catbot's slack interface has the following commands:
- random: Special keyword that selects an image at random
- list: List all known breeds of cat
- stats: Print statstics on my dataset
- breakdown: Detailed statistics on each breed
- help: This help message

## Installation

Create a configuration file based on the example, and create a database
using the data in the sql/ folder. You can then simply run `docker build .`
to build a docker image, or `rebar3 as prod tar` if you just want the
erlang release package.
