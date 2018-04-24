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

# License

Copyright 2018 Ross Schlaikjer

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
