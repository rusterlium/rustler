# FAQ

##### Nightly branch
Rustler depends on features from the Rust nightly branch. If you get sent here, it means you have either stable or beta installed.

##### Could not find rustc/cargo
This probably means you don't have Rust installed. I recommend you follow the instructions [here](https://www.rust-lang.org/downloads.html#nightly). Nightly is required.

A slightly more complicated solution that may save you time in the long run is using MultiRust.

##### Unsuppored NIF version
The Erlang NIF API is versioned. You need a NIF library spesifically built for the NIF version your BEAM supports. At the time of writing, Rustler supports the newest, and the past couple of minor NIF versions. If you get this problem it probably means you have a very old version of the BEAM installed.

##### Other problems
If you have any other issues, or you want to give feedback, I (hansihe) try to be availible in #elixir-lang on freenode as much as possible. I am also in the RustBridge and Elixir Slack.
