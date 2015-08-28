System.cmd "rustc", ["test.rs", "--crate-type", "dylib"]

ExUnit.start

defmodule Native do
  
end
