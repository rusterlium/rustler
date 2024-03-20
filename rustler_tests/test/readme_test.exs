defmodule SerdeRustlerTests.Readme.ReadmeTest do
  use ExUnit.Case
  doctest SerdeRustlerTests.Readme, import: true, except: [transcode: 0]
end
