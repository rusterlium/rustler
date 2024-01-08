defmodule SerdeRustlerTests.Readme.AnimalType.Cat do
  require Record
  @type t :: {__MODULE__, String.t()}
  Record.defrecord(:record, __MODULE__, breed: "tabby")
end

defmodule SerdeRustlerTests.Readme.AnimalType.Dog do
  require Record
  @type t :: {__MODULE__, String.t()}
  Record.defrecord(:record, __MODULE__, breed: "mutt")
end

defmodule SerdeRustlerTests.Readme.Animal do
  alias SerdeRustlerTests.Readme.AnimalType
  alias AnimalType.{Cat, Dog}
  require Cat
  require Dog

  @type t :: %__MODULE__{
          type: Cat.t() | Dog.t(),
          name: bitstring,
          age: pos_integer,
          owner: nil | bitstring
        }
  defstruct type: Cat.record(),
            name: "",
            age: 0,
            owner: nil
end

defmodule SerdeRustlerTests.Readme do
  alias SerdeRustlerTests.Readme.{Animal, AnimalType.Cat}
  require Cat

  def animal(),
    do: %Animal{
      type: Cat.record(),
      name: "Garfield",
      age: 41
    }

  @doc """
  Round-trips an `t:Animal.t/0` struct using NIF-defined type hints.

  ## Examples:
      iex> readme()
      animal()
  """
  def readme(), do: SerdeRustlerTests.readme(animal())

  @doc """
  Transcodes an `t:Animal.t/0` struct using `serde-transcode`.

  ## Examples:
      iex> transcode()
      {:ok, animal()}
  """
  def transcode(), do: SerdeRustlerTests.transcode(animal())
end
