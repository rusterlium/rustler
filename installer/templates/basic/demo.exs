defmodule <%= module %> do
  @on_load {:init, 0}

  def init do
    path = :filelib.wildcard('target/{debug,release}/lib<%= library_name %>.*') 
            |> hd 
            |> :filename.rootname
    :ok = :erlang.load_nif(path, 0)

    :ok
  end

  def add(_a, _b), do: exit(:nif_not_loaded)
end

IO.inspect <%= module %>.add(17, 9273)
