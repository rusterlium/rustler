defmodule MixHelper do
  # Helpers copied from Phoenix's installer: 
  # https://github.com/phoenixframework/phoenix/blob/3eac023daa7885314ba8a52c57426abe750ed82b/installer/test/mix_helper.exs#L17 
  def in_tmp(tmp_path, function) do
    path = Path.join([tmp_path, random_string(10)])

    try do
      File.rm_rf(path)
      File.mkdir_p!(path)
      File.cd!(path, function)
    after
      File.rm_rf(path)
      :ok
    end
  end

  defp random_string(len) do
    len
    |> :crypto.strong_rand_bytes()
    |> Base.encode64()
    |> binary_part(0, len)
  end
end
