defmodule NifNotLoadedError do
  defexception message: "nif not loaded"
end

defmodule RustlerTest do
  defmodule Helper do
    @nif_version Version.parse!("#{:erlang.system_info(:nif_version)}.0")

    def nif_feature_from_running_version() do
      "nif_version_#{@nif_version.major}_#{@nif_version.minor}"
    end

    def has_nif_version(version) do
      req = Version.parse_requirement!("~> #{version}")
      Version.match?(@nif_version, req)
    end
  end

  use Rustler,
    otp_app: :rustler_test,
    crate: :rustler_test,
    features: [Helper.nif_feature_from_running_version()]

  def term_debug_and_reparse(term) do
    with debug_str <- term_debug(term),
         debug_str <- :erlang.binary_to_list(debug_str) ++ ~c".",
         {:ok, tokens, _} <- :erl_scan.string(debug_str),
         {:ok, ast} <- :erl_parse.parse_exprs(tokens),
         {:value, res, _} <- :erl_eval.exprs(ast, %{}) do
      res
    end
  end
end
