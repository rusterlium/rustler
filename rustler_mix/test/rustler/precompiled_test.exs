defmodule Rustler.PrecompiledTest do
  use ExUnit.Case, async: true
  alias Rustler.Precompiled

  import ExUnit.CaptureLog
  import MixHelper

  test "target/1" do
    target_system = %{arch: "arm", vendor: "apple", os: "darwin20.3.0"}

    config = %{
      target_system: target_system,
      nif_version: "2.16",
      os_type: {:unix, :darwin}
    }

    assert {:ok, "nif-2.16-aarch64-apple-darwin"} = Precompiled.target(config)

    target_system = %{arch: "x86_64", vendor: "apple", os: "darwin20.3.0"}

    config = %{
      target_system: target_system,
      nif_version: "2.15",
      os_type: {:unix, :darwin}
    }

    assert {:ok, "nif-2.15-x86_64-apple-darwin"} = Precompiled.target(config)

    target_system = %{arch: "amd64", vendor: "pc", os: "linux", abi: "gnu"}

    config = %{
      target_system: target_system,
      nif_version: "2.14",
      os_type: {:unix, :linux}
    }

    assert {:ok, "nif-2.14-x86_64-unknown-linux-gnu"} = Precompiled.target(config)

    config = %{
      config
      | target_system: %{arch: "x86_64", vendor: "unknown", os: "linux", abi: "gnu"}
    }

    assert {:ok, "nif-2.14-x86_64-unknown-linux-gnu"} = Precompiled.target(config)

    config = %{
      target_system: %{arch: "arm", vendor: "unknown", os: "linux", abi: "gnueabihf"},
      nif_version: "2.16",
      os_type: {:unix, :linux}
    }

    assert {:ok, "nif-2.16-arm-unknown-linux-gnueabihf"} = Precompiled.target(config)

    config = %{
      target_system: %{arch: "aarch64", vendor: "unknown", os: "linux", abi: "gnu"},
      nif_version: "2.16",
      os_type: {:unix, :linux}
    }

    assert {:ok, "nif-2.16-aarch64-unknown-linux-gnu"} = Precompiled.target(config)

    config = %{
      target_system: %{arch: "aarch64", vendor: "unknown", os: "linux", abi: "gnu"},
      nif_version: "2.16",
      os_type: {:unix, :darwin}
    }

    assert {:ok, "nif-2.16-aarch64-unknown-linux-gnu"} = Precompiled.target(config)

    config = %{
      target_system: %{},
      word_size: 8,
      nif_version: "2.14",
      os_type: {:win32, :nt}
    }

    assert {:ok, "nif-2.14-x86_64-pc-windows-msvc"} = Precompiled.target(config)

    config = %{
      target_system: %{arch: "arm", vendor: "unknown", os: "linux", abi: "gnueabihf"},
      word_size: 8,
      nif_version: "2.14",
      os_type: {:win32, :nt}
    }

    assert {:ok, "nif-2.14-arm-unknown-linux-gnueabihf"} = Precompiled.target(config)

    config = %{
      target_system: %{arch: "i686", vendor: "unknown", os: "linux", abi: "gnu"},
      nif_version: "2.14",
      os_type: {:unix, :linux}
    }

    error_message =
      "precompiled NIF is not available for this target: \"i686-unknown-linux-gnu\".\nThe available targets are:\n - aarch64-apple-darwin\n - x86_64-apple-darwin\n - x86_64-unknown-linux-gnu\n - x86_64-unknown-linux-musl\n - arm-unknown-linux-gnueabihf\n - aarch64-unknown-linux-gnu\n - x86_64-pc-windows-msvc\n - x86_64-pc-windows-gnu"

    assert {:error, ^error_message} = Precompiled.target(config)
  end

  test "find_compatible_nif_version/2" do
    available = ~w(2.14 2.15 2.16)

    assert Precompiled.find_compatible_nif_version("2.14", available) == {:ok, "2.14"}
    assert Precompiled.find_compatible_nif_version("2.15", available) == {:ok, "2.15"}
    assert Precompiled.find_compatible_nif_version("2.16", available) == {:ok, "2.16"}
    assert Precompiled.find_compatible_nif_version("2.17", available) == {:ok, "2.16"}
    assert Precompiled.find_compatible_nif_version("2.13", available) == :error
    assert Precompiled.find_compatible_nif_version("3.0", available) == :error
    assert Precompiled.find_compatible_nif_version("1.0", available) == :error

    assert Precompiled.find_compatible_nif_version("2.14", ["2.14"]) == {:ok, "2.14"}
    assert Precompiled.find_compatible_nif_version("2.17", ["2.14"]) == {:ok, "2.14"}
    assert Precompiled.find_compatible_nif_version("2.13", ["2.14"]) == :error
  end

  test "maybe_override_with_env_vars/2" do
    target_system = %{
      arch: "x86_64",
      vendor: "apple",
      os: "darwin20.3.0"
    }

    assert Precompiled.maybe_override_with_env_vars(target_system, fn _ -> nil end) ==
             target_system

    env_with_targets = fn
      "TARGET_OS" -> "linux"
      "TARGET_ARCH" -> "aarch64"
      "TARGET_ABI" -> "gnu"
      _ -> nil
    end

    assert Precompiled.maybe_override_with_env_vars(target_system, env_with_targets) == %{
             arch: "aarch64",
             vendor: "unknown",
             os: "linux",
             abi: "gnu"
           }

    env_with_targets = fn
      "TARGET_OS" -> "freebsd"
      "TARGET_ARCH" -> "arm"
      "TARGET_ABI" -> "musl"
      "TARGET_VENDOR" -> "ecorp"
    end

    assert Precompiled.maybe_override_with_env_vars(target_system, env_with_targets) == %{
             arch: "arm",
             vendor: "ecorp",
             os: "freebsd",
             abi: "musl"
           }
  end

  @tag :tmp_dir
  test "check_integrity_from_map/3", %{tmp_dir: tmp_dir} do
    content = """
    Roses are red
    Violets are blue
    """

    file_path = Path.join(tmp_dir, "poem.txt")
    :ok = File.write(file_path, content)

    # the checksum is calculated with `:crypto.hash(:sha256, content) |> Base.encode16(case: :lower)`
    checksum_map = %{
      "poem.txt" => "sha256:fe16da553f29a704ad4c78624bc9354b8e4df6e4de8edb5b0f8d9f9090501911"
    }

    assert :ok = Precompiled.check_integrity_from_map(checksum_map, file_path, MyModule)

    assert {:error,
            "the precompiled NIF file does not exist in the checksum file. Please consider run: `mix rustler.download MyModule --only-local` to generate the checksum file."} =
             Precompiled.check_integrity_from_map(checksum_map, "idontexist", MyModule)

    not_supported_checksum_map = %{
      "poem.txt" => "md5:fe16da553f29a704ad4c78624bc9354b8e4df6e4de8edb5b0f8d9f9090501911"
    }

    assert {:error,
            "checksum algorithm is not supported: :md5. The supported ones are:\n - sha256"} =
             Precompiled.check_integrity_from_map(
               not_supported_checksum_map,
               file_path,
               MyModule
             )

    :ok = File.write(file_path, "let's change the content of the file")

    assert {:error, "the integrity check failed because the checksum of files does not match"} =
             Precompiled.check_integrity_from_map(checksum_map, file_path, MyModule)

    wrong_file_path = Path.join(tmp_dir, "i-dont-exist/poem.txt")

    assert {:error, message} =
             Precompiled.check_integrity_from_map(checksum_map, wrong_file_path, MyModule)

    assert message =~ "cannot read the file for checksum comparison: "
    assert message =~ wrong_file_path
    assert message =~ "Reason: :enoent"
  end

  describe "download_or_reuse_nif_file/2" do
    setup do
      root_path = File.cwd!()
      nif_fixtures_dir = Path.join(root_path, "test/fixtures")
      checksum_sample_file = Path.join(nif_fixtures_dir, "checksum-sample-file.exs")
      checksum_sample = File.read!(checksum_sample_file)

      {:ok, nif_fixtures_dir: nif_fixtures_dir, checksum_sample: checksum_sample}
    end

    @tag :tmp_dir
    test "a project using precompiled NIFs from cache", %{
      tmp_dir: tmp_dir,
      checksum_sample: checksum_sample,
      nif_fixtures_dir: nif_fixtures_dir
    } do
      in_tmp(tmp_dir, fn ->
        File.write!("checksum-Elixir.RustlerPrecompilationExample.Native.exs", checksum_sample)

        result =
          capture_log(fn ->
            config =
              Rustler.Config.from(:rustler, RustlerPrecompilationExample.Native,
                precompiled: [
                  cache_base_dir: nif_fixtures_dir,
                  base_url:
                    "https://github.com/philss/rustler_precompilation_example/releases/download/v0.2.0",
                  version: "0.2.0"
                ],
                crate: "example"
              )

            assert {:ok, new_config} = Precompiled.download_or_reuse_nif_file(config)

            assert new_config.skip_compilation?
            assert {:rustler, path} = new_config.load_from

            assert path =~ "priv/native"
            assert path =~ "example-v0.2.0-nif"
          end)

        refute result =~ "Downloading"
        refute result =~ "http://localhost"
        assert result =~ "from cache"
      end)
    end

    @tag :tmp_dir
    test "a project downloading precompiled NIFs", %{
      tmp_dir: tmp_dir,
      checksum_sample: checksum_sample,
      nif_fixtures_dir: nif_fixtures_dir
    } do
      bypass = Bypass.open()

      in_tmp(tmp_dir, fn ->
        File.write!("checksum-Elixir.RustlerPrecompilationExample.Native.exs", checksum_sample)

        Bypass.expect_once(bypass, fn conn ->
          file_name = List.last(conn.path_info)
          file = File.read!(Path.join([nif_fixtures_dir, "precompiled_nifs", file_name]))

          Plug.Conn.resp(conn, 200, file)
        end)

        result =
          capture_log(fn ->
            config =
              Rustler.Config.from(:rustler, RustlerPrecompilationExample.Native,
                precompiled: [
                  cache_base_dir: tmp_dir,
                  base_url: "http://localhost:#{bypass.port}/download",
                  version: "0.2.0"
                ],
                crate: "example"
              )

            assert {:ok, new_config} = Precompiled.download_or_reuse_nif_file(config)

            assert new_config.skip_compilation?
            assert {:rustler, path} = new_config.load_from

            assert path =~ "priv/native"
            assert path =~ "example-v0.2.0-nif"
          end)

        assert result =~ "Downloading"
        assert result =~ "http://localhost:#{bypass.port}/download"
        assert result =~ "NIF cached at"
      end)
    end
  end
end
