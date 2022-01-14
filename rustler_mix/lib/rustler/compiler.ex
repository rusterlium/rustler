defmodule Rustler.Compiler do
  @moduledoc false

  alias Rustler.Compiler.{Config, Messages, Rustup}

  @doc false
  def compile_crate(module, opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)
    config = Config.from(otp_app, module, opts)

    unless config.skip_compilation? do
      crate_full_path = Path.expand(config.path, File.cwd!())

      File.mkdir_p!(priv_dir())

      Mix.shell().info("Compiling crate #{config.crate} in #{config.mode} mode (#{config.path})")

      [cmd | args] =
        make_base_command(config.cargo)
        |> make_no_default_features_flag(config.default_features)
        |> make_features_flag(config.features)
        |> make_target_flag(config.target)
        |> make_build_mode_flag(config.mode)
        |> make_platform_hacks(crate_full_path, :os.type())

      compile_result =
        System.cmd(cmd, args,
          cd: crate_full_path,
          stderr_to_stdout: true,
          env: [
            {"CARGO_TARGET_DIR", config.target_dir},
            {"RUSTLER_NIF_VERSION", nif_version()}
            | config.env
          ],
          into: IO.stream(:stdio, :line)
        )

      case compile_result do
        {_, 0} -> :ok
        {_, code} -> raise "Rust NIF compile error (rustc exit code #{code})"
      end

      handle_artifacts(crate_full_path, config)
      # See #326: Ensure that the libraries are copied into the correct subdirectory
      # in `_build`.
      Mix.Project.build_structure()
    end

    config
  end

  defp nif_version do
    System.get_env("RUSTLER_NIF_VERSION") || to_string(:erlang.system_info(:nif_version))
  end

  defp make_base_command(:system), do: ["cargo", "rustc"]
  defp make_base_command({:system, channel}), do: ["cargo", channel, "rustc"]
  defp make_base_command({:bin, path}), do: [path, "rustc"]

  defp make_base_command({:rustup, version}) do
    if Rustup.version() == :none do
      throw_error(:rustup_not_installed)
    end

    unless Rustup.version_installed?(version) do
      throw_error({:rust_version_not_installed, version})
    end

    ["rustup", "run", version, "cargo", "rustc"]
  end

  defp make_platform_hacks(args, crate_path, {:unix, :darwin}) do
    root = Path.join([".cargo", "config"])
    path = Path.join([crate_path, ".cargo", "config"])

    if File.exists?(root) || File.exists?(path) do
      args
    else
      IO.write([
        "\n",
        IO.ANSI.yellow(),
        """
        Compiling on macOS requires special link args in order to compile
        correctly.

        Rustler is currently working around this issue in the compiler task.
        This will be removed in v1.0.0 in favor of a user supplied .cargo/config
        file.

        To remove this warning, please create #{path}
        with the following content:

              [target.x86_64-apple-darwin]
              rustflags = [
                  "-C", "link-arg=-undefined",
                  "-C", "link-arg=dynamic_lookup",
              ]

        See https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/MachOTopics/1-Articles/executing_files.html
        for more details.

        """,
        IO.ANSI.default_color(),
        "\n"
      ])

      args ++ ["--", "-C", "link-arg=-undefined", "-C", "link-arg=dynamic_lookup"]
    end
  end

  defp make_platform_hacks(args, _, _), do: args

  defp make_no_default_features_flag(args, true), do: args
  defp make_no_default_features_flag(args, false), do: args ++ ["--no-default-features"]

  defp make_features_flag(args, []), do: args
  defp make_features_flag(args, flags), do: args ++ ["--features", Enum.join(flags, ",")]

  defp make_target_flag(args, target) when is_binary(target), do: args ++ ["--target=#{target}"]
  defp make_target_flag(args, _), do: args

  defp make_build_mode_flag(args, :release), do: args ++ ["--release"]
  defp make_build_mode_flag(args, :debug), do: args

  defp get_target_os_type(target) when is_binary(target) do
    case target do
      "aarch64-apple-darwin" -> {:unix, :darwin}
      "aarch64-apple-ios" -> {:unix, :darwin}
      "aarch64-apple-ios-macabi" -> {:unix, :darwin}
      "aarch64-apple-ios-sim" -> {:unix, :darwin}
      "aarch64-apple-tvos" -> {:unix, :darwin}
      "aarch64-fuchsia" -> {:unix, :linux}
      "aarch64-linux-android" -> {:unix, :linux}
      "aarch64-pc-windows-msvc" -> {:win32, :nt}
      "aarch64-unknown-freebsd" -> {:unix, :bsd}
      "aarch64-unknown-hermit" -> {:unix, :linux}
      "aarch64-unknown-linux-gnu" -> {:unix, :linux}
      "aarch64-unknown-linux-gnu_ilp32" -> {:unix, :linux}
      "aarch64-unknown-linux-musl" -> {:unix, :linux}
      "aarch64-unknown-netbsd" -> {:unix, :bsd}
      "aarch64-unknown-none" -> {:unix, :linux}
      "aarch64-unknown-none-softfloat" -> {:unix, :linux}
      "aarch64-unknown-openbsd" -> {:unix, :linux}
      "aarch64-unknown-redox" -> {:unix, :linux}
      "aarch64-unknown-uefi" -> {:unix, :linux}
      "aarch64-uwp-windows-msvc" -> {:win32, :nt}
      "aarch64-wrs-vxworks" -> {:unix, :linux}
      "aarch64_be-unknown-linux-gnu" -> {:unix, :linux}
      "aarch64_be-unknown-linux-gnu_ilp32" -> {:unix, :linux}
      "arm-linux-androideabi" -> {:unix, :linux}
      "arm-unknown-linux-gnueabi" -> {:unix, :linux}
      "arm-unknown-linux-gnueabihf" -> {:unix, :linux}
      "arm-unknown-linux-musleabi" -> {:unix, :linux}
      "arm-unknown-linux-musleabihf" -> {:unix, :linux}
      "armebv7r-none-eabi" -> {:unix, :linux}
      "armebv7r-none-eabihf" -> {:unix, :linux}
      "armv4t-unknown-linux-gnueabi" -> {:unix, :linux}
      "armv5te-unknown-linux-gnueabi" -> {:unix, :linux}
      "armv5te-unknown-linux-musleabi" -> {:unix, :linux}
      "armv5te-unknown-linux-uclibceabi" -> {:unix, :linux}
      "armv6-unknown-freebsd" -> {:unix, :bsd}
      "armv6-unknown-netbsd-eabihf" -> {:unix, :bsd}
      "armv7-apple-ios" -> {:unix, :darwin}
      "armv7-linux-androideabi" -> {:unix, :linux}
      "armv7-unknown-freebsd" -> {:unix, :bsd}
      "armv7-unknown-linux-gnueabi" -> {:unix, :linux}
      "armv7-unknown-linux-gnueabihf" -> {:unix, :linux}
      "armv7-unknown-linux-musleabi" -> {:unix, :linux}
      "armv7-unknown-linux-musleabihf" -> {:unix, :linux}
      "armv7-unknown-netbsd-eabihf" -> {:unix, :bsd}
      "armv7-wrs-vxworks-eabihf" -> {:unix, :linux}
      "armv7a-none-eabi" -> {:unix, :linux}
      "armv7a-none-eabihf" -> {:unix, :linux}
      "armv7r-none-eabi" -> {:unix, :linux}
      "armv7r-none-eabihf" -> {:unix, :linux}
      "armv7s-apple-ios" -> {:unix, :darwin}
      "asmjs-unknown-emscripten" -> {:unix, :linux}
      "avr-unknown-gnu-atmega328" -> {:unix, :linux}
      "bpfeb-unknown-none" -> {:unix, :linux}
      "bpfel-unknown-none" -> {:unix, :linux}
      "hexagon-unknown-linux-musl" -> {:unix, :linux}
      "i386-apple-ios" -> {:unix, :darwin}
      "i586-pc-windows-msvc" -> {:win32, :nt}
      "i586-unknown-linux-gnu" -> {:unix, :linux}
      "i586-unknown-linux-musl" -> {:unix, :linux}
      "i686-apple-darwin" -> {:unix, :darwin}
      "i686-linux-android" -> {:unix, :linux}
      "i686-pc-windows-gnu" -> {:win32, :nt}
      "i686-pc-windows-msvc" -> {:win32, :nt}
      "i686-unknown-freebsd" -> {:unix, :bsd}
      "i686-unknown-haiku" -> {:unix, :linux}
      "i686-unknown-linux-gnu" -> {:unix, :linux}
      "i686-unknown-linux-musl" -> {:unix, :linux}
      "i686-unknown-netbsd" -> {:unix, :bsd}
      "i686-unknown-openbsd" -> {:unix, :linux}
      "i686-unknown-uefi" -> {:unix, :linux}
      "i686-uwp-windows-gnu" -> {:win32, :nt}
      "i686-uwp-windows-msvc" -> {:win32, :nt}
      "i686-wrs-vxworks" -> {:unix, :linux}
      "mips-unknown-linux-gnu" -> {:unix, :linux}
      "mips-unknown-linux-musl" -> {:unix, :linux}
      "mips-unknown-linux-uclibc" -> {:unix, :linux}
      "mips64-unknown-linux-gnuabi64" -> {:unix, :linux}
      "mips64-unknown-linux-muslabi64" -> {:unix, :linux}
      "mips64el-unknown-linux-gnuabi64" -> {:unix, :linux}
      "mips64el-unknown-linux-muslabi64" -> {:unix, :linux}
      "mipsel-sony-psp" -> {:unix, :linux}
      "mipsel-unknown-linux-gnu" -> {:unix, :linux}
      "mipsel-unknown-linux-musl" -> {:unix, :linux}
      "mipsel-unknown-linux-uclibc" -> {:unix, :linux}
      "mipsel-unknown-none" -> {:unix, :linux}
      "mipsisa32r6-unknown-linux-gnu" -> {:unix, :linux}
      "mipsisa32r6el-unknown-linux-gnu" -> {:unix, :linux}
      "mipsisa64r6-unknown-linux-gnuabi64" -> {:unix, :linux}
      "mipsisa64r6el-unknown-linux-gnuabi64" -> {:unix, :linux}
      "msp430-none-elf" -> {:unix, :linux}
      "nvptx64-nvidia-cuda" -> {:unix, :linux}
      "powerpc-unknown-freebsd" -> {:unix, :bsd}
      "powerpc-unknown-linux-gnu" -> {:unix, :linux}
      "powerpc-unknown-linux-gnuspe" -> {:unix, :linux}
      "powerpc-unknown-linux-musl" -> {:unix, :linux}
      "powerpc-unknown-netbsd" -> {:unix, :bsd}
      "powerpc-unknown-openbsd" -> {:unix, :linux}
      "powerpc-wrs-vxworks" -> {:unix, :linux}
      "powerpc-wrs-vxworks-spe" -> {:unix, :linux}
      "powerpc64-unknown-freebsd" -> {:unix, :bsd}
      "powerpc64-unknown-linux-gnu" -> {:unix, :linux}
      "powerpc64-unknown-linux-musl" -> {:unix, :linux}
      "powerpc64-wrs-vxworks" -> {:unix, :linux}
      "powerpc64le-unknown-freebsd" -> {:unix, :bsd}
      "powerpc64le-unknown-linux-gnu" -> {:unix, :linux}
      "powerpc64le-unknown-linux-musl" -> {:unix, :linux}
      "riscv32gc-unknown-linux-gnu" -> {:unix, :linux}
      "riscv32gc-unknown-linux-musl" -> {:unix, :linux}
      "riscv32i-unknown-none-elf" -> {:unix, :linux}
      "riscv32imac-unknown-none-elf" -> {:unix, :linux}
      "riscv32imc-esp-espidf" -> {:unix, :linux}
      "riscv32imc-unknown-none-elf" -> {:unix, :linux}
      "riscv64gc-unknown-linux-gnu" -> {:unix, :linux}
      "riscv64gc-unknown-linux-musl" -> {:unix, :linux}
      "riscv64gc-unknown-none-elf" -> {:unix, :linux}
      "riscv64imac-unknown-none-elf" -> {:unix, :linux}
      "s390x-unknown-linux-gnu" -> {:unix, :linux}
      "s390x-unknown-linux-musl" -> {:unix, :linux}
      "sparc-unknown-linux-gnu" -> {:unix, :linux}
      "sparc64-unknown-linux-gnu" -> {:unix, :linux}
      "sparc64-unknown-netbsd" -> {:unix, :bsd}
      "sparc64-unknown-openbsd" -> {:unix, :linux}
      "sparcv9-sun-solaris" -> {:unix, :sunos}
      "thumbv4t-none-eabi" -> {:unix, :linux}
      "thumbv6m-none-eabi" -> {:unix, :linux}
      "thumbv7a-pc-windows-msvc" -> {:win32, :nt}
      "thumbv7a-uwp-windows-msvc" -> {:win32, :nt}
      "thumbv7em-none-eabi" -> {:unix, :linux}
      "thumbv7em-none-eabihf" -> {:unix, :linux}
      "thumbv7m-none-eabi" -> {:unix, :linux}
      "thumbv7neon-linux-androideabi" -> {:unix, :linux}
      "thumbv7neon-unknown-linux-gnueabihf" -> {:unix, :linux}
      "thumbv7neon-unknown-linux-musleabihf" -> {:unix, :linux}
      "thumbv8m.base-none-eabi" -> {:unix, :linux}
      "thumbv8m.main-none-eabi" -> {:unix, :linux}
      "thumbv8m.main-none-eabihf" -> {:unix, :linux}
      "wasm32-unknown-emscripten" -> {:unix, :linux}
      "wasm32-unknown-unknown" -> {:unix, :linux}
      "wasm32-wasi" -> {:unix, :linux}
      "wasm64-unknown-unknown" -> {:unix, :linux}
      "x86_64-apple-darwin" -> {:unix, :darwin}
      "x86_64-apple-ios" -> {:unix, :darwin}
      "x86_64-apple-ios-macabi" -> {:unix, :darwin}
      "x86_64-apple-tvos" -> {:unix, :darwin}
      "x86_64-fortanix-unknown-sgx" -> {:unix, :linux}
      "x86_64-fuchsia" -> {:unix, :linux}
      "x86_64-linux-android" -> {:unix, :linux}
      "x86_64-pc-solaris" -> {:unix, :linux}
      "x86_64-pc-windows-gnu" -> {:win32, :nt}
      "x86_64-pc-windows-msvc" -> {:win32, :nt}
      "x86_64-sun-solaris" -> {:unix, :sunos}
      "x86_64-unknown-dragonfly" -> {:unix, :linux}
      "x86_64-unknown-freebsd" -> {:unix, :bsd}
      "x86_64-unknown-haiku" -> {:unix, :linux}
      "x86_64-unknown-hermit" -> {:unix, :linux}
      "x86_64-unknown-illumos" -> {:unix, :linux}
      "x86_64-unknown-l4re-uclibc" -> {:unix, :linux}
      "x86_64-unknown-linux-gnu" -> {:unix, :linux}
      "x86_64-unknown-linux-gnux32" -> {:unix, :linux}
      "x86_64-unknown-linux-musl" -> {:unix, :linux}
      "x86_64-unknown-netbsd" -> {:unix, :bsd}
      "x86_64-unknown-none-hermitkernel" -> {:unix, :linux}
      "x86_64-unknown-none-linuxkernel" -> {:unix, :linux}
      "x86_64-unknown-openbsd" -> {:unix, :linux}
      "x86_64-unknown-redox" -> {:unix, :linux}
      "x86_64-unknown-uefi" -> {:unix, :linux}
      "x86_64-uwp-windows-gnu" -> {:win32, :nt}
      "x86_64-uwp-windows-msvc" -> {:win32, :nt}
      "x86_64-wrs-vxworks" -> {:unix, :linux}
    end
  end

  defp get_target_os_type(_) do
    :os.type()
  end

  defp handle_artifacts(path, config) do
    toml = toml_data(path)
    target = config.target
    names = get_name(toml, :lib) ++ get_name(toml, :bin)

    output_dir =
      if is_binary(target) do
        Path.join([config.target, Atom.to_string(config.mode)])
      else
        Atom.to_string(config.mode)
      end

    Enum.each(names, fn {name, type} ->
      {src_file, dst_file} = make_file_names(name, type, target)
      compiled_lib = Path.join([config.target_dir, output_dir, src_file])
      destination_lib = Path.join(priv_dir(), dst_file)

      # If the file exists already, we delete it first. This is to ensure that another
      # process, which might have the library dynamically linked in, does not generate
      # a segfault. By deleting it first, we ensure that the copy operation below does
      # not write into the existing file.
      File.rm(destination_lib)
      File.cp!(compiled_lib, destination_lib)
    end)
  end

  defp get_name(toml, section) do
    case toml[to_string(section)] do
      nil -> []
      values when is_map(values) -> [{values["name"], section}]
      values when is_list(values) -> Enum.map(values, &{&1["name"], section})
    end
  end

  defp get_suffix(target, :lib) do
    case get_target_os_type(target) do
      {:win32, _} -> "dll"
      {:unix, :darwin} -> "dylib"
      {:unix, _} -> "so"
    end
  end

  defp get_suffix(target, :bin) do
    case get_target_os_type(target) do
      {:win32, _} -> "exe"
      {:unix, _} -> ""
    end
  end

  defp make_file_names(base_name, :lib, target) do
    suffix = get_suffix(target, :lib)
    case get_target_os_type(target) do
      {:win32, _} -> {"#{base_name}.#{suffix}", "lib#{base_name}.dll"}
      {:unix, :darwin} -> {"lib#{base_name}.#{suffix}", "lib#{base_name}.so"}
      {:unix, _} -> {"lib#{base_name}.#{suffix}", "lib#{base_name}.so"}
    end
  end

  defp make_file_names(base_name, :bin, target) do
    suffix = get_suffix(target, :bin)
    case get_target_os_type(target) do
      {:win32, _} -> {"#{base_name}.#{suffix}", "#{base_name}.exe"}
      {:unix, _} -> {base_name, base_name}
    end
  end

  defp throw_error(error_descr) do
    Mix.shell().error(Messages.message(error_descr))
    raise "Compilation error"
  end

  defp toml_data(path) do
    unless File.dir?(path) do
      throw_error({:nonexistent_crate_directory, path})
    end

    case File.read("#{path}/Cargo.toml") do
      {:error, :enoent} ->
        throw_error({:cargo_toml_not_found, path})

      {:ok, text} ->
        Toml.decode!(text)
    end
  end

  defp priv_dir, do: "priv/native"
end
