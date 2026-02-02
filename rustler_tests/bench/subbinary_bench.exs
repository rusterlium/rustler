n = 1_000_000
input = :crypto.strong_rand_bytes(1024)

Benchee.run(
  %{
    "make_subbinary (Binary struct)" => fn -> RustlerTest.bench_make_subbinary(input, n) end,
    "make_subbinary_term (Term direct)" => fn -> RustlerTest.bench_make_subbinary_term(input, n) end
  },
  warmup: 3,
  time: 10,
  memory_time: 2,
  print: [configuration: false],
  formatters: [Benchee.Formatters.Console]
)
