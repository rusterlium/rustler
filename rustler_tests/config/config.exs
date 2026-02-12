import Config

config :rustler_test, RustlerTest,
  load_data: [
    worker_threads: 4,
    thread_name: "rustler-test"
  ]
