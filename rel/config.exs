~w(rel plugins *.exs)
|> Path.join()
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
  default_release: :default,
  default_environment: Mix.env()

environment :dev do
  set dev_mode: true
  set include_erts: false
end

environment :prod do
  set include_erts: true
  set include_src: false
  set vm_args: "rel/vm.args"
end

release :gate do
  set version: "0.1.0"
  set applications: [
    :runtime_tools,
    elven_gard_tower: :permanent,
    elven_gard_gate: :permanent
  ]
end
