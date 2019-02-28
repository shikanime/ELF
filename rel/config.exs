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

release :elven_gard_bastion do
  set version: current_version(:elven_gard_bastion)
  set applications: [
    :runtime_tools,
    elven_gard: :permanent
    elven_gard_bastion: :permanent
  ]
end
