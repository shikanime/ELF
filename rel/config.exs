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
    elven_gard_bastion: :permanent
  ]
end

release :elven_gard_citadel do
  set version: current_version(:elven_gard_citadel)
  set applications: [
    :runtime_tools,
    elven_gard_citadel: :permanent
  ]
end

release :elven_gard_universe do
  set version: current_version(:elven_gard_universe)
  set applications: [
    :runtime_tools,
    elven_gard_universe: :permanent
  ]
end

release :elven_gard_guard do
  set version: current_version(:elven_gard_guard)
  set applications: [
    :runtime_tools,
    elven_gard_guard: :permanent
  ]
end

release :elven_gard do
  set version: current_version(:elven_gard_guard)
  set applications: [
    :runtime_tools,
    elven_gard_bastion: :permanent,
    elven_gard_citadel: :permanent,
    elven_gard_guard: :permanent,
    elven_gard_universe: :permanent
  ]
end
