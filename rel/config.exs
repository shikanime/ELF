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
  set cookie: :"BN(KZDg5;3J4y,ia4itTD/Ce@8=Nt9a(s)|5r:c1HOuSm$Jg~0Km!Kr)%lj[})>f"
end

environment :prod do
  set include_erts: false
  set include_src: false
  set cookie: :"rq|&n]dptRz=pWDi([E[8h.BMTV$Q^>K`3L}W{8|tYHn!*RtL;!yoF2vdT,F6_Pe"
  set vm_args: "rel/vm.args"
end

release :nosale do
  set version: "0.1.0"
  set applications: [
    :runtime_tools,
    elven_gard_tower: :permanent,
    elven_gard_gate: :permanent
  ]
end

