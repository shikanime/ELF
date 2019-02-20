defmodule ElvenGardUmbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "ElvenGard",
      source_url: "https://github.com/deva-hub/ELF",
      docs: [
        main: "Top",
        extras: ["README.md"]
      ]
    ]
  end

  defp deps do
    [
      {:distillery, "~> 2.0", runtime: false},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false},
      {:ex_doc, "~> 0.19", only: :dev, runtime: false},
      {:credo, "~> 0.10.0", only: [:dev, :test], runtime: false},
      {:espec, "~> 1.6.3", only: :test},
      {:mock, "~> 0.3.0", only: :test},
      {:benchee, "~> 0.13", only: :dev}
    ]
  end
end
