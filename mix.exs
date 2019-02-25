defmodule ElvenGardUmbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      name: "ElvenGardBastion",
      source_url: "https://github.com/deva-hub/ELF",
      start_permanent: Mix.env() == :prod,
      docs: docs(),
      deps: deps()
    ]
  end

  defp docs do
    [
      main: "Top",
      extras: ["README.md"],
      groups_for_modules: [
        "Packets": [
          ElvenGardBastion.LoginRequest,
          ElvenGardBastion.SessionRequest
        ]
      ]
    ]
  end

  defp deps do
    [
      {:distillery, "~> 2.0", runtime: false},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false},
      {:ex_doc, "~> 0.19", only: :dev, runtime: false},
      {:credo, "~> 0.10.0", only: [:dev, :test], runtime: false},
      {:benchee, "~> 0.13", only: :dev}
    ]
  end
end
