defmodule ElvenGardCitadel.Endpoint do
  def child_spec(_opts) do
    :ranch.child_spec(
      __MODULE__,
      10,
      :ranch_tcp,
      [port: 4124],
      ElvenGardCitadel.Protocol,
      []
    )
  end
end
