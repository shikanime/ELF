defmodule ElvenGardBastion.Endpoint do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    children = [
      :ranch.child_spec(
        ElvenGardBastion.GateEndpoint,
        10,
        :ranch_tcp,
        [port: 4123],
        ElvenGardBastion.NostaleLoginProtocol,
        []
      ),
      :ranch.child_spec(
        ElvenGardBastion.WorldEndpoint,
        10,
        :ranch_tcp,
        [port: 4124],
        ElvenGardBastion.NostaleWorldProtocol,
        []
      ),
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
