defmodule ElvenGard.EntitesPool do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_) do
    children = [
      %{
        id: ElvenGard.Entity,
        start: {ElvenGard.Entity, :start_link, []},
        restart: :temporary
      }
    ]

    Supervisor.init(children, strategy: :simple_one_for_one)
  end

  def register(worker_name) do
    Supervisor.start_child(__MODULE__, [worker_name])
  end
end
