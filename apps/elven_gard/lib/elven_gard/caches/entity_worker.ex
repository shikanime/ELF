defmodule ElvenGard.EntityWorker do
  use GenServer

  def start_link(init_args) do
    GenServer.start_link(__MODULE__, init_args)
  end

  ## Callbacks

  @impl true
  def init(stack) do
    {:ok, stack}
  end
end
