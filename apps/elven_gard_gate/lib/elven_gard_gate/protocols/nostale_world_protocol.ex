defmodule ElvenGardGate.NostaleWorldProtocol do
  @behaviour :ranch_protocol

  def start_link(ref, socket, transporter, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transporter])}
  end

  def init(ref, socket, transporter) do
    with :ok <- :ranch.accept_ack(ref),
         :ok <- transporter.setopts(socket, active: true) do
      :gen_server.enter_loop(__MODULE__, [], %{
        transporter: transporter
      })
    end
  end

  def handle_info({:tcp, _socket, _req}, state) do
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state) do
    state.transporter.close(socket)
    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _socket, reason}, state) do
    case reason do
      :closed -> {:stop, :normal, state}
      :timeout -> {:stop, :normal, state}
      reason -> {:stop, reason, state}
    end
  end
end
