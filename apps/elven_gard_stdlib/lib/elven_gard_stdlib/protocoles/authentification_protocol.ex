defmodule ElvenGardStdlib.AuthentificationProtocol do
  use GenStateMachine

  alias ElvenGardStdlib.{
    SessionPacket,
    CharacterSelectView,
    WorldCrypto,
    SessionCrypto,
    PasswordPacket,
    UsernamePacket,
    LoginCrypto,
    LoginPacket,
    LoginView
  }

  def start_link() do
    GenStateMachine.start_link(
      __MODULE__,
      {:session,
       %{
         session_id: nil,
         user_name: nil,
         user_password: nil
       }}
    )
  end

  def decrypt_packet(pid, packet) do
    GenStateMachine.call(pid, {:decrypt_packet, packet})
  end

  def parse_packet(pid, packet) do
    GenStateMachine.call(pid, {:parse_packet, packet})
  end

  def render_packet(pid, view, params) do
    GenStateMachine.call(pid, {:render_packet, {view, params}})
  end

  def encrypt_packet(pid, packet) do
    GenStateMachine.call(pid, {:encrypt_packet, packet})
  end

  def handle_event({:call, from}, {:decrypt_packet, packet}, :login, _data) do
    {
      :keep_state_and_data,
      [{:reply, from, LoginCrypto.decrypt(packet)}]
    }
  end

  def handle_event({:call, from}, {:parse_packet, packet}, :login, _data) do
    {
      :keep_state_and_data,
      [{:reply, from, LoginPacket.parse!(packet)}]
    }
  end

  def handle_event({:call, from}, {:produce_packet, {view, params}}, :login, _data) do
    {
      :keep_state_and_data,
      [{:reply, from, LoginView.render(view, params)}]
    }
  end

  def handle_event({:call, from}, {:encrypt_packet, packet}, :login, data) do
    {
      :next_state,
      :session,
      data,
      [{:reply, from, Enum.map(packet, &WorldCrypto.encrypt(&1))}]
    }
  end

  def handle_event({:call, from}, {:decrypt_packet, packet}, :session, _data) do
    {
      :keep_state_and_data,
      [{:reply, from, SessionCrypto.decrypt(packet)}]
    }
  end

  def handle_event({:call, from}, {:parse_packet, packet}, :session, data) do
    parsed_packet = SessionPacket.parse!(packet)

    {
      :next_state,
      :credential,
      %{data | session_id: parsed_packet.session_id},
      [{:reply, from, parsed_packet}]
    }
  end

  def handle_event({:call, from}, {:decrypt_packet, packet}, :credential, data) do
    {
      :keep_state_and_data,
      [{:reply, from, WorldCrypto.decrypt(packet, data.session_id)}]
    }
  end

  def handle_event(
        {:call, from},
        {:parse_packet, packet},
        :credential,
        %{user_name: nil, user_password: nil}
      ) do
    {
      :keep_state_and_data,
      [{:reply, from, UsernamePacket.parse!(packet)}]
    }
  end

  def handle_event(
        {:call, from},
        {:parse_packet, packet},
        :credential,
        data = %{user_password: nil}
      ) do
    {
      :next_state,
      :character,
      data,
      [{:reply, from, PasswordPacket.parse!(packet)}]
    }
  end

  def handle_event({:call, from}, {:render_packet, {view, params}}, :character, _data) do
    {
      :keep_state_and_data,
      [{:reply, from, CharacterSelectView.render(view, params)}]
    }
  end

  def handle_event({:call, from}, {:encrypt_packet, packet}, :character, _data) do
    {
      :stop,
      :normal,
      [{:reply, from, Enum.map(packet, &WorldCrypto.encrypt(&1))}]
    }
  end
end
