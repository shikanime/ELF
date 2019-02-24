defmodule ElvenGard.LoginResponse do
  def render("loging_success.nsl", %{user_id: user_id, client_id: client_id, server_statuses: server_statuses}) do
    "NsTeST #{user_id} #{client_id} #{render_server_statuses(server_statuses)}"
  end

  def render("session_already_used.nsl", %{}) do
    "failc 4"
  end

  def render("bad_credential.nsl", %{}) do
    "failc 5"
  end

  def render("cant_login.nsl", %{}) do
    "failc 6"
  end

  def render("user_blacklisted.nsl", %{}) do
    "failc 7"
  end

  def render("country_blacklisted.nsl", %{}) do
    "failc 8"
  end

  defp render_server_statuses(server_statuses) do
    Enum.reduce(server_statuses, "", fn server_status, acc ->
      """
      #{acc} \
      #{server_status.ip}:\
      #{server_status.port}:\
      #{render_server_charge(server_status.population)}:\
      #{server_status.world_id}.\
      #{server_status.channel_id}.\
      #{server_status.name}\
      """
    end) <> " -1:-1:-1:10000.10000.1"
  end

  defp render_server_charge(population) do
    # TODO: move to global configuration
    "#{population / 200 * 20}"
  end
end
