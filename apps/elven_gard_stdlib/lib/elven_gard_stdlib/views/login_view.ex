defmodule ElvenGardStdlib.LoginView do
  def render("loging_success.nsl", %{user_name: user_name, session_id: session_id, server_statuses: server_statuses}) do
    "NsTeST #{user_name} #{session_id} #{render_server_statuses(server_statuses)}"
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
    server_statuses
    |> Enum.map(&(render_server_status(&1)))
    |> Enum.concat(["-1:-1:-1:10000.10000.1"])
    |> Enum.join(" ")
  end

  defp render_server_status(server_status) do
    """
    #{server_status.ip}:\
    #{server_status.port}:\
    #{render_server_charge(server_status.population, server_status.population_limit)}:\
    #{server_status.world_id}.\
    #{server_status.channel_id}.\
    #{server_status.name}\
    """
  end

  defp render_server_charge(population, population_limit) do
    "#{population / population_limit * 20}"
  end
end