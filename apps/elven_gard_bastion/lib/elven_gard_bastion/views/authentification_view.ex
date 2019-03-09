defmodule ElvenGardBastion.AuthentificationView do
  def render(:sign_in, params) do
    """
    NsTeST \
    #{params.user_name} \
    #{params.client_id} \
    #{render_worlds(params.worlds)}\
    """
  end

  def render(:outdated_client, _params) do
    "failc 1"
  end

  def render(:error, _params) do
    "failc 2"
  end

  def render(:maintenance, _params) do
    "failc 3"
  end

  def render(:session_already_used, _params) do
    "failc 4"
  end

  def render(:unvalid_credential, _params) do
    "failc 5"
  end

  def render(:cant_login, _params) do
    "failc 6"
  end

  def render(:user_blacklisted, _params) do
    "failc 7"
  end

  def render(:country_blacklisted, _params) do
    "failc 8"
  end

  def render(:check_case, _params) do
    "failc 9"
  end

  def render(_type, _params) do
    "failc 10"
  end

  defp render_worlds(worlds) do
    worlds
    |> Enum.map(&(render_world(&1)))
    |> Enum.concat(["-1:-1:-1:10000.10000.1"])
    |> Enum.join(" ")
  end

  defp render_world(worlds_status) do
    """
    #{worlds_status.ip}:\
    #{worlds_status.port}:\
    #{render_world_population(worlds_status.population_number, worlds_status.population_limit)}:\
    #{worlds_status.world_id}.\
    #{worlds_status.channel_id}.\
    #{Recase.to_pascal(worlds_status.name)}\
    """
  end

  defp render_world_population(current, limit) do
    "#{current / limit * 20}"
  end
end
