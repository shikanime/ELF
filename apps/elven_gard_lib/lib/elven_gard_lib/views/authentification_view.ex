defmodule ElvenGardLib.AuthentificationView do
  def render(:sign_in, params) do
    """
    NsTeST \
    #{Recase.to_snake(params.user_name)} \
    #{params.client_id} \
    #{render_worlds(params.worlds)}\
    """
  end

  def render(:outdated_client, _params), do: "failc 1"
  def render(:error, _params), do: "failc 2"
  def render(:maintenance, _params), do: "failc 3"
  def render(:session_already_used, _params), do: "failc 4"
  def render(:unvalid_credential, _params), do: "failc 5"
  def render(:cant_login, _params), do: "failc 6"
  def render(:user_blacklisted, _params), do: "failc 7"
  def render(:country_blacklisted, _params), do: "failc 8"
  def render(:check_case, _params), do: "failc 9"
  def render(_type, _params), do: "failc 10"

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
