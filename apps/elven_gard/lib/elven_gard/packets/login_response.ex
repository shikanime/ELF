defmodule ElvenGard.LoginResponse do
  def render("loging_success.nsl", %{user_id: user_id, client_id: client_id, server_status: server_status}) do
    "NsTeST #{user_id} #{client_id} #{Enum.join(server_status, " ")} -1:-1:-1:10000.10000.1"
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
end
