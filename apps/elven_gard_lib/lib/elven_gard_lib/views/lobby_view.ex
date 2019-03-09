defmodule ElvenGardLib.LobbyView do
  def render("list_heros", params) do
    [
      "clist_start 0",
      render_heros(params.heros),
      "clist_end"
    ]
  end

  defp render_heros(heros) do
    heros
    |> Enum.map(&(render_hero(&1)))
    |> Enum.join(" ")
  end

  defp render_hero(hero) do
    """
    clist #{hero.slot} \
    #{hero.name} \
    0 \
    #{hero.gender} \
    #{hero.hair_style} \
    #{hero.hair_color} \
    0 \
    #{hero.class} \
    #{hero.level} \
    #{hero.hero_level} \
    #{hero.equipments} \
    #{hero.job_level} \
    1 \
    1 \
    #{hero.pets} \
    0\
    """
  end
end
