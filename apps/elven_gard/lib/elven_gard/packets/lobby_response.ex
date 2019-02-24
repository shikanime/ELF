defmodule ElvenGard.LobbyResponse do
  def render("list_characters.nsl", %{characters: characters}) do
    [
      "clist_start 0",
      Enum.map(characters, &(render_character(&1))),
      "clist_end"
    ]
  end

  def render_character(character) do
    """
    clist #{character.slot} #{character.name} 0 \
    #{character.gender} #{character.hair_style} \
    #{character.hair_color} 0 #{character.class} \
    #{character.level} #{character.hero_level} \
    #{character.equipments} #{character.job_level} \
    1 1 #{character.pets} 0
    """
  end
end
