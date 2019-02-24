defmodule ElvenGard.LobbyResponse do
  def render("list_characters.nsl", %{characters: characters}) do
    [
      "clist_start 0",
      Enum.map(characters, fn character ->
        """
        clist #{character.slot} #{character.name} 0 \
        #{character.gender} #{character.hair_style} \
        #{character.hair_color} 0 #{character.class} \
        #{character.level} #{character.hero_level} \
        #{character.equipments} #{character.job_level} \
        1 1 #{character.pets} 0
        """
      end),
      "clist_end"
    ]
  end
end
