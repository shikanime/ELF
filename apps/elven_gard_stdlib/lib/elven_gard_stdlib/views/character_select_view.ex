defmodule ElvenGardStdlib.CharacterSelectView do
  def render("list_characters.nsl", %{characters: characters}) do
    [
      "clist_start 0",
      render_characters(characters),
      "clist_end"
    ]
  end

  defp render_characters(characters) do
    characters
    |> Enum.map(&(render_character(&1)))
    |> Enum.join(" ")
  end

  defp render_character(character) do
    """
    clist #{character.slot} #{character.name} 0 \
    #{character.gender} #{character.hair_style} \
    #{character.hair_color} 0 #{character.class} \
    #{character.level} #{character.hero_level} \
    #{character.equipments} #{character.job_level} \
    1 1 #{character.pets} 0\
    """
  end
end
