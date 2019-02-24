defmodule ElvenGard.PositionResponse do
  def render("place_character.nsw", %{character_id: character_id, map_name: map_name, position_x: position_x, position_y: position_y, music_id: music_id}) do
    "at #{character_id} #{map_name} #{position_x} #{position_y} 2 0 #{music_id} 1 -1"
  end
end
