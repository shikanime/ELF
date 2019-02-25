defmodule ElvenGardStdlib.CharacterView do
  def render(
        "move_character.nsw",
        %{
          id: id,
          map_name: map_name,
          position_x: position_x,
          position_y: position_y,
          music_id: music_id
        }
      ) do
    "at #{id} #{map_name} #{position_x} #{position_y} 2 0 #{music_id} 1 -1"
  end

  def render(
        "spawn_character.nsw",
        %{
          id: id,
          name: name,
          gender: gender,
          hair_style: hair_style,
          hair_color: hair_color,
          class: class,
          group_id: group_id,
          family_id: family_id,
          family_name: family_name,
          family_level: family_level,
          authority: authority,
          dignity: dignity,
          compliment: compliment,
          morph: morph,
          invisible: invisible,
          # TODO: document this param
          sp_upgrade: sp_upgrade,
          arena_winner: arena_winner
        }
      ) do
    """
    c_info #{name} - #{group_id} #{family_id} #{family_name} \
    #{id} #{authority} #{gender} #{hair_style} \
    #{hair_color} #{class} #{dignity} #{compliment} #{morph} \
    #{if invisible, do: 1, else: 0} #{family_level} #{sp_upgrade} #{arena_winner}\
    """
  end
end
