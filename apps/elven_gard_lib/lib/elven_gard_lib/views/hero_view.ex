defmodule ElvenGardLib.HeroView do
  alias ElvenGardLib.HeroSerializer

  def render("move_hero", params) do
    """
    at \
    #{params.hero_id} \
    #{Recase.to_pascal(params.map_name)} \
    #{params.hero_position_x} \
    #{params.hero_position_y} \
    2 \
    0 \
    #{params.map_music_id} \
    1 \
    -1\
    """
  end

  def render("sync_hero", params) do
    """
    c_info \
    #{Recase.to_snake(params.hero_name)} - \
    #{params.group_id} \
    #{params.family_id} \
    #{Recase.to_snake(params.family_name)} \
    #{params.hero_id} \
    #{params.authority} \
    #{HeroSerializer.hero_genders(params.hero_gender)} \
    #{HeroSerializer.hero_hair_styles(params.hero_hair_style)} \
    #{HeroSerializer.hero_hair_colors(params.hero_hair_color)} \
    #{HeroSerializer.hero_classes(params.hero_class)} \
    #{params.dignity} \
    #{params.compliment} \
    #{params.hero_morph} \
    #{if params.hero_invisible?, do: 1, else: 0} \
    #{params.family_level} \
    #{if params.hero_sp_upgrade?, do: 1, else: 0} \
    #{if params.hero_arena_winner?, do: 1, else: 0}\
    """
  end
end
