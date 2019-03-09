defmodule ElvenGardLib.HeroView do
  @hero_genders %{male: 0, female: 1}
  @hero_classes %{neutre: 1}
  @hero_hair_styles %{a: 0, b: 1}
  @hero_hair_colors %{
    cerise: 1,
    red: 9,
    nutmeg: 7,
    saddle: 8,
    raven: 5,
    dixie: 4,
    killarney: 6,
    san_marino: 2,
    affair: 3,
    mauve_taupe: 0
  }

  def render("move_hero", params) do
    """
    at \
    #{params.hero_id} \
    #{params.map_name} \
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
    #{params.hero_name} - \
    #{params.group_id} \
    #{params.family_id} \
    #{params.family_name} \
    #{params.hero_id} \
    #{params.authority} \
    #{Map.get(@hero_genders, params.hero_gender)} \
    #{Map.get(@hero_hair_styles, params.hero_hair_style)} \
    #{Map.get(@hero_hair_colors, params.hero_hair_color)} \
    #{Map.get(@hero_classes, params.hero_class)} \
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
