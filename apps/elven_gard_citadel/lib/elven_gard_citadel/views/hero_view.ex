defmodule ElvenGardCitadel.HeroView do
  def render("move_hero", params) do
    """
    at
    #{params.hero_id} \
    #{params.map_name} \
    #{params.x} \
    #{params.y} \
    2 \
    0 \
    #{params.music_id} \
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
    #{params.hero_gender} \
    #{params.hair_style} \
    #{params.hair_color} \
    #{params.hero_class} \
    #{params.dignity} \
    #{params.compliment} \
    #{params.hero_morph} \
    #{if params.invisible?, do: 1, else: 0} \
    #{params.family_level} \
    #{params.hero_sp_upgrade?} \
    #{params.hero_arena_winner?}\
    """
  end
end
