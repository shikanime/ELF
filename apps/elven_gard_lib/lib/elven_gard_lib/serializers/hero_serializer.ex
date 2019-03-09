defmodule ElvenGardLib.HeroSerializer do
  def hero_class(:adventurer), do: 0
  def hero_class(:sorcerer), do: 1
  def hero_class(:archer), do: 2
  def hero_class(:swordsman), do: 3
  def hero_class(:martial_artist), do: 4

  def hero_hair_styles(:a), do: 0
  def hero_hair_styles(:b), do: 1

  def hero_hair_colors(:cerise), do: 1
  def hero_hair_colors(:red), do: 9
  def hero_hair_colors(:nutmeg), do: 7
  def hero_hair_colors(:saddle), do: 8
  def hero_hair_colors(:raven), do: 5
  def hero_hair_colors(:dixie), do: 4
  def hero_hair_colors(:killarney), do: 6
  def hero_hair_colors(:san_marino), do: 2
  def hero_hair_colors(:affair), do: 3
  def hero_hair_colors(:mauve_taupe), do: 0

  def hero_gender(:male), do: 0
  def hero_gender(:female), do: 1
end
