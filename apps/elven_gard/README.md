# ElvenGard

This module's goal is to provide multi-level and multi-verse spacial
positionning for abstract intactable entities.

Designed to be massively distributed scallable horizontally for the purpose of
the development of massive multiplayer game

This module have 3 main type of levels which will be described.

## Design

```text
Virtual World

+-----------+
|           |
|  Channel  +------+
|           |      |
+-----------+      |
                   |
                   |
Physical World     |
                   |
+---------+    +---v---+    +----------+    +------------+
|         |    |       |    |          |    |            |
|  World  +---->  Map  +---->  Entity  +---->  Position  |
|         |    |       |    |          |    |            |
+---------+    +-------+    +----------+    +------------+

```

## Layers

### World

This layer is designed as the highest group level of the 3 and
is designed as a namespace por every lower level layers.

### Map

Map are the layer responsible to handle the positionning of every entities.
And are intended to be sharded arbitrary.

### Channel

Channel are virtual layer which is responsible split the presence of entities
into map. But let entities and functionnality working between channel.

