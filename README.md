# ElvenGard

ELF (Elven Lagrangian Flow) is an MMORPG game server mainly developed for Nostale.
Using Kubernetes and Erlang as the backbone for a modern, distributed, highly conciliatory and fault-tolerant system.

## Nostale specific architecture schema

```text
+--------+   Login packet   +---------+  Authentification  +-------+  User credentials  +----------+
|        +------------------>         +-------------------->       +-------------------->          |
|        | Game server list | Bastion |                    | Guard |                    | Postgres |
|        +------------------+         <--------------------+       <--------------------+          |
|        |                  +----+----+                    +-------+                    +----------+
|        |                       |
|        |                Create |
|        |                       |
|        |            +----------v----------+
|        |            |                     |  Broadcast event
| Client |            | Distributed Session <-------------------+
|        |            |                     |                   |
|        |            +----------^----------+                   |
|        |                       |                              |
|        |      Validate session |                              |
|        |                       |                              |
|        |   Connect lobby  +----+----+                    +----+-----+
|        +------------------>         |  Position command  |          |
|        |   Game command   | Citadel +--------------------+ Universe |
|        +------------------>         |                    |          |
+--------+                  +---------+                    +----------+

```
