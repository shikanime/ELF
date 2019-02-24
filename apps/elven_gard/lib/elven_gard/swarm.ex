defmodule ElvenGard.Swarm do
  def list_world_entities() do
    Swarm.members(:worlds)
  end

  @doc """
  Publish a message to all members of group `:world`
  """
  def publish_world(msg) do
    Swarm.publish(:world, msg)
  end

  def list_channel_entities() do
    Swarm.members(:channels)
  end

  @doc """
  Publish a message to all members of group `:channel`
  """
  def publish_channel(msg) do
    Swarm.publish(:channel, msg)
  end

  @doc """
  Gets the pid of the entity with the given name
  """
  def get_entity(name) do
    Swarm.whereis_name(name)
  end

  @doc """
  Call some entity by name
  """
  def call_entity(name, msg) do
    GenServer.call({:via, :swarm, name}, msg)
  end

  @doc """
  Cast to some entity by name
  """
  def cast_entity(name, msg) do
    GenServer.cast({:via, :swarm, name}, msg)
  end
end
