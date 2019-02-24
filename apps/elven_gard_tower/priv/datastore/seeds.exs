ElvenGardTower.Datastore.Account.register_user(%{
  name: "root",
  # TODO: Put in libary
  password: :crypto.hash(:sha512, "toor") |> Base.encode16(case: :lower) |> String.upcase
})
