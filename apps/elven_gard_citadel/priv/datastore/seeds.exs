ElvenGardCitadel.Datastore.Account.register_user(%{
  name: "root",
  # TODO: Put in libary
  password: ElvenGardStdlib.PasswordCrypto.encrypt("toor")
})
