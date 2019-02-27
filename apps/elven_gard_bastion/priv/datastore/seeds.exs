ElvenGardBastion.AccountRepo.register_user(%{
  name: "root",
  password: ElvenGardStdlib.PasswordCrypto.encrypt("toor")
})
