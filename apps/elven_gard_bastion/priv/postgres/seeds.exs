ElvenGardBastion.AccountRepo.register_user(%{
  name: "root",
  password: ElvenGard.PasswordCrypto.encrypt("toor")
})
