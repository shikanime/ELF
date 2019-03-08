ElvenGardGuard.Account.register_user(%{
  name: "root",
  password_hash: :crypto.hash(:sha512, "toor") |> Base.encode16() |> Argon2.hash_pwd_salt()
})
