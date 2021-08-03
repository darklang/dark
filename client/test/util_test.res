open Tester
open Util

let run = () => {
  describe("obscureString", () => {
    test("string is empty", () => expect(obscureString("")) |> toEqual(""))
    test("string len = 3", () => expect(obscureString("ace")) |> toEqual("XXX"))
    test("string len = 5", () => expect(obscureString("alice")) |> toEqual("XXXXe"))
    test("string len = 8", () => expect(obscureString("abcd1234")) |> toEqual("XXXXXX34"))
    test("string len = 10", () => expect(obscureString("a1b2c3d4e5")) |> toEqual("XXXXXXXXe5"))
    test("string len = 16", () =>
      expect(obscureString("abcd1234efgh0987")) |> toEqual("XXXXXXXXXXXX0987")
    )
    test("string len > 16", () =>
      expect(obscureString("abc-123-def-456-ghi-789-xyz")) |> toEqual("XXXXXXXXXXXXXXXXXXXXXXX-xyz")
    )
    ()
  })
  describe("hideSecrets", () => {
    let secrets = list{"abc[123]-\\bunnies", "XSUYD-JFKJWD-NKFADS"}
    test("replaces a secret", () =>
      expect(hideSecrets(secrets, "XSUYD-JFKJWD-NKFADS")) |> toEqual("XXXXXXXXXXXXXXXFADS")
    )
    test("replaces a in substring", () =>
      expect(hideSecrets(secrets, "Bearer XSUYD-JFKJWD-NKFADS")) |> toEqual(
        "Bearer XXXXXXXXXXXXXXXFADS",
      )
    )
    test("replaces secret with regex like characters", () =>
      expect(hideSecrets(secrets, "abc[123]-\\bunnies")) |> toEqual("XXXXXXXXXXXXXnies")
    )
    test("replaces all of the same secret", () =>
      expect(
        hideSecrets(
          secrets,
          "{ \"token\" : \"XSUYD-JFKJWD-NKFADS\", \"auth\" : \"XSUYD-JFKJWD-NKFADS\" }",
        ),
      ) |> toEqual("{ \"token\" : \"XXXXXXXXXXXXXXXFADS\", \"auth\" : \"XXXXXXXXXXXXXXXFADS\" }")
    )
    test("replaces multiple different secrets", () =>
      expect(
        hideSecrets(
          secrets,
          "{ \"token\" : \"XSUYD-JFKJWD-NKFADS\", \"secret\" : \"abc[123]-\\bunnies\" }",
        ),
      ) |> toEqual("{ \"token\" : \"XXXXXXXXXXXXXXXFADS\", \"secret\" : \"XXXXXXXXXXXXXnies\" }")
    )
  })
  ()
}
