module Params = {
  type t = SettingsViewTypes.inviteFormMessage

  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("email", string(params.email)),
      ("inviterUsername", string(params.inviterUsername)),
      ("inviterName", string(params.inviterName)),
    })
  }
}
