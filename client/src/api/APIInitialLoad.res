module PT = ProgramTypes
module RT = RuntimeTypes

@ppx.deriving(show({with_path: false}))
type rec t = {
  handlers: list<PT.Handler.t>,
  deletedHandlers: list<PT.Handler.t>,
  dbs: list<PT.DB.t>,
  deletedDBs: list<PT.DB.t>,
  userFunctions: list<PT.UserFunction.t>,
  deletedUserFunctions: list<PT.UserFunction.t>,
  userTypes: list<PT.UserType.t>,
  deletedUserTypes: list<PT.UserType.t>,
  unlockedDBs: TLID.Set.t,
  staticDeploys: list<StaticAssets.Deploy.t>,
  permission: option<AccountTypes.Permission.t>,
  opCtrs: Tc.Map.String.t<int>,
  account: AccountTypes.Account.t,
  canvasList: list<string>,
  orgs: list<string>,
  orgCanvasList: list<string>,
  workerSchedules: Tc.Map.String.t<string>,
  secrets: list<SecretTypes.t>,
  @opaque creationDate: Js.Date.t,
}

let decode = (j): t => {
  open Json_decode_extended
  {
    handlers: field("handlers", list(PT.Handler.decode), j),
    deletedHandlers: field("deletedHandlers", list(PT.Handler.decode), j),
    dbs: field("dbs", list(PT.DB.decode), j),
    deletedDBs: field("deletedDBs", list(PT.DB.decode), j),
    userFunctions: field("userFunctions", list(PT.UserFunction.decode), j),
    deletedUserFunctions: field("deletedUserFunctions", list(PT.UserFunction.decode), j),
    userTypes: field("userTypes", list(PT.UserType.decode), j),
    deletedUserTypes: field("deletedUserTypes", list(PT.UserType.decode), j),
    unlockedDBs: field("unlockedDBs", TLID.Set.decode, j),
    staticDeploys: field("staticDeploys", list(StaticAssets.Deploy.decode), j),
    permission: field("permission", optional(AccountTypes.Permission.decode), j),
    opCtrs: field("opCtrs", strListDict(int), j),
    account: field("account", AccountTypes.Account.decode, j),
    canvasList: field("canvasList", list(string), j),
    orgs: field("orgs", list(string), j),
    orgCanvasList: field("orgCanvasList", list(string), j),
    workerSchedules: field("workerSchedules", strDict(string), j),
    creationDate: field("creationDate", date, j),
    secrets: field("secrets", list(SecretTypes.decode), j),
  }
}

let encode = (l: t): Js.Json.t => {
  open Json_encode_extended
  object_(list{
    ("handlers", list(PT.Handler.encode, l.handlers)),
    ("deletedHandlers", list(PT.Handler.encode, l.deletedHandlers)),
    ("dbs", list(PT.DB.encode, l.dbs)),
    ("deletedDBs", list(PT.DB.encode, l.deletedDBs)),
    ("userFunctions", list(PT.UserFunction.encode, l.userFunctions)),
    ("deletedUserFunctions", list(PT.UserFunction.encode, l.deletedUserFunctions)),
    ("userTypes", list(PT.UserType.encode, l.userTypes)),
    ("deletedUserTypes", list(PT.UserType.encode, l.deletedUserTypes)),
    ("unlockedDBs", TLID.Set.encode(l.unlockedDBs)),
    ("staticDeploys", list(StaticAssets.Deploy.encode, l.staticDeploys)),
    ("permission", nullable(AccountTypes.Permission.encode, l.permission)),
    ("opCtrs", strListDict(int, l.opCtrs)),
    ("account", AccountTypes.Account.encode(l.account)),
    ("canvasList", list(string, l.canvasList)),
    ("orgs", list(string, l.orgs)),
    ("orgCanvasList", list(string, l.orgCanvasList)),
    ("workerSchedules", strDict(string, l.workerSchedules)),
    ("creationDate", date(l.creationDate)),
    ("secrets", list(SecretTypes.encode, l.secrets)),
  })
}
