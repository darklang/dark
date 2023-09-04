module Uuid

let nilNamespace : System.Guid = System.Guid "00000000-0000-0000-0000-000000000000"

let uuidV5 (data : string) (nameSpace : System.Guid) : System.Guid =
  Faithlife.Utility.GuidUtility.Create(nameSpace, data, 5)
