ALTER TABLE canvases
ADD COLUMN cors_setting json -- NULL -> None, json array -> Some (Origins [...]), json string "*" -> Some AllOrigins
