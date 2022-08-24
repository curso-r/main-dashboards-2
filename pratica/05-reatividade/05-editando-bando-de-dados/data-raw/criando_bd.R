con <- RSQLite::dbConnect(
  RSQLite::SQLite(), 
  "pratica/05-reatividade/05-editando-bando-de-dados/mtcars.sqlite"
)

RSQLite::dbWriteTable(con, "mtcars", mtcars)

RSQLite::dbDisconnect(con)
