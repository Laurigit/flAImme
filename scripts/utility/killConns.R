killConns <- function() {

  lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}
