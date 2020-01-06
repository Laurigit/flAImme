#read_posi
read_posi <- function(input_posi) {
  result <- as.POSIXct(input_posi, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  return(result)
}
