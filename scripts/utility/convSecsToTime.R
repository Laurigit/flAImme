#convert itime seconds to time

convSecsToTime <- function(seconds, days, tzfix = 0) {
  total <- seconds + days * 60 * 60 * 24
  result <- as.character(anytime(total,tz = "EET") - tzfix)

  return(result)
}
# test <- convSecsToTime(10, as.integer(as.IDate("2018-08-18")))
# #M_9_2018-08-18_73167
# 
# convSecsToTime(74542,17761)
# 	
