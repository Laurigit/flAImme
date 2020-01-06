convSecsToTimeFormat <- function(sekunnit) {

 kesto_h <- floor(sekunnit / 60 / 60)
 kesto_min <-  floor((sekunnit - kesto_h * 60 * 60) / 60)
  kesto_min_pad <-  str_pad(kesto_min, width = 2, pad = "0")
 kesto_sec <-  str_pad(floor(sekunnit - kesto_h * 60 * 60 - kesto_min * 60), width = 2, pad = "0")
  uuden_videon_alkuaika <-  paste0(kesto_h, ":", kesto_min_pad, ":", kesto_sec)
  return(uuden_videon_alkuaika)
  
}

