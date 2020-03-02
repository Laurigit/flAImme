

boundi_filtteri <- function(i, j, k, max_move_vect, ascend_v, odds_filtter) {

  rividata <- data.table(i, j, k, max_move_vect, ascend_v)
  rividata[, distance := j - i]
  rividata[, tulos_save := !(distance >= 2 & k == distance)]

  rividata[ascend_v > 2 & distance == ascend_v, tulos_save := !(k >= 2 & k <= ascend_v)] #caset, missä pääsee laskeen alamäkeen
  rividata[ascend_v > 2 & distance < ascend_v, tulos_save := TRUE] #mahdoton lasketella alle 5
  rividata[ascend_v > 2 & distance > ascend_v, tulos_save := !(k == distance)] #voi mennä kovempaa ku 5 alamäessä
  #restirct movement
  rividata[k > max_move_vect, tulos_save := TRUE]

  #card value is higher than max_movement. Card value is greater or equal to distance. Then distance on max movement.
  rividata[k > max_move_vect & k >= distance & max_move_vect == distance, tulos_save := FALSE]

  if (!is.null(odds_filtter)) {
    max_odds <- odds_filtter[, max(Turn_to_Draw)]
    prev_round_res <- data.table(j = 1)
    total_res <- NULL
    for (turn_loop in 1:max_odds) {
      # round_rows <- nrow(odds_filtter[Turn_to_Draw == 1])
      keep_or_drop <- odds_filtter[Turn_to_Draw == turn_loop, .N, by = keep_me][, keep_me]
      round_res <- rividata[i %in% prev_round_res[, j] & k %in% odds_filtter[Turn_to_Draw == turn_loop, MOVEMENT] & tulos_save == FALSE]
      #here is invert logic where TRUE means DELETE possibility
      round_res[, drop_me := !keep_or_drop]
      total_res <- rbind(total_res, round_res)
      prev_round_res <- round_res
    }
    sscols_res_all <- total_res[, .(i, j, k, drop_me)][drop_me == TRUE]
    sscols_res <- sscols_res_all[, .(.N), by = .(i, j, k,drop_me)][, N := NULL]
    joini <- sscols_res[rividata, on = .(i, j, k)]
    joini[!is.na(drop_me), tulos_save := TRUE]
  } else {
    joini <- rividata
  }
  #  rividata[, tulos_save := !(((j - i) <= k)  & ((j - i) >= 2))] #either of these cases is true, then it is filterd out
  return(joini[, tulos_save])
}
