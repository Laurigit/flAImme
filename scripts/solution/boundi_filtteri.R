

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
    #first round


      round_res <- rividata[i == 1 & k %in% odds_filtter[Turn_to_Draw == 1 & keep_me == FALSE, MOVEMENT] & tulos_save == FALSE]

      #possible 2nd turn start positions
      start_posses <- odds_filtter[Turn_to_Draw == 1 & keep_me == TRUE, MOVEMENT] + 1
      start_posses <- rividata[tulos_save == FALSE & i == 1 & k %in% odds_filtter[Turn_to_Draw == 1 & keep_me == TRUE, MOVEMENT], j]

      round_res2 <- rividata[i %in% start_posses & k %in% odds_filtter[Turn_to_Draw == 2 & keep_me == FALSE, MOVEMENT] & tulos_save == FALSE]
      total_res <- rbind(round_res2, round_res)
      total_res[, drop_me := TRUE]

    sscols_res_all <- total_res[, .(i, j, k, drop_me)][drop_me == TRUE]
    sscols_res <- sscols_res_all[, .(.N), by = .(i, j, k,drop_me)][, N := NULL]
    joini <- sscols_res[rividata, on = .(i, j, k)]
    joini[!is.na(drop_me), tulos_save := TRUE]
  } else {
    joini <- rividata
  }
  finish_slot <- joini[, max(j)]
  #tällä pääsee täsmälleen maaliin
  joini[j == finish_slot & pmax(ascend_v, k) >= distance & distance <= max_move_vect, tulos_save := FALSE]
  joini[k == 100, tulos_save := FALSE]
  #  rividata[, tulos_save := !(((j - i) <= k)  & ((j - i) >= 2))] #either of these cases is true, then it is filterd out
  return(joini[, tulos_save])
}
