

ijk_map <- function(input_track,  STG_TRACK_PIECE, STG_TRACK) {

 mappi <-  create_track_table(input_track, STG_TRACK_PIECE, STG_TRACK, extra_slots_to_end = 5)
 aggr_to_slots <- mappi[, .N, by = .(MAXIMUM_MOVEMENT, GAME_SLOT_ID, MINIMUM_MOVEMENT)]
  end_slot <- aggr_to_slots[, .(END_SLOT = GAME_SLOT_ID)]
  card_played <- data.table(movement = c(2,3,4,5,6,7,9))
cj_dt_first <-  CJ.dt(aggr_to_slots, end_slot)
cj_dt <- CJ.dt(cj_dt_first, card_played)
cj_dt[, distance := END_SLOT - GAME_SLOT_ID]
#filter limits
filter <- cj_dt[distance <= 9 & distance >= 2 ]
#adjust_min_max_move
filter_easy <- filter[distance >= MINIMUM_MOVEMENT & distance <= MAXIMUM_MOVEMENT]
filter1 <- filter_easy[(distance == movement) |
                    (distance == MAXIMUM_MOVEMENT & movement >= MAXIMUM_MOVEMENT) |
                       (movement <= MINIMUM_MOVEMENT & distance == MINIMUM_MOVEMENT)]# | (distance == MINIMUM_MOVEMENT & distance == movement)]
# filter1[GAME_SLOT_ID == 23]
# filter1[GAME_SLOT_ID == 27]
sscols <- filter1[, .(GAME_SLOT_ID, END_SLOT, MOVEMENT = movement)]
sscols
}

