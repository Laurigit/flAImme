#POPULATE TEST DATA

#CARD

cards <- data.table(CARD_ID = c(1, 2,3,4,5,6,7,9), MOVEMENT = c(2,2,3,4,5,6,7,9), SKILL_ID = 0)
#dbWriteTable(con, "CARD", cards, row.names = FALSE, append = TRUE)

#TRACK PIECE
pieses <- data.table(read_excel("./external_files/track_pieces.xlsx"))

#dbWriteTable(con, "TRACK_PIECE", pieses, row.names = FALSE, append = TRUE)
#dbWriteTable(con, "TRACK_PIECE", pieses, row.names = FALSE, append = TRUE)
#CYCLER

cycl <- data.table(CYCLER_ID =
                     1:12,
                   CYCLER_TYPE_ID = c(1, 2),
                   TEAM_ID = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))

#dbWriteTable(con, "CYCLER", cycl, row.names = FALSE, append = TRUE)


#TEAM

tiimi <- data.table(  TEAM_ID = 1:6,
                      TEAM_COLOR = c("Red", "Blue", "Black", "Green",
                                     "Purple","White"))

#dbWriteTable(con, "TEAM", tiimi, row.names = FALSE, append = TRUE)

typec <- data.table(CYCLER_TYPE_ID = c(1, 2),
                    CYCLER_TYPE_NAME = c("Rouler", "Sprinteur"),
                    DECK_ID = c(1, 2))
#dbWriteTable(con, "CYCLER_TYPE", typec, row.names = FALSE, append = TRUE)



DECK  <- data.table(DECK_ID = c(1, 1, 1, 1, 1,
                                2, 2, 2, 2, 2),
                    CARD_ID = c(3, 4, 5, 6, 7,
                                2, 3, 4, 5, 9),
                    Count = 3)
#dbWriteTable(con, "DECK", DECK, row.names = FALSE, append = TRUE)



PLAYER <- data.table(PLAYER_ID = c(1, 2, 3),
                     PLAYER_NM = c("Lauri", "AI", "Kone"))

#dbWriteTable(con, "PLAYER", PLAYER, row.names = FALSE, append = TRUE)



GAME <- data.table(  GAME_ID = c(1, 1, 1, 1, 1, 1),
                     TRACK_ID = c(1, 1, 1, 1, 1, 1),
                     CYCLER_ID = c(1, 2, 3, 4, 5, 6),
                     PLAYER_ID = c(1, 1, 2, 2, 3, 3))

#dbWriteTable(con, "GAME", GAME, row.names = FALSE, append = TRUE)

#TRACK

tracki <- data.table(TRACK_ID = 2,
                     TRACK_NAME = "OMA",
                     TRACK_PIECE_VECTOR = paste0("a,", "b,", "c,", "d,", "e,", "F,",
                                                 "g,", "h,", "i,", "j,", "k,", "l,", "m,", "N,", "o,", "p,", "q,", "r,", "s,", "t,", "u"))
#tracki <- data.table(read_excel("./external_files/track_pieces.xlsx", sheet = "Sheet2"))[!is.na(TRACK_PIECE_VECTOR)]

#dbWriteTable(con, "TRACK", tracki, row.names = FALSE, append = TRUE)
