use flaimme;
SET SQL_SAFE_UPDATES = 0;
DELETE FROM BREAKAWAY_BET where 1 = 1;
DELETE FROM TOURNAMENT_RESULT where 1 = 1;
DELETE FROM DECK_STATUS where 1 = 1;
DELETE FROM DECK_STATUS where HAND_OPTIONS = 1;
DELETE FROM BREAKAWAY_BET_CARDS where 1 = 1;
DELETE FROM MOVE_FACT where 1 = 1;
DELETE FROM MOVE_FACT where TURN_ID = 4;
DELETE FROM GAME_STATUS where 1 = 1;#TURN_ID= 0;
DELETE FROM CLIENT_COMMANDS  where 1 = 1;
#DELETE FROM TOURNAMENT where 1 = 1;