
use flaimme;
#drop table DECK_STATUS;
create table DECK_STATUS (
TOURNAMENT_NM char(50),
GAME_ID  int ,
TURN_ID int,
CYCLER_ID int,
CARD_ID int,
Zone char(10),
MOVEMENT  int,
row_id int,
HAND_OPTIONS int,
PRIMARY KEY ( TOURNAMENT_NM, GAME_ID, TURN_ID, row_id, HAND_OPTIONS));



