
use flaimme;
#drop table DECK_STATUS;
create table DECK_STATUS (
GAME_ID  int ,
TURN_ID int,
CYCLER_ID int,
CARD_ID int,
Zone char(10),
PRIMARY KEY (GAME_ID, TURN_ID, CYCLER_ID))
;



