use flaimme;
#drop table GAME_STATUS;
create table GAME_STATUS (
TOURNAMENT_NM char(50),
GAME_ID int,
CYCLER_ID int,
SQUARE_ID int,
TURN_ID int,
PRIMARY KEY (TOURNAMENT_NM, TURN_ID, GAME_ID));
