
#drop table MOVE_FACT;
	create table MOVE_FACT (
    TOURNAMENT_NM char(50),
    GAME_ID int,
    FIRST_SELECTED bool,
	CYCLER_ID int,
    TEAM_ID int,
	CARD_ID int,
    TURN_ID int,
	PRIMARY KEY (TOURNAMENT_NM, GAME_ID, CYCLER_ID, TURN_ID));

