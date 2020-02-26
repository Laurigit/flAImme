
#drop table MOVE_FACT;
	create table MOVE_FACT (
	MOVE_FACT_ID int auto_increment,
	CYCLER_ID int,
	CARD_ID int,
    TURN_ID int,
    GAME_ID int,
	PRIMARY KEY (MOVE_FACT_ID));

