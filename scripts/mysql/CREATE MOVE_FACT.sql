

	create table MOVE_FACT (
	MOVE_FACT_ID int auto_increment,
	CYCLER_ID int,
	CARD_ID int,
    START_SLOT_ID int,
    END_SLOT_ID int,
    TURN_ID int,
	PRIMARY KEY (MOVE_FACT_ID));

