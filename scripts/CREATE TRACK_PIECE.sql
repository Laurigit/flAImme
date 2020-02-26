use flaimme;
#drop table TRACK_PIECE;
create table TRACK_PIECE (
SLOT_ID  int AUTO_INCREMENT,
TRACK_PIECE_ID char(10),
PIECE_SLOT int,
LANES int,
PIECE_ATTRIBUTE char(20),
ALIGN char(20),
START bool,
FINISH bool,
PRIMARY KEY (SLOT_ID));

