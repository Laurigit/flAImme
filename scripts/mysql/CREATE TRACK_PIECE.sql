#drop table TRACK_PIECE;
use flaimme;
create table TRACK_PIECE (
SLOT_ID  int AUTO_INCREMENT,
TRCACK_PIECE_ID char(10),
PIECE_SLOT int,
LANES int,
PIECE_ATTRIBUTE char(20),
ALIGN char(20),
FINISH bool,
PRIMARY KEY (SLOT_ID));

