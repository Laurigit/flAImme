#drop table flaimme.ADM_OPTIMAL_MOVES;

create table flaimme.ADM_OPTIMAL_MOVES (
TRACK_LEFT VARCHAR(110),
DECK_LEFT VARCHAR(30),
TURNS_TO_FINISH numeric,
DRAW_ODDS VARCHAR(35),
SLOTS_OVER_FINISH numeric,
NEXT_MOVE numeric,
PARENT_ID  VARCHAR(175),
PRIMARY KEY (TRACK_LEFT(110), DRAW_ODDS(35), DECK_LEFT(30)));

