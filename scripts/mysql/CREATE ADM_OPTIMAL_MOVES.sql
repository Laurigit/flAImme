#drop table flaimme.ADM_OPTIMAL_MOVES;

create table flaimme.ADM_OPTIMAL_MOVES (
TRACK_LEFT char(80),
DECK_LEFT char(15),
TURNS_TO_FINISH numeric,
DRAW_ODDS VARCHAR(35),
SLOTS_OVER_FINISH numeric,
NEXT_MOVE numeric,
FOLLOWING_MOVE numeric,
PARENT_ID  char(175),
PRIORITY numeric,
SOLUTION char(29),

PRIMARY KEY (TRACK_LEFT(80), DRAW_ODDS(35), DECK_LEFT(15)));

