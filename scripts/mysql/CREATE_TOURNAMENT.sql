use flaimme;
#drop table TOURNAMENT;
create table TOURNAMENT (
TOURNAMENT_NM char(50),
TEAM_ID int,
PLAYER_NM char(20),
PLAYER_TYPE char (20),
PRIMARY KEY (TOURNAMENT_NM, TEAM_ID));

