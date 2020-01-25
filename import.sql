-- !preview conn=DBI::dbConnect(RSQLite::SQLite())
CREATE TABLE trainData (event_id,game_session,timestamp,event_data,installation_id,event_count,event_code,game_time,title,type,world);
.separator ,
.import train.csv trainData