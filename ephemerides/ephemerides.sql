create table bodies
  ( id integer primary key
  , name text not null
  , radius real not null -- km
  , mass real not null -- kg
  , tilt real not null -- degrees
  , rotationalPeriod real not null -- days
  , colour integer not null -- 32-bit packed
  );
