create table bodies
  ( id integer primary key
  , name text not null
  , radius real not null -- km
  , mass real not null -- kg
  , tilt real not null -- degrees
  , rotationalPeriod real not null -- days
  , colour integer not null -- 32-bit packed
  , eccentricity real not null
  , semimajor real not null -- km
  , longitudeOfAscendingNode real not null -- degrees
  , inclination real not null -- degrees
  , argumentOfPerifocus real not null -- degrees
  , orbitalPeriod real not null -- seconds
  , timeOfPeriapsis real not null -- seconds
  );
