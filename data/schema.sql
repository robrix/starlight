-- schema for factions & their relationships
create table factions
  ( name text not null
  , colour integer not null
  );

create table relationships
  ( factionId1 integer not null
  , factionId2 integer not null
  , relationship real not null
  );
