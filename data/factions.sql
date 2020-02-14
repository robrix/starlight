drop table if exists factions;

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

insert into factions values
  ( "venusian"
  , 0xffff80ff
  );

insert into factions values
  ( "terran"
  , 0x0000ffff
  );

insert into factions values
  ( "martian"
  , 0xff0000ff
  );
