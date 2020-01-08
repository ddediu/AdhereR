# TODO
  
  
## Error handling

  - better error and messages handling and reporting [OK]


## Processing table

  - the default processings must be defined in the processing table
  
  - use "*" to mean "all" for both ids and classes (it is required to include "*" in the actions)
  
  - use "*" as a class to mean "all other non-matched classes"
  

## Optimisations

  - cache stuff
  
  - read & process several patients at once (in "chunks")?
  
  - parallelise several such chunks? (does it work with all database servers?)


## Check it on MySQL/MariaDB, SQLite and MS SQL

  - make it work on MS SQL as well

  - allow "normal" (i.e. non-ODBC) MS SQL servers
