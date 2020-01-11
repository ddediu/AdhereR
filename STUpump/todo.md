# TODO
  
  
## General

  - [*NOT NECESSARY*] make sure get(...) has a version for data.table (i.e., sometimes the attribute's SQL value might not work as an attribute for data.frame!)
  
  - [*IN PROGRESS*] refactor so that the database-specific stuff is better insulated from the algortihms

  
## Error handling

  - [*DONE*] better error and messages handling and reporting


## Processing table

  - [*DONE*] the default processings must be defined in the processing table
  
  - [*DONE*] use "*" to mean "all" for both ids and classes (it is required to include this in the actions)
  
  - [*DONE*] use "*" as a class to mean "all other non-matched classes"
  

## Optimisations

  - cache stuff
  
  - read & process several patients at once (in "chunks")?
  
  - parallelise several such chunks? (does it work with all database servers?)


## Check it on MySQL/MariaDB, SQLite and MS SQL

  - make it work on MS SQL as well

  - allow "normal" (i.e. non-ODBC) MS SQL servers