# TODO
  
  
## General

  - [*NOT NECESSARY*] make sure get(...) has a version for data.table (i.e., sometimes the attribute's SQL value might not work as an attribute for data.frame!)
  
  - [*IN PROGRESS*] refactor so that the database-specific stuff is better insulated from the algortihms
  
  - [*OK*] function for fully qualifed database and attribute name (to avoind long concatenations of specific calls)
  
  - [*OK*] function for checking package, installing and loading it
  
  
## Plotting

  - better vertical size especially for a small number of events

  
## Error handling

  - [*DONE*] better error and messages handling and reporting
  
  
## Medication classes

  - use ^() for *dosage* and also add numeric comparisons
  
  - add ^(*) for dosage for any medication


## Processing table

  - [*DONE*] the default processings must be defined in the processing table
  
  - [*DONE*] use "*" to mean "all" for both ids and classes (it is required to include this in the actions)
  
  - [*DONE*] use "*" as a class to mean "all other non-matched classes"
  

## Optimisations

  - cache stuff
  
  - read & process several patients at once (in "chunks")?
  
  - parallelise several such chunks? (does it work with all database servers?)


## Check it on MySQL/MariaDB, SQLite and MS SQL

  - [*OK*] test on MySQL, MS SQL and SQLite

  - allow "normal" (i.e. non-ODBC) MS SQL servers
  
  
