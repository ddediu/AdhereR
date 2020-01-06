# TODO


## "Complex" CMAs

  - in the results table, make the CMA numeric for the "simple" CMAs  and a flag for "complex" CMAs + pointer to the range of rows in a dedicated table (one for sliding windows and one for per episodes)
  
  - in the results table, add an AUTOINCREMENT PRIMARY KEY column (to which we refer back from the sliding windows and per episode records) --  https://www.w3schools.com/sql/sql_autoincrement.asp
  
  - in the sliding windows and per episode table make sure there's an AUTOINCREMENT PRIMARY KEY column to which we refer from the results table --  https://www.w3schools.com/sql/sql_autoincrement.asp


## Processing table

  - the default processings must be defined in the processing table
  
  - use "*" to mean "all" for both ids and classes
  
  - allow the specific call of the default rule for those formulas that have a more sepcific rule (by default, the default rule does not apply in these cases)
  
  - split this into two tables: one defining the processings and one their application


## Config file

  - make the config file XML instead of TSV ???


## Optimisations

  - cache stuff
  
  - read & process several patients at once (in "chunks")?
  
  - parallelise several such chunks? (does it work with all database servers?)


## Check it on MySQL/MariaDB, SQLite and MS SQL

  - make it work on MS SQL as well

  - allow "normal" (i.e. non-ODBC) MS SQL servers
