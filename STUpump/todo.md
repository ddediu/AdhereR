# TODO
  

## Bugs

  - make sure the proc_id used in the main results refer to the original in_procs table and not the tmp_in_procs (if any)!

  
## General

  - [*NOT NECESSARY*] make sure get(...) has a version for data.table (i.e., sometimes the attribute's SQL value might not work as an attribute for data.frame!)
  
  - [*IN PROGRESS*] refactor so that the database-specific stuff is better insulated from the algortihms
  
  - [*OK*] function for fully qualifed database and attribute name (to avoind long concatenations of specific calls)
  
  - [*OK*] function for checking package, installing and loading it
  
  - [*OK*] make sure the escape character "\" works as intended
  
  - [*NOT NECESSARY*] use a proper parser for the medication classes
  
  - do the preprocessing of the classes, actions and processings anyways and store them; make sure the id's used are those in the original tables!
  
  - do the parsing and resolution of the classes and actions once during the preprocessing
  
  - [*OK*] add in the config file a parameter specifying if the default processings * apply on top of more specific rules or not
  
  - [*OK*] also allow for:
  
    - "==": "IS", "is", "EQUAL", "equal", "="
    - "!=": "IS NOT", "is not", "NOT EQUAL", "not equal", "DIFFERENT", "different", "<>", "≠"
    - "&": "AND", "and", "&&"
    - "|": "OR", "or", "||"
    - "!": "NOT", "not", "^", "~", "¬"
    - "#": "DOSE", "dose"
    - "<": "LESS THAN", "less than", "LE", "le"
    - "<=": "LESS THAN OR EQUAL", "less than or equal", "LEQ", "leq", "≤"
    - ">": "GREATER THAN", "greater than", "GE", "ge"
    - ">=": "GREATER THAN OR EQUAL", "greater than or equal", "GEQ", "geq", "≥"
    - "{...}": "REF(...)"
    - "?": "OTHERWISE", "otherwise", "ELSE", "else"
    - "*": "ALL", "all", "NONE", "none", "DEFAULT", "default" (depending on its exact context, i.e., table and column)
  
  
## Plotting

  - better vertical size especially for a small number of events

  
## Error handling

  - [*DONE*] better error and messages handling and reporting
  
  
## Medication classes

  - [*OK*] use # for *dosage* and also add numeric comparisons
  
  - [*OK*] add stand-alone # for dosage for any medication


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


## Testing

  - write some simple unit tests
  
