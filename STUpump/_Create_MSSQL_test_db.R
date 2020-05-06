###############################################################################################
#
#    Create the MS SQL Server database and tables used for testing the STU pump.
#
#    I am using the default setup on the test environment.
#    The connection, username and password are stored in a separate file not on GitHub"
#    ./server_credentials_STUpump.tsv
#    
#
###############################################################################################

# Use



library("RODBC"); 









library("RODBC")
db <- odbcConnect("MSSQLTest","adherer","[4dh3r3r]")

# List databases:
sqlQuery(db, "SELECT name FROM master.sys.databases");

# Create databse:
sqlQuery(db, "CREATE DATABASE STUpumpTests;");

# Remove table (if already created):
sqlQuery(db, "DROP TABLE [STUpumpTests].[dbo].[med_events]");

# Create and populate the med_events table:
sqlQuery(db, "CREATE TABLE [STUpumpTests].[dbo].[med_events] ( id INT NOT NULL,
                                                               date DATE NOT NULL,
                                                               perday INT NOT NULL,
                                                               category VARCHAR(45) NOT NULL, 
                                                               duration INT NOT NULL);");

# Clear it:
sqlQuery(db, "TRUNCATE TABLE [STUpumpTests].[dbo].[med_events];");
# Fill it in one by one (for some reason, saving the whole data.frame seems not to be working):
for( i in 1:nrow(med.events) )
{
  sqlQuery(db, paste0("INSERT INTO [STUpumpTests].[dbo].[med_events] VALUES (",
                      paste0("'",as.character(med.events[i,]),"'",collapse=","),
                      ");"));
}

# List tabe:
sqlQuery(db, "SELECT * FROM [STUpumpTests].[dbo].[med_events];")







# Original code:
library("RODBC")
db <- odbcConnect("Sandbox.albasoft.co.uk\\MSSQLTest","adherer","[4dh3r3r]")
sql   <- "SELECT *  FROM [TestDB].[dbo].[ReadDict] order by Code"
df1   <- sqlQuery(db, sql)
View(df1)


