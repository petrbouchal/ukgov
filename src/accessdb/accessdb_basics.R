## Testing code for connecting to Entities/data infrastructure Access database

# RODBC's odbcAccessConnect only works on 32-bit Win
# This code below from
# http://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window

library(pbtools)
library(RODBC)

conn <- connectAccess('T:/Entity information.accdb')

# Run query - note queries saved inside the DB can be referred to as tables
table <- sqlQuery(conn , paste ("select * from Sources"))
table <- sqlQuery(conn , paste ("select * from Organisations"))
glimpse(table)

### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### DON'T FORGET TO CLOSE THE CONNECTION - OTHERWISE IT LOCKS
### THE DATABASE FOR EVERYONE
##########################################################################
close(conn)
