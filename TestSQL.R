# Packages
require(odbc)
require(DBI)
require(tidyverse)

# Database connection
cnn <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server Native Client 11.0",
                 Server = "DFBCV9TWVASP001\\SQLEXPRESS16",
                 Database = "Herring",
                 Trusted_Connection = "Yes")

db_info <- list(schema = "Program", table = "VegTrans")
sql <- paste("SELECT", "*",
             "FROM", paste(db_info$schema, db_info$table, sep = ".") )

# Execute the query
df <- dbGetQuery(conn = cnn, statement = sql) %>%
  as_tibble()

# Get the table
# df <- dbReadTable(
#   conn = cnn, name = Id(schema = "Biosample", table = "Lookup_Gear")
# ) %>%
#   as_tibble()

# Close the connection
dbDisconnect(conn = cnn)

# Print the table
print(df)
