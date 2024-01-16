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

# Execute the query
# df <- dbGetQuery(
#   conn = cnn, statement = "SELECT * FROM Biosample.Lookup_Gear"
# ) %>%
#   as_tibble()

# Get the table
df <- dbReadTable(
  conn = cnn, name = Id(schema = "Biosample", table = "Lookup_Gear")
) %>%
  as_tibble()

# Close the connection
dbDisconnect(conn = cnn)

# Print the table
print(df)
