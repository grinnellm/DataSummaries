# Packages
require(tidyverse)
require(DBI)
require(odbc)

# Database connection
# cnn <- dbConnect(
#   odbc::odbc(),
#   Driver             = "SQL Server Native Client 11.0",
#   Server             = "DFBCV9TWVASP001\\SQLEXPRESS16",
#   Database           = "Herring",
#   Trusted_Connection = "Yes"
# )

# Database connection
cnn <- dbConnect(
  odbc::odbc(),
  driver   = "SQL Server",
  server   = "DFBCV9TWVASP003",
  database = "Herring",
  uid      = "HerringUser",
  pwd      = "H3rr1ngUs3r"
)

# Database info
db_info <- list(
  schema = "dbo", table = "Sections", columns = c("SAR", "Section")
)

# SQL statement
sql <- paste(
  "SELECT", paste(db_info$columns, collapse = ", "), "FROM",
  paste(db_info$schema, db_info$table, sep = ".") 
)

# Execute the query
dat <- dbGetQuery(conn = cnn, statement = sql) %>%
  as_tibble()

# Get the table
# df <- dbReadTable(
#   conn = cnn, name = Id(schema = "Biosample", table = "Lookup_Gear")
# ) %>%
#   as_tibble()

# Close the connection
dbDisconnect(conn = cnn)

# Print the table
print(dat)
