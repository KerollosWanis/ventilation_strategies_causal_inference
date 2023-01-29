#install.packages("bigrquery")
library(bigrquery)
library(tidyverse)

#Pull data from BigQuery
bigrquery::bq_auth()   ##0 to reset the token/email login (tick all boxes)

sql <- "SELECT * FROM `mech-vent-data.mv.mechanical-vent`
ORDER BY subject_id, stay_id, hr"


# Run the query and store the data in a tibble
query <- bq_project_query("mech-vent-data", sql)  ##yes login initiated here
cohort <- bq_table_download(query) %>%
  arrange(., subject_id, hr) 
save(cohort, file='cohort.Rdata')
