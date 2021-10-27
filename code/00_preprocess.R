# Script to take a first look into the data

# Libaries --------------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Read and join ---------------------------------------------------------------
# This is what we have. Lets read them all and join them into a dataframe
filenames <- list.files("data/raw/", full.names = TRUE)
list_of_files <- lapply(filenames, read.csv)

# Set all accident number to character so we can join later the tables
list_of_files <- lapply(
  list_of_files, 
  function(x) x %>% mutate(Num_Acc = as.character(Num_Acc))
  )


# Unify in a single dataframe
raw_data <- list_of_files[[1]] %>% 
  full_join(list_of_files[[2]], by = "Num_Acc") %>% 
  full_join(list_of_files[[3]], by = "Num_Acc") %>% 
  full_join(list_of_files[[4]], by = "Num_Acc")

# Save it so we don't have to run this over and over again
dim(raw_data)  # 250k x 56

write.csv(raw_data, "data/raw/full_data.csv", row.names = FALSE)

# Preprocessing ---------------------------------------------------------------

# clean environment except from raw data 
rm(list = setdiff(ls(), "raw_data"))

  