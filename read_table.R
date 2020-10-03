# Read in any table from FBref

require(tidyverse)
require(rvest)
require(dplyr)

# All urls to read
urls <- c(
  "https://fbref.com/en/comps/9/3232/keepersadv/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/shooting/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/passing/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/passing_types/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/gca/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/defense/2019-2020-Premier-League-Stats",
  "https://fbref.com/en/comps/9/3232/possession/2019-2020-Premier-League-Stats"
)

# All tables to read
tables <- c(
  "%23stats_keeper_adv",
  "%23stats_shooting",
  "%23stats_passing",
  "%23stats_passing_types",
  "%23stats_gca",
  "%23stats_defense",
  "%23stats_possession"
)

# Function to read table
read_fbref <- function(url, table) {
  # Format URL
  url_pasted <-
    paste0(
      "http://acciotables.herokuapp.com/?page_url=",
      url,
      "&content_selector_id=",
      table
    )
  # Read table from html
  table_output <- url_pasted %>% read_html() %>% html_table()
  # Extract first element of list
  dataframe_output <- table_output[[1]]
  # Collapse headers
  names(dataframe_output) <- trimws(paste(names(dataframe_output), dataframe_output[1, ]))
  # Remove any repeat headers
  dataframe_output <- dataframe_output %>% 
    filter(Player != "Player")
  # Change to tibble
  dataframe_output <- as_tibble(dataframe_output) %>% select(-Matches)
  #Return
  return(dataframe_output)
}

# Read in all tables
all_tables <- vector("list", 7)
for (i in 1:nrow(table_list)) {
  all_tables[[i]] <- read_fbref(urls[i], tables[i])
}

# Output goalkeeper and outfielder dataframes
goalkeepers <- all_tables[[1]]
outfielders <- bind_cols(all_tables[2:7]) %>% 
  select(-grep("[1-9]$", colnames(.)))

# Save data to package directory
save(goalkeepers, file = "goalkeepers.rda")
save(outfielders, file = "outfielders.rda")