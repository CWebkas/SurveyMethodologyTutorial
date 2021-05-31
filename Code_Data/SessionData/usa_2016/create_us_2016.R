# Packages ----------------------------------------------------------------

suppressWarnings(if (!require("pacman"))
  install.packages("pacman", repos = "http://cran.r-project.org"))

pacman::p_load(data.table, tidyverse)


# Data --------------------------------------------------------------------

# source:
# https://dataverse.harvard.edu/dataset.xhtml;jsessionid=1dd02598ea6d8fb7c74571ac4772?persistentId=doi%3A10.7910%2FDVN%2FVOQCHQ&version=&q=&fileTypeGroupFacet=%22Tabular+Data%22&fileAccess=&fileTag=&fileSortField=&fileSortOrder=

## load data
#load("SessionData/usa_2016/countypres_2000-2016.RData")
load("../SessionData/usa_2016/countypres_2000-2016.RData")


# Processing --------------------------------------------------------------

df_wide <-
  x %>%
  as_tibble() %>%
  # filter down to the year we are interested in (2016)
  filter(year == 2016) %>%
  # select three variables (state, candidates name and candidates votes) we are interested in
  select(state, candidate, candidatevotes) %>%
  # remove NAs
  drop_na() %>%
  # summarize candidate votes by state as they are on county level
  group_by(state, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes)) %>%
  ungroup() %>%
  # create one column for each candidate
  pivot_wider(values_from = candidatevotes,
              names_from = candidate) %>%
  # create readable column names
  rename(Trump = 2,
         Clinton = 3,
         Other = 4) %>%
  mutate(state = as.character(state))

df_long <-
  df_wide %>%
  # bring wide data set into long format (one observation per voter)
  pivot_longer(!state) %>%
  uncount(value) %>%
  # add unique id for each voter
  mutate(voter_id = 1:nrow(.)) %>%
  rename(vote = name)

## get turnout by state
turnout_state <-
  df_wide %>%
  # grouping by state as we want the turnout on the state level
  group_by(state) %>%
  # summarize all votes for the turnout
  summarise(turnout_state = sum(Trump, Clinton, Other)) %>%
  ungroup()

# merge the two data frames
df_final <-
  df_long %>%
  left_join(turnout_state)

# save data in rds format
#fwrite(df_final, "SessionData/usa_2016/us_2016.gz")
fwrite(df_final, "../SessionData/usa_2016/us_2016.gz")