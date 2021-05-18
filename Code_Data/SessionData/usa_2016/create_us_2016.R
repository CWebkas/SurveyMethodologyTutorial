library(dplyr)
library(tidyr)

# data source: https://dataverse.harvard.edu/dataset.xhtml;jsessionid=1dd02598ea6d8fb7c74571ac4772?persistentId=doi%3A10.7910%2FDVN%2FVOQCHQ&version=&q=&fileTypeGroupFacet=%22Tabular+Data%22&fileAccess=&fileTag=&fileSortField=&fileSortOrder=

load("SessionData/usa2016/countypres_2000-2016.RData")

df_wide <-
  x %>%
  as_tibble() %>%
  filter(year == 2016) %>%
  select(state, candidate, candidatevotes) %>%
  drop_na() %>%
  group_by(state, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes)) %>%
  ungroup() %>%
  pivot_wider(values_from = candidatevotes,
              names_from = candidate) %>%
  rename(trump = 2,
         clinton = 3,
         other = 4)

df_long <-
  df_wide %>%
  pivot_longer(!state) %>%
  uncount(value)

saveRDS(df_long, "SessionData/usa_2016/us_2016.rds")
