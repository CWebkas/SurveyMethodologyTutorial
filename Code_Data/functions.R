library(dplyr)
library(survey)
library(purrr)

# session 2 ----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank()
    )
}

# session 3 ----
srs_obj_fun <-
  function(data, sample_size) {
    data %>%
      sample_n(sample_size) %>%
      mutate(fpc = nrow(data)) %>%
      svydesign(data = .,
                fpc = ~ fpc,
                id = ~ 1)
  }

multiple_srs <- function(df, n_samples, sample_sizes) {
  map(seq_len(n_samples),
      ~ srs_obj_fun(data = df, sample_size = sample_sizes)) %>%
    map(., ~ svymean( ~ vote, design = .)) %>%
    map_df(~ as_tibble(.)) %>%
    rename(se = 2)
}

# session 6 ----
ttest_imp <- function(df) {
  map(df, 9) %>%
    unlist() %>%
    as_tibble() %>%
    split(1:2) %>%
    bind_cols(.name_repair = "minimal") %>%
    rename(low = 1,
           upper = 2) %>%
    summarise(low = sum(low) / 5,
              upper = sum(upper) / 5) %>%
    mutate(diff_mean = map(df, 3) %>%
             unlist() %>%
             sum() / 5) %>%
    relocate(diff_mean, low, upper)
}
