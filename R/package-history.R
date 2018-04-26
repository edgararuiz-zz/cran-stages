library(tidyverse)
library(lubridate)

tidy_incoming <- read_rds("data/tidy_incoming.RDS")

package_history <- tidy_incoming %>%
  group_by(package) %>%
  arrange(snapshot_time) %>%
  arrange(package, snapshot_time) %>%
  mutate(
    previous = lag(subfolder),
    after = lead(subfolder),
    start = ifelse(subfolder != previous | is.na(previous), snapshot_time, NA),
    end = ifelse(subfolder != after | is.na(after), snapshot_time, NA)
  ) %>%
  filter(!is.na(start) | !is.na(end)) %>%
  mutate(
    folder_in = ifelse(is.na(start), lag(start), start),
    folder_out = ifelse(is.na(end), lead(end), end)
  ) %>%
  select(package, subfolder, folder_in, folder_out) %>%
  group_by_all() %>%
  summarise() %>%
  arrange(folder_in) %>%
  mutate(
    start = ymd_hms(folder_in) - hours(6),
    end = ymd_hms(folder_out) - hours(6),
    wait_minutes = round(interval(folder_in, folder_out) / 60)
  ) %>%
  ungroup() %>%
  select(-folder_in, -folder_out)


saveRDS(package_history, "data/package_history.RDS")
