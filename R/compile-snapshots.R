library(tidyverse)
library(lubridate)

# Compile files into a single data frame ----------------------------
data_location <- "data/snapshots/"
raw_data <- list.files(data_location) %>%
  map_df(~ read_csv(file.path(data_location, .x)))

# Extract the package name from its file name -----------------------
parsed_line <- raw_data %>%
  pull(lines) %>%
  str_split(" ") %>%
  map_chr(~ .x[length(.x)])
packages <- parsed_line %>%
  str_split("_") %>%
  map_chr(~ .x[[1]])

# Clean up and identify packages ------------------------------------
incoming_df <- raw_data %>%
  bind_cols(tibble(line = parsed_line)) %>%
  mutate(
    is_package = ifelse(str_sub(line, str_length(line) - 6) == ".tar.gz", TRUE, FALSE),
    package = ifelse(is_package, packages, "")
  ) 

# Further cleanup and filtering -------------------------------------
tidy_incoming <- incoming_df %>%
  filter(
    subfolder != "archive",
    subfolder != "pending",
    is_package
  ) %>%
  select(
    package,
    subfolder,
    snapshot_time
  )

saveRDS(incoming_df, "data/incoming_df.RDS")
saveRDS(raw_data, "data/raw_data.RDS")
saveRDS(tidy_incoming, "data/tidy_incoming.RDS")
