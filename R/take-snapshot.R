library(tidyverse)
library(lubridate)
library(curl)

# Map sub-folders within the 'incoming' folder ----------------------
incoming_folders <- curl("ftp://cran.r-project.org/incoming/")
dir <- readLines(incoming_folders)
folders <- dir %>%
  str_split(" ") %>%
  map_chr(~ .x[length(.x)])

# Iterate through the mapped folders to extract contents ------------
cran_incoming <- folders %>%
  map_df(~ {
    current_folder <- curl(paste0("ftp://cran.r-project.org/incoming/", .x, "/"))
    tibble(
      lines = readLines(current_folder),
      subfolder = .x
    )
    close(current_folder)
  }) %>%
  bind_rows(
    tibble(
      lines = dir,
      subfolder = "root"
    )
  ) %>%
  mutate(
    snapshot_date = Sys.Date(),
    snapshot_time = Sys.time()
  )

# Prepare new snapshot file name ------------------------------------
ts <- now() %>%
  as.character() %>%
  str_replace_all(" ", "-") %>%
  str_replace_all(":", "-") %>%
  paste0("data/snapshots/cran-incoming-", ., ".csv")

write_csv(cran_incoming, ts)
