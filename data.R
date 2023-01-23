data_path <- "data/"
files <- dir(data_path)
site_files <- files[files != "Sites.csv"]
site_paths <- paste0(data_path, site_files)
site_metadata <- read_csv(paste0(data_path, "Sites.csv"))
site_data <- map_dfr(
  site_paths,
  ~ read_csv(
    .x,
    col_types = "ciiiddddi",
    na = c("NA", NA)
  )
) %>%
  ## Remove duplicate records where at least one is NA
  arrange(Site, ob_time, rltv_hum) %>%
  fill(rltv_hum, .direction = "down") %>%
  ## Remove all duplicates
  distinct() %>%
  mutate(ob_time = dmy_hm(ob_time, quiet = TRUE)) %>%
  drop_na(ob_time) %>%
  select(-c(hour, day, month))
