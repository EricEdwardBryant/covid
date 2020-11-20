#' Download COVID case data for CA
#'
#'

download_new_cases_ca <- function(save_as,
                                  force = FALSE,
                                  api = "https://data.ca.gov/api/3/action/datastore_search_sql?sql=",
                                  sql_select = "*",
                                  dataset = "926fd08f-cc91-4828-af38-bd45de97f8c3",
                                  sql_where = '"date" IS NOT NULL') {
  # Where to download the data
  json <- str_c(tools::file_path_sans_ext(save_as), ".json")
  csv  <- save_as

  # Query for LA covid data
  url <-
    URLencode(str_c(
      api, "SELECT ", sql_select, ' from "', dataset, '" WHERE ', sql_where
    ))

  # Only download if modified date of target is different from today's date
  if (force | !file.exists(json) | Sys.Date() != as.Date(file.mtime(json), tz = "")) {
    download.file(url, json)
  }

  # Convert JSON to dataframe
  df <-
    # Combine records into a dataframe
    jsonlite::read_json(json, simplifyVector = TRUE)$result$records %>%
    # Simplify and convert names to snake_case
    select(
      date, county,
      count_new   = newcountconfirmed,
      count_total = totalcountconfirmed,
      death_new   = newcountdeaths,
      death_total = totalcountdeaths
    ) %>%
    # Remove trailing ".0" (causes warning when converting counts to integer)
    mutate_all(~str_replace(., "[.]0$", "")) %>%
    # Convert columns to appropriate type
    type_convert(
      col_types = cols(date = col_datetime(), county = "c", .default = "i")
    ) %>%
    mutate(date = as.Date(date, tz = "")) %>%
    # Calculate estimates for each county
    arrange(county, date) %>%
    group_by(county) %>%
    mutate(
      count_new_roll  = slide_dbl(count_new, median, .before = 7L, .after = 7L),
      count_new_loess = predict_loess(date, count_new, span = smooth_span)
    )

  write_csv(df, csv)
}


download_new_cases_ca_la <- function(save_to = ".",
                                     file_name = "new-cases-ca-la",
                                     force = FALSE,
                                     api = "https://data.ca.gov/api/3/action/datastore_search_sql?sql=",
                                     sql_select = "*",
                                     dataset = "926fd08f-cc91-4828-af38-bd45de97f8c3",
                                     sql_where = "\"county\" = 'Los Angeles'") {
  download_new_cases_ca(save_to, file_name, force, api, sql_select, dataset, sql_where)
}
