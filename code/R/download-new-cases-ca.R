#' Download COVID daily new case data for CA
#'
#' @param save_as Where to save the resulting CSV.
#' @param force Whether to force a download even if the data were downloaded
#' today. Defaults to `FALSE` to prevent from accidentally spamming the API.
#' `span` argument.

download_new_cases_ca <- function(save_as = "data/new-cases-ca.csv",
                                  force = FALSE) {
  # Download if not yet downloaded today, unless force = TRUE
  json_file <-
    download_data_ca(
      save_as, force,
      dataset    = "926fd08f-cc91-4828-af38-bd45de97f8c3",
      sql_select = "*",
      sql_where  = '"date" IS NOT NULL'
    )

  # Convert JSON to dataframe
  df <-
    # Combine records into a dataframe
    jsonlite::read_json(json_file, simplifyVector = TRUE)$result$records %>%
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
    mutate(date = as.Date(date, tz = ""))

  write_csv(df, save_as)
}


download_new_cases_ca_csv <- function(save_as = "data/new-cases-ca.csv",
                                      force = FALSE) {
  if (!(force | !file.exists(save_as) | Sys.Date() != as.Date(file.mtime(save_as), tz = ""))) {
    return(invisible())
  }

  url <-
    "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"

  message("Updating new cases from:\n  ", url)

  df <-
    read_csv(url, col_types = cols()) %>%
    select(
      date, county,
      count_new   = newcountconfirmed,
      count_total = totalcountconfirmed,
      death_new   = newcountdeaths,
      death_total = totalcountdeaths
    ) %>%
    # Convert columns to appropriate type
    type_convert(
      col_types = cols(date = col_datetime(), county = "c", .default = "i")
    ) %>%
    mutate(date = as.Date(date, tz = ""))

  write_csv(df, save_as)
}
