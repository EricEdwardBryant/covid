#' Download COVID case data for CA
#'
#' @param save_as
#' @param force Whether to force a download even if the data were downloaded
#' today. Defaults to `FALSE` to prevent from accidentally spamming the API.
#' @param api URL to the [https://data.ca.gov] API.
#' @param dataset ID for the dataset to query.
#' @param sql_select Columns to retrieve as an SQL string. Defaults to all
#' columns i.e.`"*"`.
#' @param sql_where Rows to retrieve as an SQL "WHERE" clause. Defaults to rows
#' without a missing date i.e. `'"date" IS NOT NULL'`. To get e.g. just LA
#' county, you can use `"\"county\" = 'Los Angeles'"`.
#' @param smooth_span The smoothing parameter passed to the [stats::loess]
#' `span` argument.
#'
#' @details Constructs a URL with an SQL query to request data covid data from
#' [https://data.ca.gov]. By default, data will only be downloaded if the
#' save as target (specifically the `.json` version) was not already downloaded
#' today.

download_new_cases_ca <- function(save_as,
                                  force = FALSE,
                                  api = "https://data.ca.gov/api/3/action/datastore_search_sql?sql=",
                                  dataset = "926fd08f-cc91-4828-af38-bd45de97f8c3",
                                  sql_select = "*",
                                  sql_where = '"date" IS NOT NULL',
                                  smooth_span = 0.25) {
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
      count_new_loess = predict_loess(date, count_new, span = smooth_span),
      count_new_loess_diff = count_new_loess - lag(count_new_loess)
    )

  write_csv(df, csv)
}
