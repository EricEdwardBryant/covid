#' Download COVID case data for CA
#'
#' Query the ca.data.gov API. Downloads data in JSON format.
#'
#' @param save_as Where to save the returned JSON data. File extension is
#' replaced with `.json`
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

download_data_ca <- function(save_as,
                             force = FALSE,
                             api = "https://data.ca.gov/api/3/action/datastore_search_sql?sql=",
                             dataset = "926fd08f-cc91-4828-af38-bd45de97f8c3",
                             sql_select = "*",
                             sql_where = '"date" IS NOT NULL') {
  # Where to download the data
  json <- str_c(tools::file_path_sans_ext(save_as), ".json")

  # Query for LA covid data
  url <-
    URLencode(str_c(
      api, "SELECT ", sql_select, ' from "', dataset, '" WHERE ', sql_where
    ))

  # Only download if modified date of target is different from today's date
  if (force | !file.exists(json) | Sys.Date() != as.Date(file.mtime(json), tz = "")) {
    download.file(url, json)
  }

  return(json)
}
