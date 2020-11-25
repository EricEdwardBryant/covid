#' Download COVID daily new case data for CA
#'
#' @param save_as Where to save the resulting CSV.
#' @param force Whether to force a download even if the data were downloaded
#' today. Defaults to `FALSE` to prevent from accidentally spamming the API.
#' @param smooth_span The smoothing parameter passed to the [stats::loess]
#' `span` argument.

download_new_cases_ca <- function(save_as = "data/new-cases-ca.csv",
                                  force = FALSE,
                                  smooth_span = 0.25) {
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
    mutate(date = as.Date(date, tz = "")) %>%
    # Calculate estimates for each county
    arrange(county, date) %>%
    group_by(county) %>%
    mutate(
      count_new_roll  = slide_dbl(count_new, median, .before = 7L, .after = 7L),
      count_new_loess = predict_loess(date, count_new, span = smooth_span),
      count_new_loess_diff = count_new_loess - lag(count_new_loess)
    )

  write_csv(df, save_as)
}
