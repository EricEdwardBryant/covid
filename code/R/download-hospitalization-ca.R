#' Download COVID hospitalization data for CA
#'
#' @param save_as Where to save the resulting CSV.
#' @param force Whether to force a download even if the data were downloaded
#' today. Defaults to `FALSE` to prevent from accidentally spamming the API.
#' @param smooth_span The smoothing parameter passed to the [stats::loess]
#' `span` argument.

download_hospitalization_ca <- function(save_as = "data/hospitalization-ca.csv",
                                        force = FALSE) {
  # Download if not yet downloaded today, unless force = TRUE
  json_file <-
    download_data_ca(
      save_as, force,
      dataset    = "42d33765-20fd-44b8-a978-b083b7542225",
      sql_select = "*",
      sql_where  = '"todays_date" IS NOT NULL'
    )

  # Convert JSON to dataframe
  df <-
    # Combine records into a dataframe
    jsonlite::read_json(json_file, simplifyVector = TRUE)$result$records %>%
    as_tibble() %>%
    select(
      date = todays_date,
      county,
      icu_covid_confirmed = icu_covid_confirmed_patients,
      icu_covid_suspected = icu_suspected_covid_patients,
      hsp_covid_confirmed = hospitalized_covid_confirmed_patients,   # Includes ICU patients
      hsp_covid_suspected = hospitalized_suspected_covid_patients,
      icu_available_beds,
      hsp_all_beds = all_hospital_beds
    ) %>%
    # Remove trailing ".0" (causes warning when converting counts to integer)
    mutate_all(~str_replace(., "[.]0$", "")) %>%
    # Convert columns to appropriate type
    type_convert(
      col_types = cols(date = col_datetime(), county = "c", .default = "i")
    ) %>%
    mutate(date = as.Date(date, tz = "")) %>%
    # Calculate estimates for each county
    arrange(county, date)

  write_csv(df, save_as)

}

download_hospitalization_ca_csv <- function(save_as = "data/hospitalization-ca.csv",
                                            force = FALSE) {
  if (!(force | !file.exists(save_as) | Sys.Date() != as.Date(file.mtime(save_as), tz = ""))) {
    return(invisible())
  }

  url <-
    "https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv"

  message("Updating hospitalization:\n  ", url)

  df <-
    read_csv(url, col_types = cols()) %>%
    select(
      date = todays_date,
      county,
      icu_covid_confirmed = icu_covid_confirmed_patients,
      icu_covid_suspected = icu_suspected_covid_patients,
      hsp_covid_confirmed = hospitalized_covid_confirmed_patients,   # Includes ICU patients
      hsp_covid_suspected = hospitalized_suspected_covid_patients,
      icu_available_beds,
      hsp_all_beds = all_hospital_beds
    ) %>%
    # Convert columns to appropriate type
    type_convert(
      col_types = cols(date = col_datetime(), county = "c", .default = "i")
    ) %>%
    mutate(date = as.Date(date, tz = "")) %>%
    # Calculate estimates for each county
    arrange(county, date)

  write_csv(df, save_as)
}


download_hospitalization_ca_csv_v2 <- function(save_as = "data/hospitalization-ca-v2.csv",
                                            force = FALSE) {
  if (!(force | !file.exists(save_as) | Sys.Date() != as.Date(file.mtime(save_as), tz = ""))) {
    return(invisible())
  }

  url <-
    "https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv"

  message("Updating hospitalization:\n  ", url)

  df_raw <-
    read_csv(url, col_types = cols(
      county                                 = col_character(),
      todays_date                           = col_date(),
      hospitalized_covid_confirmed_patients = col_double(),
      hospitalized_suspected_covid_patients = col_double(),
      hospitalized_covid_patients           = col_double(),
      all_hospital_beds                     = col_double(),
      icu_covid_confirmed_patients          = col_double(),
      icu_suspected_covid_patients          = col_double(),
      icu_available_beds                    = col_double()
    ))

  df <-
    df_raw %>%
    select(
      date                = todays_date,
      county,
      icu_covid_confirmed = icu_covid_confirmed_patients,
      icu_covid_suspected = icu_suspected_covid_patients,
      hsp_covid_confirmed = hospitalized_covid_confirmed_patients,   # Includes ICU patients
      hsp_covid_suspected = hospitalized_suspected_covid_patients,
      icu_available_beds,
      hsp_all_beds        = all_hospital_beds
    ) %>%
    mutate_at(vars(icu_covid_confirmed:hsp_all_beds), as.integer) %>%
    # Calculate estimates for each county
    arrange(county, date)

  write_csv(df, save_as)
}

