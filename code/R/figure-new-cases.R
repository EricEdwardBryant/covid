
figure_new_cases <- function(csv, county, population, date_limits = c(NA, "2021-03-01")) {
  this_county <- county
  json <- str_c(tools::file_path_sans_ext(csv), ".json")

  df <-
    read_csv(csv, col_types = cols()) %>%
    filter(county == this_county) %>%
    # Calculate estimates
    arrange(date) %>%
    mutate(
      count_new_roll  = slide_dbl(count_new, mean, .before = 7L, .after = 7L),
      count_new_loess = predict_loess(date, count_new, span = 0.11),
      count_new_loess_diff = count_new_loess - lag(count_new_loess)
    )

  # Observations to directly annotate
  df_text <-
    filter(df, date == max(date))

  df %>%
    ggplot(aes(date, count_new)) +
    labs(
      title = str_c(this_county, " county"),
      y = "New cases",
      caption =
        str_c(
          "<span style='color:red'>Red line</span>: 2 week rolling mean",
          "<span style='color:blue'>Blue line</span>: loess estimate",
          sep = "<br>"
        )
    ) +
    geom_vline(xintercept = as.Date(c("2021-01-01", "2022-01-01", "2023-01-01"), tz = ""), linetype = "dotted") +
    geom_hline(yintercept = 0) +
    geom_line(alpha = 0.25) +
    geom_line(aes(y = count_new_roll), color = "red") +
    geom_line(aes(y = count_new_loess), color = "blue") +
    geom_label(
      aes(
        y     = round(count_new_loess),
        # Round the direct annotation to nearest 10
        label = scales::comma(round(count_new_loess, digits = -1))
      ),
      color = "blue",
      hjust = -0.1,
      vjust = 0.5, # Place label just above the point on the plot
      data = df_text
    ) +
    geom_label(
      aes(
        y     = round(count_new_loess),
        label = scales::comma(count_new)
      ),
      color = "black",
      hjust = -0.1,
      vjust = -0.5, # Place label just above the point on the plot
      data = df_text
    ) +
    geom_label(
      aes(
        y     = round(count_new_loess),
        # Round the direct annotation to nearest 10
        label = scales::comma(round(count_new_roll, digits = -1))
      ),
      color = "red",
      hjust = -0.1,
      vjust = 1.5, # Place label just above the point on the plot
      data = df_text
    ) +
    scale_y_continuous(
      breaks = function(x) labeling::extended(0, max(x, na.rm = TRUE), 5),
      labels = scales::comma,
      sec.axis =
        sec_axis(
          ~ . / (population / 1e4),
          name = "Per 10K people"
        ),
      expand = expansion(c(0.30, 0))
    ) +
    scale_x_date(
      date_breaks = "4 month",
      date_labels = "%b",
      date_minor_breaks = "1 month",
      sec.axis =
        dup_axis(
          breaks = scales::date_breaks("1 year"),
          labels = scales::date_format("%Y")
        )
    ) +
    coord_cartesian(
      xlim = as.Date(date_limits, tz = "")
    ) +
    theme(
      plot.caption = ggtext::element_markdown(hjust = 0, size = 7),
      aspect.ratio = 0.5,
      axis.title.x = element_blank(),
      axis.text.x = element_text(hjust = 0)
    )
}


predict_loess <- function(x, y, span) {
  predict(loess(as.numeric(y) ~ as.numeric(x), span = span))
}
