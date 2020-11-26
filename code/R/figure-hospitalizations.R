figure_hospitalization <- function(csv, county, date_limits = c("2020-03-17", "2021-03-01")) {
  this_county <- county

  # Prepare the data for plotting
  plot_data <-
    read_csv(csv, col_types = cols()) %>%
    # Date hard censored due to change in reporting criteria
    filter(county == this_county, date >= as.Date("2020-05-01")) %>%
    # Combine confirmed and suspected counts (at this point most are confirmed)
    # In the spring there were more "suspected" probably due to testing backlogs?
    mutate(
      hsp = hsp_covid_confirmed + hsp_covid_suspected,
      icu = icu_covid_confirmed + icu_covid_suspected,
      hsp = hsp - icu # remove ICU cases from inpatient (the figure will stack)
    ) %>%
    # Reshape into a long table to make plotting easier
    select(date, hsp, icu) %>%
    pivot_longer(c("hsp", "icu"), names_to = "type", values_to = "count") %>%
    mutate(
      type =
        factor(type, levels = c("hsp", "icu"), labels = c("Inpatient", "ICU"))
    )

  # Plot date vs count and color by ICU/inpatient
  gg_hsp_icu <-
    ggplot(plot_data, aes(date, count, color = type, fill = type)) +
    geom_area(alpha = 0.5)

  # All this just makes the plot pretty
  gg_hsp_icu +
    # Add a line to mark the new year
    geom_vline(xintercept = as.Date("2021-01-01", tz = ""), linetype = "dotted") +
    labs(
      # be clear about the numbers in the figure caption
      caption = str_c(
        "COVID confirmed + suspected",
        "Numbers do not include ER, overflow, or outpatient",
        sep = "<br>"
      ),
      y = "Patients"
    ) +
    scale_fill_brewer(palette = "Dark2") +       # Pick different colors
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::comma) + # Add commas to the numbers
    scale_x_date(
      date_breaks = "2 month",       # Label the date every two months
      date_labels = "%b",            # Label with just the month abbreviation
      date_minor_breaks = "1 month", # add vertical lines for every month
      limits = as.Date(date_limits, tz = ""),     # set the date limits
      sec.axis =
        dup_axis(
          breaks = scales::date_breaks("1 year"), # Add a 1 year marker
          labels = scales::date_format("%Y")      # Label the year
        )
    ) +
    theme(
      # Ugh, legends are annoying
      legend.title = element_blank(),
      legend.key.height = unit(3, "points"),
      legend.key.width  = unit(25, "points"),
      legend.text = element_text(size = 6),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.box.spacing = unit(1, "points"),
      plot.caption = ggtext::element_markdown(hjust = 0, size = 7),
      aspect.ratio = 0.5,
      axis.title.x = element_blank(),
      axis.text.x = element_text(hjust = 0)
    )
}
