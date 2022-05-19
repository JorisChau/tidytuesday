## libraries
library(data.table)
library(ggplot2)
library(ggtext)
library(patchwork)

## raw data
votes <- as.data.table(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv'))
emojis <- setNames(
  c("<img src='img/flag-ukraine_1f1fa-1f1e6.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-netherlands_1f1f3-1f1f1.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-greece_1f1ec-1f1f7.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-portugal_1f1f5-1f1f9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-bulgaria_1f1e7-1f1ec.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-slovenia_1f1f8-1f1ee.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-armenia_1f1e6-1f1f2.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-norway_1f1f3-1f1f4.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-lithuania_1f1f1-1f1f9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-moldova_1f1f2-1f1e9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-switzerland_1f1e8-1f1ed.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-iceland_1f1ee-1f1f8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-croatia_1f1ed-1f1f7.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-albania_1f1e6-1f1f1.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-denmark_1f1e9-1f1f0.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-latvia_1f1f1-1f1fb.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-austria_1f1e6-1f1f9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-sweden_1f1f8-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-australia_1f1e6-1f1fa.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-serbia_1f1f7-1f1f8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-czechia_1f1e8-1f1ff.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-estonia_1f1ea-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-finland_1f1eb-1f1ee.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-belgium_1f1e7-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-azerbaijan_1f1e6-1f1ff.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-cyprus_1f1e8-1f1fe.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-san-marino_1f1f8-1f1f2.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-malta_1f1f2-1f1f9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-poland_1f1f5-1f1f1.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-romania_1f1f7-1f1f4.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-north-macedonia_1f1f2-1f1f0.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-israel_1f1ee-1f1f1.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-ireland_1f1ee-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-montenegro_1f1f2-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-georgia_1f1ec-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-united-kingdom_1f1ec-1f1e7.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-spain_1f1ea-1f1f8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-italy_1f1ee-1f1f9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-france_1f1eb-1f1f7.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-germany_1f1e9-1f1ea.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-russia_1f1f7-1f1fa.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-belarus_1f1e7-1f1fe.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-hungary_1f1ed-1f1fa.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-bosnia-herzegovina_1f1e7-1f1e6.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-slovakia_1f1f8-1f1f0.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-turkey_1f1f9-1f1f7.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-andorra_1f1e6-1f1e9.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-serbia_1f1f7-1f1f8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-monaco_1f1f2-1f1e8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-luxembourg_1f1f1-1f1fa.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-serbia_1f1f7-1f1f8.png' width='%d' style='vertical-align:middle'/>", 
    "<img src='img/flag-morocco_1f1f2-1f1e6.png' width='%d' style='vertical-align:middle'/>"
  ),
  c("Ukraine", "The Netherlands", "Greece", "Portugal", "Bulgaria",
    "Slovenia", "Armenia", "Norway", "Lithuania", "Moldova", "Switzerland",
    "Iceland", "Croatia", "Albania", "Denmark", "Latvia", "Austria",
    "Sweden", "Australia", "Serbia", "Czech Republic", "Estonia",
    "Finland", "Belgium", "Azerbaijan", "Cyprus", "San Marino", "Malta",
    "Poland", "Romania", "North Macedonia", "Israel", "Ireland",
    "Montenegro", "Georgia", "United Kingdom", "Spain", "Italy",
    "France", "Germany", "Russia", "Belarus", "Hungary", "Bosnia & Herzegovina",
    "Slovakia", "Turkey", "Andorra", "Serbia & Montenegro", "Monaco",
    "Luxembourg", "Yugoslavia", "Morocco")
)

## helper function
get_emoji <- function(country, size, label = TRUE) {
  if(label) {
    html <- paste0("<p>", country, " ", emojis[country], "</p>")
  } else {
    html <- emojis[country]
  }
  setNames(sprintf(html, size), country)
}

## filter/clean-up
countries <- c("Denmark", "Sweden", "Norway", "Finland", "Iceland")
votes <- votes[to_country %in% countries & semi_final == "f" & jury_or_televoting == "T" & from_country != to_country]
votes[, c("to_country", "from_country") := .(
  fcase(
    to_country == "Netherlands", "The Netherlands",
    to_country == "F.Y.R. Macedonia", "North Macedonia",
    rep(TRUE, .N), to_country
  ),
  fcase(
    from_country == "Netherlands", "The Netherlands",
    from_country == "F.Y.R. Macedonia", "North Macedonia",
    rep(TRUE, .N), from_country
  )
)]

## add columns
votes[, c("points_received", "points_mean") := .(sum(points), mean(points)), by = c("year", "to_country")]
votes[, points_diff := points - points_mean]
votes[, points_diff_mean := mean(points_diff), by = c("from_country", "to_country")]
votes[, year := factor(year, levels = sort(unique(year)))]

## generate plots
plots <- list()
colors <- c("2016" = "#7338B3", "2017" = "#FA822A", "2018" = "#FFBF23", "2019" = "#FC4400", "2021" = "#0479C7", "2022" = "#7CC605")

for(country in countries) {
  
  votes_country <- unique(votes[to_country == country, .(from_country, points_diff, points_diff_mean, year)])
  setorder(votes_country, points_diff_mean)
  votes_country[, from_country := factor(from_country, levels = unique(from_country))]

  plots[[country]] <- ggplot(votes_country, aes(x = from_country, y = points_diff, fill = year)) +
    geom_col(position = "stack", color = "#FFFFFF", size = 0.1) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_fill_manual(
      values = colors[which(names(colors) %in% as.character(votes_country[["year"]]))]
    ) +
    scale_x_discrete(name = NULL, labels = get_emoji(levels(votes_country[["from_country"]]), size = 10)) +
    labs(title = sprintf("<p>To %s %s</p>", country, get_emoji(country, size = 25, label = FALSE)), y = "Bias (# Points)") + 
    bbplot::bbc_style() +
    theme(
      plot.title = element_markdown(size = 16, color = "#FFFFFF"),
      axis.title.y = element_text(angle = 90, size = 8, color = "#FFFFFF"), 
      axis.text.y = element_text(size = 8, color = "#FFFFFF"), 
      axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust = 1, size = 7, color = "#FFFFFF"),
      plot.background = element_rect(color = NA, fill = "#0b0b45"), 
      panel.background = element_rect(color = NA, fill = "#0b0b45"),
      legend.position = "right",
      # plot.margin = margin(l = 15, b = 15, unit = "pt"),
      legend.text = element_text(size = 8, color = "#FFFFFF"),
      legend.key.size = grid::unit(10, "points"),
    ) +
    guides(fill = guide_legend(title = ""))
  
}

## combine plots 
patchwork <- plots[[1]] + 
  plots[[2]] + 
  plots[[3]] + 
  plots[[4]] + 
  plots[[5]] + 
  plot_layout(ncol = 3, nrow = 2) + 
  plot_annotation(
    subtitle = "<span style='font-size:24pt'><b>Eurovision televoting: Hey there neighbor!</b> <img src='img/telephone_260e-fe0f.png' width='30' style='vertical-align:middle'></span><br><br>How biased are televoters towards the Nordic countries? Bias (# Points) is the difference between the number of points given by a country relative to the average number of points across countries.",
    caption = "#TidyTuesday Week 20 | Eurovision Data"
  ) & 
  theme(
    plot.background = element_rect(color = NA, fill = "#0b0b45"),
    plot.subtitle = element_markdown(size = 12, color = "#FFFFFF", margin = margin(b = 35, t = 10, unit = "pt")),
    plot.caption = element_text(size = 8, color = "#FFFFFF")
  )

## save plot
ggsave(
  "eurovision_plot.png",
  patchwork,
  height= 3000,
  width = 5500,
  units = "px"
)





