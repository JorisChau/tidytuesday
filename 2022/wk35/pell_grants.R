library(data.table)
library(ggplot2)

institutions <- as.data.table(
  read.csv(
    file = "Most-Recent-Cohorts-Institution.csv"
  )
)

aliases <- c("University of Southern California" = "USC",
             "University of Michigan-Ann Arbor" = "Univ. of Michigan",
             "Emory University" = "Emory",
             "Carnegie Mellon University" = "Carnegie Mellon",
             "Columbia University in the City of New York" = "Columbia",
             "Princeton University" = "Princeton",
             "Harvard University" = "Harvard",
             "Johns Hopkins University" = "Johns Hopkins",
             "Yale University" = "Yale",
             "Massachusetts Institute of Technology" = "MIT",
             "Rice University" = "Rice",
             "Vanderbilt University" = "Vanderbilt",
             "Washington University in St Louis" = "Washington Univ.",
             "University of Chicago" = "Univ. of Chicago",
             "Brown University" = "Brown",
             "University of Pennsylvania" = "UPenn",
             "Duke University" = "Duke",
             "University of Notre Dame" = "Notre Dame",
             "Georgia Institute of Technology-Main Campus" = "Georgia Tech.",
             "Tufts University" = "Tufts",
             "Case Western Reserve University" = "CWRU",
             "Cornell University" = "Cornell",
             "Dartmouth College" = "Dartmouth",
             "Stanford University" = "Stanford",
             "Northwestern University" = "Northwestern",
             "New York University" = "NYU",
             "University of Rochester" = "Univ. of Rochester",
             "Boston University" = "Boston Univ.",
             "Tulane University of Louisiana" = "Tulane Univ.",
             "University of Virginia-Main Campus" = "Univ. of Virginia",
             "Northeastern University" = "Northeastern",
             "Georgetown University" = "Georgetown",
             "University of California-Los Angeles" = "UCLA"
)
aliases <- data.table(
  INSTNM = names(aliases),
  ALIAS = aliases
)

institutions1 <- institutions[MAIN == "1", .SD, .SDcols = c("INSTNM", "TUITFTE", "UGDS", "ADM_RATE_ALL", "FTFTPCTPELL_POOLED_SUPP", "SAT_AVG_ALL")]
cols <- c("UGDS", "ADM_RATE_ALL", "TUITFTE", "FTFTPCTPELL_POOLED_SUPP", "SAT_AVG_ALL")
institutions1[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

## clean-up data
institutions1 <- institutions1[TUITFTE > 0 & UGDS > 2000 & FTFTPCTPELL_POOLED_SUPP > 0]  
institutions1 <- merge(institutions1, aliases, all.x = TRUE, by = "INSTNM", sort = FALSE)
institutions1 <- institutions1[, ALIAS := fcoalesce(ALIAS, INSTNM)]
institutions1 <- institutions1[, ALIAS := sub("University of California-", "UC ", ALIAS)]
institutions1[ALIAS == "UCLA", SAT_AVG_ALL := 1428]   ## add SAT score ucla
institutions1 <- institutions1[!is.na(SAT_AVG_ALL) & !is.na(FTFTPCTPELL_POOLED_SUPP) & !is.na(UGDS) & !is.na(ADM_RATE_ALL)]

## color palette
palette <- grDevices::colorRampPalette(colors = rev(pals::ocean.tempo(20)), bias = 0.5)

## generate plot
newplot <- ggplot(institutions1, aes(x = SAT_AVG_ALL, y = FTFTPCTPELL_POOLED_SUPP, size = UGDS / 1000, color = ADM_RATE_ALL)) + 
  geom_point(alpha = 0.8) +
  scale_size(range = c(0.5, 20), guide = guide_legend(title.vjust = 0.87, label.vjust = 0.85)) + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_color_gradientn(colors = palette(n = 20), guide = guide_colourbar(title.vjust = 0.75, barwidth = grid::unit(5, "cm"))) + 
  labs(
    title = "US school admission criteria and awarded Pell grants",
    subtitle = stringr::str_wrap(paste("This plot highlights the share of full-time, first-time degree/certificate-seeking undergraduate students who received", 
                                       "Pell Grants (data from 2019-2020) across US colleges and universities, an important measure of the access provided to low-income students.",
                                       "This is contrasted against several indicators of US school exclusivity: the average SAT equivalent score of admitted students and the institution's admission rate.",
                                       "The share of awarded Pell grants is seen to be (negatively) correlated with stricter school admission criteria. Note that this does not necessarily imply",
                                       "that more exclusive schools are less accessible to low-income students, as students eligible for Pell grants may tend to apply elsewhere or alternative grants",
                                       "may be available at these institutions that reduce the need for Pell."), width = 150),
    x = "Average SAT (equivalent) score of students admitted", 
    y = "Undergraduate students awarded a Pell Grant",
    color = "Admission rate",
    size = "Undergraduate student enrollment (\u00D71000)",
    caption = "Data source: https://collegescorecard.ed.gov/data/ Most-Recent-Cohorts-Institution_04262022 (data for academic year 2019-2020 reported in IPEDS 2020-2021)"
  ) + 
  ggrepel::geom_label_repel(
    aes(point.size = UGDS / 1000, label = institutions1[, fifelse((SAT_AVG_ALL > 1425 & UGDS > 4000 & FTFTPCTPELL_POOLED_SUPP < 0.25), ALIAS, "")]),
    max.overlaps = 25,
    point.size = 5,
    size = 3,
    fill = "#fcfcfc",
    family = "Palatino"
  ) + 
  theme(
    text = element_text(family = "Palatino"),
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(color = NA, fill = "#ededed"),
    panel.background = element_rect(color = NA, fill = "#ededed"),
    plot.margin = margin(50, 50, 50, 50),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey85"),
    plot.subtitle = element_text(margin = margin(10, 0, 0, 0), size = 12, lineheight = 1)
  ) 

ggsave(
  filename = "pell_grants.png",
  newplot,
  width = 3000,
  height = 2250,
  scale = 1.5,
  units = "px",
  dpi = 300
)

## generate plot 2
newplot <- ggplot(institutions1, aes(x = SAT_AVG_ALL, y = FTFTPCTPELL_POOLED_SUPP, size = UGDS / 1000, color = ADM_RATE_ALL)) + 
  geom_point(alpha = 0.8) +
  scale_size(range = c(0.5, 20), guide = guide_legend(title.vjust = 0.87, label.vjust = 0.85)) + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_color_gradientn(colors = palette(n = 20), guide = guide_colourbar(title.vjust = 0.75, barwidth = grid::unit(5, "cm"))) + 
  labs(
    title = "US school admission criteria and awarded Pell grants",
    # subtitle = stringr::str_wrap(paste("This plot highlights the share of full-time, first-time degree/certificate-seeking undergraduate students who received", 
    #                                    "Pell Grants (data from 2019-2020) across US colleges and universities, an important measure of the access provided to low-income students.",
    #                                    "This is contrasted against several indicators of US school exclusivity: the average SAT equivalent score of admitted students and the institution's admission rate.",
    #                                    "The share of awarded Pell grants is seen to be (negatively) correlated with stricter school admission criteria. Note that this does not necessarily imply",
    #                                    "that more exclusive schools are less accessible to low-income students, as students eligible for Pell grants may tend to apply elsewhere or alternative grants",
    #                                    "may be available at these institutions that reduce the need for Pell."), width = 150),
    x = "Average SAT (equivalent) score of students admitted", 
    y = "Undergraduate students awarded a Pell Grant",
    color = "Admission rate",
    size = "Undergraduate student enrollment (\u00D71000)",
    caption = "Data source: https://collegescorecard.ed.gov/data/ Most-Recent-Cohorts-Institution_04262022 (data for academic year 2019-2020 reported in IPEDS 2020-2021)"
  ) + 
  ggrepel::geom_label_repel(
    aes(point.size = UGDS / 1000, label = institutions1[, fifelse(grepl("(CUNY|University\\sof\\sCalifornia)", INSTNM), ALIAS, "")]),
    max.overlaps = 25,
    point.size = 5,
    size = 3,
    fill = "#fcfcfc",
    family = "Palatino"
  ) + 
  theme(
    text = element_text(family = "Palatino"),
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(color = NA, fill = "#ededed"),
    panel.background = element_rect(color = NA, fill = "#ededed"),
    plot.margin = margin(50, 50, 50, 50),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey85"),
    plot.subtitle = element_text(margin = margin(10, 0, 0, 0), size = 12, lineheight = 1)
  ) 

ggsave(
  filename = "pell_grants2.png",
  newplot,
  width = 3000,
  height = 2200,
  scale = 1.5,
  units = "px",
  dpi = 300
)
