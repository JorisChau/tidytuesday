library(rayshader)
library(ggplot2)

## data
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
sets1 <- subset(sets, num_parts > 1) 

## 2d histogram
gg <- ggplot(sets1, aes(x = num_parts, y = year)) + 
  geom_bin2d(bins = 30, color = NA) +
  coord_fixed(ratio = 1 / 19) +
  labs(x = "Number of parts per set", y = "Year", title = "Evolution of LEGO number of parts per set") + 
  scale_fill_gradientn(
    colours = c("#6c98c9", "#0A69AE", "#328349", "#A5BC45", "#E4CD9E", "#F2CD37", "#C91A09"),
    values = scales::rescale(c(0, 0.05, 0.1, 0.2, 0.35, 0.5, 1)),
    guide = guide_colorbar(title = "Count", barwidth = 0.5, barheight = 2, nbin = 5, label.position = "left")) +
  scale_x_log10()

gg <- gg + theme(    
  text = element_text(family = "Palatino"),
  axis.title.y = element_text(size = 6),
  axis.title.x = element_text(size = 6),
  axis.text.y = element_text(size = 5),
  axis.text.x = element_text(size = 5),
  plot.title = element_text(size = 10),
  plot.background = element_rect(color = NA, fill = "#ffffff"),
  panel.background = element_rect(color = NA, fill = "#ffffff"),
  plot.margin = margin(t = 0.5, r = 0.5, l = 0.75, b = 0.5, unit = "cm"),
  panel.grid.major = element_line(color = "#efefef", size= 0.25),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 6),
  legend.text = element_text(size = 5)
)

## 3d image
rayshader::plot_gg(
  gg,
  multicore = TRUE,
  shadow_intensity = 0.4,
  width = 4,
  height = 4,
  scale = 50,
  preview = TRUE,
  raytrace = TRUE,
  triangulate = FALSE,
  offset_edges = TRUE
)

