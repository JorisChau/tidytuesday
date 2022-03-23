library(ggplot2)
library(data.table)

## load names
nms <- as.data.table(babynames::babynames)

## selected names/descriptions
desc <- data.table(
  name = c("Harrison", "Dewey", "Alton", "Ena", "Taft", "Willard", "Calvin", 
           "Shirley", "Rosalie", "Wendell", "Debbie", "Coretta", 
           "Jaime", "Marquita", "Whitney", "Ariel", "Hillary", "Zana", "Selena", 
           "Adaline"),
  label = c(
    "<span style='font-size:14pt'>Harrison<br><span style='color:grey40;font-size:12pt'><b>1888: </b><i>B. Harrison wins the US presidential election</i></span>",
    "<span style='font-size:14pt'>Dewey<br><span style='color:grey40;font-size:12pt'><b>1898: </b><i>George Dewey defeats the Spanish fleet at<br>the Battle of Manila Bay</i></span>",
    "<span style='font-size:14pt'>Alton<br><span style='color:grey40;font-size:12pt'><b>1904: </b><i>A. B. Parker loses the US presidential election</i></span>",
    "<span style='font-size:14pt'>Ena<br><span style='color:grey40;font-size:12pt'><b>1906: </b><i>Ena of Battenberg marries Alfonso XIII of Spain</i></span>",
    "<span style='font-size:14pt'>Taft<br><span style='color:grey40;font-size:12pt'><b>1908: </b><i>W. H. Taft wins the US presidential election</i></span>",
    "<span style='font-size:14pt'>Willard<br><span style='color:grey40;font-size:12pt'><b>1915: </b><i>Jess M. Willard becomes the new boxing<br>heavyweight champion</i></span>",
    "<span style='font-size:14pt'>Calvin<br><span style='color:grey40;font-size:12pt'><b>1924: </b><i>Calvin Coolidge wins US presidential election</i></span>",
    "<span style='font-size:14pt'>Shirley<br><span style='color:grey40;font-size:12pt'><b>1935: </b><i>Shirley Temple is a Hollywood sensation</i></span>",
    "<span style='font-size:14pt'>Rosalie<br><span style='color:grey40;font-size:12pt'><b>1938: </b><i>Broadway musical hit Rosalie is adapted to a<br>musical film</i></span>",
    "<span style='font-size:14pt'>Wendell<br><span style='color:grey40;font-size:12pt'><b>1940: </b><i>Wendell Wilkie loses US presidential election</i></span>",
    "<span style='font-size:14pt'>Debbie<br><span style='color:grey40;font-size:12pt'><b>1959: </b><i>Debbie Reynolds releases her first music album</i></span>",
    "<span style='font-size:14pt'>Coretta<br><span style='color:grey40;font-size:12pt'><b>1968: </b><i>Coretta Scott King becomes a widow after the<br>shooting of her husband Martin Luther King Jr.</i></span>",
    "<span style='font-size:14pt'>Jaime<br><span style='color:grey40;font-size:12pt'><b>1976: </b><i>Airing of the TV series The Bionic Woman<br>featuring Jaime Sommers</i></span>",
    "<span style='font-size:14pt'>Marquita<br><span style='color:grey40;font-size:12pt'><b>1983: </b><i>Airing of a toothpaste commercial featuring<br>a young African-American couple Marquita and Chris</i></span>",
    "<span style='font-size:14pt'>Whitney<br><span style='color:grey40;font-size:12pt'><b>1986: </b><i>Whitney Houston releases her first album<br>and becomes a smash pop sensation</i></span>",
    "<span style='font-size:14pt'>Ariel<br><span style='color:grey40;font-size:12pt'><b>1991: </b><i>The Little Mermaid featuring princess Ariel is a<br>box-office hit</i></span>",
    "<span style='font-size:14pt'>Hillary<br><span style='color:grey40;font-size:12pt'><b>1992: </b><i>Hillary Clinton becomes first lady of the US</i></span>",
    "<span style='font-size:14pt'>Zana<br><span style='color:grey40;font-size:12pt'><b>1994: </b><i>The Sinbad Show featuring orphaned daugher<br>Zana is cancelled by Fox network</i></span>",
    "<span style='font-size:14pt'>Selena<br><span style='color:grey40;font-size:12pt'><b>1995: </b><i>Selena Quintanilla-Perez, Queen of Tejano mu-<br>sic is shot and killed by her friend and former manager</i></span>",
    "<span style='font-size:14pt'>Adaline<br><span style='color:grey40;font-size:12pt'><b>2016: </b><i>The Age of Adaline is released in theaters</i></span>"
  )
) 

## process data
desc[, name_fct := factor(name, levels = name, labels = label)]
nms <- unique(nms[, .(prop = sum(prop) * 100), by = c("year", "name")])
nms <- nms[desc, on = "name"]
label_nms <- nms[, .SD[which.max(prop)], by = "name"]
label_nms[, c("label", "x1", "y1") := .(
  year,
  fcase(
    year < 1906, year + 15,
    year > 1990, year - 15,
    rep(TRUE, .N), year + sample(c(-15, 15), size = .N, replace = TRUE)
  ),
  prop * runif(.N, min = 0.75, max = 1)
)][, c("x2", "y2", "hjust", "vjust") := .(
  0.8 * year + 0.2 * x1,
  0.8 * prop + 0.2 * y1,
  fifelse(x1 < year, 1, 0.01),
  1
)]

## create plot
baby_plot <- ggplot(nms) + 
  geom_line(aes(x = year, y = prop, color = name_fct), lwd = 1) +
  facet_wrap(facets = vars(name_fct), scales = "free_y", ncol = 3) + 
  geom_point(data = label_nms, aes(x = year, y = prop, fill = name_fct), pch = 21, size = 3, stroke = 1, color = "grey40") + 
  geom_curve(data = label_nms, aes(x = x1, y = y1, xend = x2, yend = y2), 
             color = "grey60", curvature = -0.2, size = 0.7, arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_label(data = label_nms, aes(x = x1, y = y1, label = year, hjust = hjust, vjust = vjust), size = 4, color = "grey20") +  
  bbplot::bbc_style() +
  scale_color_hue(l = 35) +
  scale_fill_hue(l = 65) + 
  labs(title = "Trending baby names throughout US history", y = "Percentage of total births per year", subtitle = "From heavyweight boxing champions to Disney princesses") + 
  theme(
    axis.title.y = element_text(angle = 90, size = 14), 
    axis.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10), 
    strip.text.x = ggtext::element_markdown(size = 16, hjust = 0),
    strip.background = element_rect(colour=NA, fill = "#ebebeb"),
    plot.background = element_rect(color = NA, fill = "#ebebeb"), 
    panel.background = element_rect(color = NA, fill = "#ebebeb"),
    plot.subtitle = element_text(margin = margin(0, 0, 30, 0))
  ) +
  guides(color = "none", fill = "none")

## save plot
bbplot::finalise_plot(
  plot_name = baby_plot,
  save_filepath = "2022/wk12/baby_plot.png",
  source_name = "Sources: babynames R-package | Wikipedia | http://www.nancy.cc/2012/09/18/the-mysterious-baby-name-marquita/", 
  width_pixels = 1000,
  height_pixels = 1200
)

