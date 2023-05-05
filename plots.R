library(tidyverse)
library(ggalluvial)
library(ggrepel)
library(ggraph)
library(influential)

theme_set(theme_void())

data <- read.csv("data/edges.csv")

alluvial <- function(data){
  data %>%
    filter(weight > 0) %>%
    group_by(source) %>%
    mutate(source_count = n()) %>%
    filter(weight > 0) %>%
    mutate(source = factor(source),
          target = factor(target)) %>%
    ggplot(aes(axis1 = source, axis2 = target, y = weight)) +
    geom_alluvium(fill = "blue", width = 1 / 18) +
    geom_stratum(width = 1 / 18) +
    geom_text_repel(stat = "stratum", aes(label = ifelse(after_stat(x) == 1,
    as.character(after_stat(stratum)), NA)),
                    size = 4.2, direction = "y", nudge_x = -.25) +
    geom_text_repel(stat = "stratum", aes(label = ifelse(after_stat(x) == 2,
    as.character(after_stat(stratum)), NA)),
                    size = 4.2, direction = "y", nudge_x = .25) +
    scale_x_discrete(limits = c("source", "target")) +
    theme(legend.position = "none")
}

alluvial(data)
ggsave("figures/Fig1.pdf")


# create a fake data set
data_f <- data.frame(
  source = c("Andi", "Akhvakh", "Avar", "Akhvakh",  "Andi"),
  target = c("Avar", "Avar", "Akhvakh", "Andi", "Akhvakh"),
  weight = c(0.5, 1, 0.1, 0.1, 0.1)
)

alluvial(data_f)

# build a graph from data_f
graph <- data %>%
      filter(weight > 0) %>%
      graph_from_data_frame(., directed = TRUE)


ggraph(graph, layout = 'linear', circular = TRUE) +
  # geom_edge_parallel(aes(edge_width = weight),
  #   arrow = arrow(length = unit(4, "mm")),
  #   start_cap = circle(3, "mm"),
  #   end_cap = circle(3, "mm"), alpha = 0.8, sep = unit(5, 'mm')) +
  geom_edge_arc(aes(edge_width = weight),
    arrow = arrow(length = unit(4, "mm")),
    start_cap = circle(3, "mm"),
    end_cap = circle(3, "mm"), alpha = 0.5) +
  geom_node_text(aes(label = name)) +
  theme(text = element_text(size = 20)) +
  theme_void() +
  scale_edge_width(range = c(0.5, 2))