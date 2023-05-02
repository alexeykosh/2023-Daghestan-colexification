library(tidyverse)
library(ggalluvial)
library(ggrepel)

theme_set(theme_void())

data <- read.csv('data/edges.csv')

data %>%
  filter(weight > 0) %>%
  group_by(source) %>%
  mutate(source_count = n()) %>%
  filter(weight > 0) %>%
  mutate(source = factor(source),
         target = factor(target)) %>%
  ggplot(aes(axis1 = source, axis2 = target, y = weight)) +
  geom_alluvium(fill = "blue", width = 1/18) +
  geom_stratum(width=1/18) +
  geom_text_repel(stat = 'stratum', aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), NA)), 
                  size = 4.2, direction = "y", nudge_x = -.25) +
  geom_text_repel(stat = 'stratum', aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), NA)), 
                  size = 4.2, direction = "y", nudge_x = .25) +
  scale_x_discrete(limits = c('source', 'target')) +
  theme(legend.position = 'none')

ggsave('figures/Fig1.pdf')
