library(tidyverse)
library(ggalluvial)

theme_set(theme_bw())

data <- read.csv('data/edges.csv')

data %>%
  filter(weight > 0) %>%
  group_by(source) %>%
  mutate(source_count = n()) %>%
  filter(weight > 0) %>%
  mutate(source = factor(source),
         target = factor(target)) %>%
  ggplot(aes(axis1 = source, axis2 = target, y = source_count)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, fill = 'grey') +
  geom_text(stat = 'stratum', aes(label = after_stat(stratum)), size = 3) +
  geom_text(stat = 'alluvium', aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c('source', 'target')) +
  ggtitle('Alluvial plot of the data') +
  theme(legend.position = 'none')