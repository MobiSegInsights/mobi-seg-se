# Title     : POI share by type and group
# Objective : Justify small differences between F and N
# Created by: Yuan Liao
# Created on: 2024-05-09

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggsci)
library(ggthemes)
library(ggdensity)
library(arrow)
library(scales)
library(ggExtra)
library(magick)
options(scipen=10000)

rename_dict <- c(
  D = "N",
  N = "M"
)
df <- as.data.frame(read_parquet('results/poi_share_range_by_group.parquet')) %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict)) %>%
  mutate(lower = ave-std) %>%
  mutate(upper = ave+std)
df <- arrange(df, grp_r, ave)
lbs <- unique(df$poi_type)
# df$poi_type <- factor(df$poi_type, levels = lbs, labels = lbs)
df$poi_type <- factor(df$poi_type, levels=c('Retail', 'Financial', 'Office', 'Mobility',
                                            'Health and Wellness', 'Food, Drink, and Groceries',
                                            'Recreation', 'Education', 'Religious'))

df.t <- as.data.frame(read_parquet('results/poi_share_by_group.parquet')) %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict))
df.t$poi_type <- factor(df.t$poi_type, levels=rev(c('Retail', 'Financial', 'Office', 'Mobility',
                                            'Health and Wellness', 'Food, Drink, and Groceries',
                                            'Recreation', 'Education', 'Religious')))

cols <- c('N'='#001260', 'F'='#601200', 'M'='gray75')

g1 <- ggplot(data = df, aes(y=poi_type, color=grp_r)) +
  theme_hc() +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .3, size=1, alpha=1,
                 position = position_dodge(width = 0.8)) +
  geom_point(aes(x=ave), shape = 21, fill = "white", size = 2,
             position = position_dodge(width = 0.8)) +
  labs(y = 'Venue type',
       x = 'Share of activity time outside home') +
  scale_color_manual(name = "Birth background group",
                     values = cols) +
  scale_fill_manual(name = "Birth background group",
                   values = cols) +
  theme(legend.position="bottom", strip.background = element_blank()) +
  coord_flip()
ggsave(filename = "figures/supporting/venue_share_time.png", plot=g1,
       width = 12, height = 5, unit = "in", dpi = 300, bg = 'white')