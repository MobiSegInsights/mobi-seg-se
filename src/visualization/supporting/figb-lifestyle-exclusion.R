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

# Manually reverse the NPG color palette
reversed_colors <- rev(pal_npg("nrc")(9))  # Generate and reverse 4 colors

rename_dict_again <- c(
  N = "Native-born segregated",
  F = "Foreign-born segregated"
)
df.t <- df.t %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict_again))

g2 <- ggplot(df.t, aes(fill=poi_type, x=visit_share, y=grp_r)) +
  theme_hc() +
  geom_bar(position="stack", stat="identity", width = 0.5, color='white') +
  labs(x = 'Share of visits (%)',
       y = 'Birth background group') +
  guides(color = guide_legend(reverse = TRUE)) +  # Reverse the legend order
  scale_fill_manual(name = "Venue type", values=reversed_colors) +
  theme(legend.position= "right", legend.justification = c(1, 0.2),
        strip.background = element_blank(),
        legend.text.align = 1,
        legend.key.height = unit(0.9, "cm"),  # Vertical spacing
        legend.key.width = unit(0.5, "cm"),     # Horizontal spacing
        legend.spacing = unit(5, "cm")) +      # Space between title and elements)
  coord_flip()
g2
ggsave(filename = "figures/panels/venue_share.png", plot=g2,
       width = 7, height = 5, unit = "in", dpi = 300, bg = 'white')