# Title     : ICE outside home and interactions
# Objective : Distributions in comparison with the random mixing
# Created by: Yuan Liao
# Created on: 2024-05-30

library(dplyr)
library(ggplot2)
library(Hmisc)
library(ggsci)
library(ggbeeswarm)
library(cowplot)
library(ggridges)
library(ggthemes)
library(ggpubr)
library(ggdensity)
library(arrow)
library(scales)
library(ggExtra)
library(hrbrthemes)
library(magick)

options(scipen=10000)

# ICE ----

df.ice.b <- as.data.frame(read_parquet('results/statistical_tests/ice_shuffled.parquet'))
df.ice.b <- select(df.ice.b, c('uid', 'wt_p', 'ICE_rs'))
names(df.ice.b) <- c('uid', 'wt_p', 'ice_b')

df.ice <- select(as.data.frame(read_parquet('results/statistical_tests/ice.parquet')),
                 c('uid', 'wt_p', 'grp_r', 'ice_enh', 'ice_e1', 'ice_e2')) %>%
  filter(grp_r %in% c('D', 'F'))
rename_dict <- c(
  D = "N"
)
df.ice <- df.ice %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict))

g0 <- ggplot(data=df.ice.b, aes(x=ice_b, y = ..count../sum(..count..), weight=wt_p)) +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = -0.2), color='gray', linewidth=0.2) +
  geom_vline(aes(xintercept = 0.2), color='gray', linewidth=0.2) +
  geom_freqpoly(bins=40, linewidth=1, alpha=0.7) +
  scale_color_npg(name = "") +
  labs(y = 'Fraction of individuals',
       x = 'Segregation level - Random mixing') +
  guides(color = guide_legend(ncol = 1)) +
  theme(legend.position = c(.8, .85),
        legend.background = element_blank())
ggsave(filename = "figures/panels/seg_rsim.png", plot=g0,
       width = 7, height = 4, unit = "in", dpi = 300, bg = 'white')

# Experienced
g1 <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -0.2), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 0.2), color='gray', linewidth=0.5) +
  geom_density(data=df.ice.b, aes(x=ice_b, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  # Experienced
  geom_density(data=df.ice, aes(x=ice_enh, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = 'Segregation level (ICE)') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())

# Sim 1 - No destination preference
g2 <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -0.2), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 0.2), color='gray', linewidth=0.5) +
  geom_density(data=df.ice.b, aes(x=ice_b, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  # Experienced
  geom_density(data=df.ice, aes(x=ice_e1, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = 'Segregation level (ICE)') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())

# Sim 2 - No dest preference and equalized mobility
g3 <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -0.2), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 0.2), color='gray', linewidth=0.5) +
  geom_density(data=df.ice.b, aes(x=ice_b, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  # Experienced
  geom_density(data=df.ice, aes(x=ice_e2, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = 'Segregation level (ICE)') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())

G1 <- ggarrange(g1, g2, g3, ncol = 3, nrow = 1, labels = c('a', 'b', 'c'),
                common.legend = T, legend="bottom")
ggsave(filename = "figures/panels/stats_test_ice.png", plot=G1,
       width = 12, height = 4, unit = "in", dpi = 300, bg = 'white')

# Exposure ----
df.exp.b <- as.data.frame(read_parquet('results/statistical_tests/exposure_shuffled.parquet'))
df.exp.b <- select(df.exp.b, c('uid', 'wt_p', 'Ds', 'Fs', 'Ns'))
names(df.exp.b) <- c('uid', 'wt_p', 'd', 'f', 'n')
df.exp.b <- df.exp.b %>%
  mutate(f = f*100,
         d = d*100,
         n = n*100)

# df.exp <- as.data.frame(read_parquet('results/statistical_tests/exposure.parquet'))
df.exp <- as.data.frame(read_parquet('results/plot/group_interactions_plot_combined.parquet'))
rename_dict <- c(
  'No-homophily' = "No-dest. preference",
  "Equalized mobility & no-homophily" = "No-dest. preference \n& equalized mobility"
)
df.exp <- df.exp %>%
  mutate(src = recode(Source, !!!rename_dict))

rename_dict <- c(
  D = "N",
  N = "M"
)
df.exp <- df.exp %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict))

exp.plot.inter <- function(source){
  # F
  df.f <- df.exp %>%
    filter(src==source) %>%
    filter(inter_type %in% c('DF', 'FF'))

  g4f <- ggplot() +
    theme_hc() +
    geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
    geom_vline(aes(xintercept = -13), color='gray', linewidth=0.5) +
    geom_vline(aes(xintercept = 13), color='gray', linewidth=0.5) +
    geom_density(data=df.exp.b,
                 aes(x=f, weight=wt_p, fill='Random mixing'),
                 color='gray35', alpha=0.3, adjust = 4) +
    geom_density(data=df.f, aes(x=value, weight=wt_p, color=grp_r), adjust = 3) +
    scale_color_manual(name = 'Birth background group',
                    breaks = c('F', 'N'),
                    values = c('#601200', '#001260')) +
    scale_fill_manual(name = '',
                    breaks = c('Random mixing'),
                    values = c('gray35')) +
    labs(y = 'Density',
         x = '',
         title='Exposure to foreign-born') +
    xlim(-100, 100) +
    guides(color = guide_legend(ncol = 2)) +
    theme(legend.position = c(.8, .6),
          legend.background = element_blank())

  df.d <- df.exp %>%
    filter(src==source) %>%
    filter(inter_type %in% c('FD', 'DD'))

  g4n <- ggplot() +
    theme_hc() +
    geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
    geom_vline(aes(xintercept = -19), color='gray', linewidth=0.5) +
    geom_vline(aes(xintercept = 19), color='gray', linewidth=0.5) +
    geom_density(data=df.exp.b,
                 aes(x=d, weight=wt_p, fill='Random mixing'),
                 color='gray35', alpha=0.3, adjust = 4) +
    geom_density(data=df.d, aes(x=value, weight=wt_p, color=grp_r), adjust = 3) +
    scale_color_manual(name = 'Birth background group',
                    breaks = c('F', 'N'),
                    values = c('#601200', '#001260')) +
    scale_fill_manual(name = '',
                    breaks = c('Random mixing'),
                    values = c('gray35')) +
    labs(y = 'Density',
         x = 'Exposure deviation from random mixing (%)',
         title='Exposure to native-born') +
    xlim(-100, 100) +
    guides(color = guide_legend(ncol = 2)) +
    theme(legend.position = c(.8, .6),
          legend.background = element_blank())

  df.n <- df.exp %>%
    filter(src==source) %>%
    filter(inter_type %in% c('FN', 'DN'))

  g4m <- ggplot() +
    theme_hc() +
    geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
    geom_vline(aes(xintercept = -19), color='gray', linewidth=0.5) +
    geom_vline(aes(xintercept = 19), color='gray', linewidth=0.5) +
    geom_density(data=df.exp.b,
                 aes(x=n, weight=wt_p, fill='Random mixing'),
                 color='gray35', alpha=0.3, adjust = 4) +
    geom_density(data=df.n, aes(x=value, weight=wt_p, color=grp_r), adjust = 3) +
    scale_color_manual(name = 'Birth background group',
                    breaks = c('F', 'N'),
                    values = c('#601200', '#001260')) +
    scale_fill_manual(name = '',
                    breaks = c('Random mixing'),
                    values = c('gray35')) +
    labs(y = 'Density',
         x = '',
         title='Exposure to mixed') +
    xlim(-100, 100) +
    guides(color = guide_legend(ncol = 2)) +
    theme(legend.position = c(.8, .6),
          legend.background = element_blank())
    G4 <- ggarrange(g4f, g4n, g4m, ncol = 3, nrow = 1,
                  common.legend = T, legend="bottom")
  return (G4)
}

exp.plot <- function(source='Empirical') {
g4f <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -13), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 13), color='gray', linewidth=0.5) +
  geom_density(data=df.exp.b,
               aes(x=f, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  geom_density(data=filter(df.exp, src==source),
               aes(x=f, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = '',
       title='Exposure to foreign-born') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())

g4d <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -0.19), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 0.19), color='gray', linewidth=0.5) +
  geom_density(data=df.exp.b,
               aes(x=d, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  # Experienced
  geom_density(data=filter(df.exp, src==source),
               aes(x=d, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = 'Exposure deviation from random mixing',
       title='Exposure to native-born') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())

g4n <- ggplot() +
  theme_hc() +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_vline(aes(xintercept = -0.19), color='gray', linewidth=0.5) +
  geom_vline(aes(xintercept = 0.19), color='gray', linewidth=0.5) +
  geom_density(data=df.exp.b,
               aes(x=n, weight=wt_p, fill='Random mixing'),
               color='gray35', alpha=0.3, adjust = 4) +
  # Experienced
  geom_density(data=filter(df.exp, src==source),
               aes(x=n, weight=wt_p, color=grp_r), adjust = 3) +
  scale_color_manual(name = 'Birth background group',
                  breaks = c('F', 'N'),
                  values = c('#601200', '#001260')) +
  scale_fill_manual(name = '',
                  breaks = c('Random mixing'),
                  values = c('gray35')) +
  labs(y = 'Density',
       x = '',
       title='Exposure to mixed') +
  xlim(-1, 1) +
  guides(color = guide_legend(ncol = 2)) +
  theme(legend.position = c(.8, .6),
        legend.background = element_blank())
G4 <- ggarrange(g4f, g4d, g4n, ncol = 3, nrow = 1,
                common.legend = T, legend="bottom")
return (G4)
}

G4 <- exp.plot.inter(source='Empirical')
G5 <- exp.plot.inter(source='No-dest. preference')
G6 <- exp.plot.inter(source="No-dest. preference \n& equalized mobility")

G2 <- ggarrange(G4, G5, G6, ncol = 1, nrow = 3,
                labels = c('a', 'b', 'c'),
                common.legend = T, legend="bottom")
ggsave(filename = "figures/panels/stats_test_exposure.png", plot=G2,
       width = 12, height = 12, unit = "in", dpi = 300, bg = 'white')