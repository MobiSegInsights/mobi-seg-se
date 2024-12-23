# Title     : Experienced vs. residential nativity segregation
# Objective : On the map of central Gothenburg
# Created by: Yuan Liao
# Created on: 2023-9-22

library(dplyr)
library(ggforce)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggsci)
library(ggdensity)
library(ggmap)
library(ggspatial)
library(sf)
library(sp)
library(scico)
library(spdep)
library(ggraph)
library(arrow)
library(scales)
library(ggExtra)
library(networkD3)
library(magick)
options(scipen=10000)

read.img <- function(path, lb){
  image <- image_read(path) %>%
    image_annotate(lb, gravity = "northwest", color = "black", size = 70, weight = 700)
  return(image)
}

ggmap::register_stadiamaps(key='1ffbd641-ab9c-448b-8f83-95630d3c7ee3')
cols <- c('N'='#001260', 'F'='#601200', 'M'='gray75')
rename_dict <- c(
  D = "N",
  N = "M"
)

#------------ Experienced vs. residential --------------
fake_scico <- scico(7, palette = "vik")
df.deso <- as.data.frame(read_parquet('results/seg_disparity_map.parquet'))
df.deso$ice_r_g <- cut(df.deso$ice_r, breaks = c(-1, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 1))
df.deso$ice_e_g <- cut(df.deso$ice_enh, breaks = c(-1, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 1))

zones <- read_sf('dbs/DeSO/DeSO_2018_v2.shp')[, c('deso', 'geometry')]
waters <- read_sf('dbs/geo/sweden_water/sweden_water_polygons.shp')
waters <- st_transform(waters, 4326)
sea <- read_sf('dbs/geo/sweden_water/sweden_sea_polygons.shp')
sea <- st_transform(sea, 4326)
zones.seg <- merge(zones, df.deso, on='deso', how='inner')
zones.seg <- st_transform(zones.seg, 4326)

municipalities <- read_sf('dbs/municipalities/alla_kommuner/alla_kommuner.shp')[, c('ID', 'geometry')] %>%
  filter(ID %in% c('0180', '1480', '1280'))

# Stockholm
bbox <- c(17.6799476147,59.1174841345,18.4572303295,59.475092515)
names(bbox) <- c("left", "bottom", "right", "top")
stockholm_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lite", zoom = 12)

# Gothenburg
bbox <- c(11.587818,57.534778,12.290261,57.879356)
names(bbox) <- c("left", "bottom", "right", "top")
gothenburg_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lite", zoom = 12)

# Malmö
bbox <- c(12.8617560863,55.4132430111,13.4282386303,55.7174059585)
names(bbox) <- c("left", "bottom", "right", "top")
malmo_basemap <- get_stadiamap(bbox, maptype="stamen_toner_lite", zoom = 12)

water.color <- 'gray45'
# --- Residential on the map ---
g1 <- ggmap(stockholm_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_r_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 18.063240, y = 59.334591), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '0180',],
          fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  labs(title = 'Stockholm') +
  scale_fill_manual(values = rev(fake_scico),
                    breaks = c("(-1,-0.6]", "(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,1]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'top',
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 1))

g2 <- ggmap(gothenburg_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_r_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 11.974560, y = 57.708870), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '1480',],
        fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  labs(title = 'Gothenburg') +
  scale_fill_manual(values = rev(fake_scico),
                    breaks = c("(-1,-0.6]", "(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,1]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=12),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(ncol = 1))

g3 <- ggmap(malmo_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_r_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 13.003822, y = 55.604980), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '1280',],
    fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  labs(title = 'Malm\xf6') +
  scale_fill_manual(values = rev(fake_scico),
                    breaks = c("(-1,-0.6]", "(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,1]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'top',
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 1))

g4 <- ggmap(stockholm_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_e_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 18.063240, y = 59.334591), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '0180',],
        fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  scale_fill_manual(values = rev(fake_scico)[2:5],
                    breaks = c("(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'top',
        plot.title = element_text(hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))

g5 <- ggmap(gothenburg_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_e_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 11.974560, y = 57.708870), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '1480',],
      fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  scale_fill_manual(values = rev(fake_scico)[2:5],
                    breaks = c("(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'top',
        plot.title = element_text(hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))

g6 <- ggmap(malmo_basemap) +
  geom_sf(data = zones.seg, aes(fill=ice_e_g),
          color = NA, alpha=0.7, show.legend = T, inherit.aes = FALSE) +
  geom_sf(data = waters, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_sf(data = sea, fill=water.color,
          color = NA, alpha=1, show.legend = F, inherit.aes = FALSE) +
  geom_point(aes(x = 13.003822, y = 55.604980), col="black", fill='black', size=4, shape="\u2605") +
  geom_sf(data = municipalities[municipalities$ID == '1280',],
      fill=NA, color = 'black', alpha=0.7, show.legend = F, inherit.aes = FALSE) +
  scale_fill_manual(values = rev(fake_scico)[2:5],
                    breaks = c("(-0.6,-0.4]", "(-0.4,-0.2]",
                               "(-0.2,0.2]", "(0.2,0.4]"),
                    name = '') +
  theme_void() +
  theme(plot.margin = margin(0.1,0.1,0.1,0, "cm"),
        legend.position = 'top',
        plot.title = element_text(hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))

G1 <- ggarrange(g1, g2, g3, ncol = 3, nrow = 1, legend="none")
G1 <- annotate_figure(G1, left = text_grob('Residential', color = "black", size = 12, rot = 90, face='bold'))

G2 <- ggarrange(g4, g5, g6, ncol = 3, nrow = 1, legend="none")
G2 <- annotate_figure(G2, left = text_grob('Experienced', color = "black", size = 12, rot = 90, face='bold'))

G <- ggarrange(G1, G2, ncol = 1, nrow = 2, legend = 'none')
G <- ggarrange(G, get_legend(g2), ncol = 2, nrow = 1, widths = c(1, 0.12))

G <- annotate_figure(G,
                     right = text_grob("-1 <---------- 0 ----------> 1\nForeign-born segregated           Native-born segregated",
                                        color = "black", size = 12, rot = 270)
)

ggsave(filename = "figures/panels/seg_disp_map.png", plot=G,
       width = 12, height = 7, unit = "in", dpi = 300, bg = 'white')

# ------------ Group change figure ------
df.g <- read.csv('results/group_change.csv')
df.g$Residential <- factor(df.g$Residential, levels=c('F', 'N', 'D'),
                              labels=c('Foreign-born', 'Mixed', 'Native-born'))
df.g$Experienced <- factor(df.g$Experienced, levels=c('F', 'N', 'D'),
                              labels=c('Foreign-born', 'Mixed', 'Native-born'))

g7 <- ggplot(df.g, aes(x = Residential, y = Share,
                       fill = Experienced, pattern = Residential)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  scale_fill_scico_d(palette = 'vik', direction = -1, name='Experienced') +
  geom_text(aes(label = round(Share, digits=2)), colour = "#808e9b", size = 3,
            position = position_stack(vjust = .5), fontface = "bold") +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0, face = "bold"),
        text = element_text(size=14),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggsave(filename = "figures/panels/seg_disp_grp_change.png", plot=g7,
       width = 4, height = 4, unit = "in", dpi = 300)

# Destination preference ----
df.seg.p <- as.data.frame(read_parquet('results/plot/seg_by_poi.parquet'))
df.seg.p <- na.omit(df.seg.p)
df.seg.p$poi_type <- factor(df.seg.p$poi_type, levels=c('Retail', 'Financial', 'Office', 'Mobility',
                                            'Health and Wellness', 'Food, Drink, and Groceries',
                                            'Recreation', 'Education', 'Religious'))
df.seg.p <- df.seg.p %>%
  filter(grp_r %in% c('D', 'F'))
df.seg.p <- df.seg.p %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict))

g8 <- ggplot(data = df.seg.p, aes(y=poi_type, color=poi_type, shape=grp_r)) +
  theme_hc() +
  geom_segment(aes(x = 0, y = '', xend = 0.3, yend = ''),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               colour = "black", size=0.3, show.legend = F) +
  geom_label(aes(x = 0.15, y = ''), label = "Native-born\n segregated", fill = "white",
               colour = "black", size = 3, fontface = "bold", label.size = NA) +
  geom_segment(aes(x = 0, y = '', xend = -0.6, yend = ''),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               colour = "black", size=0.3) +
  geom_label(aes(x = -0.4, y = ''), label = "Foreign-born\n segregated", fill = "white",
             colour = "black", size = 3, fontface = "bold", label.size = NA) +
  geom_vline(aes(xintercept = 0), color='gray', linewidth=1) +
  geom_point(aes(x=0, y=''), color='black', size=2) +
  geom_errorbarh(aes(xmin=q25, xmax=q75), height = .3, size=1, alpha=1,
                 position = position_dodge(width = 0.8)) +
  geom_point(aes(x=q50), fill = "white", size = 4,
                 position = position_dodge(width = 0.8)) +  # shape = 21,
  labs(y = ' ',
       x = 'Experienced segregation\n outside residential area') +
  # scale_color_manual(name = "Birth background group", values = cols) +
  scale_color_npg(guide = "none") +
  scale_shape_discrete(name = 'Group') +
  theme(legend.position="top", strip.background = element_blank(),
        axis.text.y = element_blank())
g8
ggsave(filename = "figures/panels/seg_by_poi.png", plot=g8,
       width = 5, height = 5, unit = "in", dpi = 300)

# Mobility range ----
df.seg <- as.data.frame(read_parquet('results/plot/seg_grp_vs_rg.parquet')) %>%
  filter(grp_r != 'N')

var <- 'radius_of_gyration'
df.seg <- df.seg %>%
  mutate(grp_r = recode(grp_r, !!!rename_dict))
cols <- c('N'='#001260', 'F'='#601200', 'M'='gray75')

g9 <- ggplot(data = df.seg, aes(y=seg_cat, color=grp_r)) +
  theme_hc() +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 0.4),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               colour = "black", size=0.3) +
  geom_label(aes(x = 6, y = 0.35), label = "Native-born\n segregated", fill = "white",
               colour = "black", size = 3, fontface = "bold", label.size = NA) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = -0.6),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               colour = "black", size=0.3) +
  geom_label(aes(x = 6, y = -0.55), label = "Foreign-born\n segregated", fill = "white",
             colour = "black", size = 3, fontface = "bold", label.size = NA) +
  geom_hline(aes(yintercept = 0), color='gray', linewidth=1) +
  geom_point(aes(x=5, y=0), color='black', size=2) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .02, size=1, alpha=1) +
  geom_point(aes(x=q50_est), shape = 21, fill = "white", size = 2) +
  labs(x = 'Mobility range (km)',
       y = 'Experienced segregation\n outside residential area') +
  scale_color_manual(name = "Birth background group",
                     values = cols) +
  scale_fill_manual(name = "Birth background group",
                   values = cols) +
  scale_x_continuous(trans = 'log10') +
  ylim(-0.6, 0.4) +
  theme(legend.position="top", strip.background = element_blank()) +
  coord_flip()

ggsave(filename = "figures/panels/seg_by_mobi_range.png", plot=g9,
       width = 5, height = 5, unit = "in", dpi = 300)

# ----- Combine labeled images -------
image1 <- read.img(path="figures/panels/seg_disp_map.png", lb='a')
# image2 <- read.img(path="figures/panels/seg_disp_res.png", lb='b')
image2 <- read.img(path="figures/panels/seg_disp_FD.png", lb='b')
image3 <- read.img(path="figures/panels/venue_share.png", lb='c')
# image4 <- read.img(path="figures/panels/seg_disp_grp_change.png", lb='d')
image4 <- read.img(path="figures/panels/seg_by_poi.png", lb='d')
image5 <- read.img(path="figures/panels/seg_by_mobi_range.png", lb='e')


## Combine images 1-2
# Get width of image 2
image1_height <- image_info(image1)$height

# Create blank space between them and stack three
blank_space_h <- image_blank(2, image1_height, color = "white")
combined_image1 <- image_append(c(image1, blank_space_h, image2), stack = F)

## Combine images 3-4
# Get width of image 2
image3_height <- image_info(image3)$height

# Create blank space between them and stack three
blank_space_h2 <- image_blank(4, image3_height, color = "white")
blank_space_h2s <- image_blank(0.5, image3_height, color = "white")
combined_image2 <- image_append(c(image3, blank_space_h2s, image4, blank_space_h2, image5), stack = F)

## Combine image1 with combined_image1
# Get height of image 1
image1_width <- image_info(combined_image1)$width

# Create a blank space image
blank_space_w <- image_blank(image1_width, 2, color = "white")

# Combine the images side by side
combined_image <- image_append(c(combined_image1, blank_space_w, combined_image2), stack = T)
image_write(combined_image, "figures/panels/seg_disp_fig1.png")

# Appendix Group change and 2D distribution ----
image1 <- read.img(path="figures/panels/seg_disp_res.png", lb='a')
image2 <- read.img(path="figures/panels/seg_disp_grp_change.png", lb='b')
image1_height <- image_info(image1)$height
blank_space_h <- image_blank(4, image1_height, color = "white")
combined_image <- image_append(c(image1, blank_space_h, image2), stack = F)
image_write(combined_image, "figures/panels/seg_disp_fig1_appendix.png")
