library(tidyverse)
library(openxlsx)
library(stringi)

load("./Maps/russia_adm_map.RData")
regions_counts <- read.xlsx("./Maps/russia_regions_map_data.xlsx")

# Data prep
names(regions_counts)[1] <- "NAME"

## Use stringi to transliterate

russia_adm_map$NAME <- stri_trans_general(russia_adm_map$NAME, 'cyrillic-latin')
regions_counts$NAME <- stri_trans_general(regions_counts$NAME, 'cyrillic-latin')

## Convert to lower case for both

russia_adm_map$NAME <- str_to_lower(russia_adm_map$NAME)
regions_counts$NAME <- str_to_lower(regions_counts$NAME)

## Non-matching names

x <- levels(factor(russia_adm_map$NAME))
y <- as.character(regions_counts$NAME)
x[!(x %in% y)]


regions_counts$NAME <- str_replace_all(regions_counts$NAME, c("respublika adygeâ" = "adygeâ",
                                                            "respublika altaj" = "altaj",
                                                            "respublika baškortostan" = "baškortostan",
                                                            "respublika burâtiâ" = "burâtiâ",
                                                            "respublika dagestan" = "dagestan",
                                                            "respublika ingušetiâ" = "ingušetiâ",
                                                            "kabardino-balkarskaâ respublika" = "kabardino-balkarskaâ respublika",
                                                            "karačaevo-čerkesskaâ respublika" = "karačaevo-čerkesskaâ respublika",
                                                            "respublika marij él" = "marij él",
                                                            "g.moskva" = "moskva",
                                                            "neneckij avt. okrug" = "neneckij avtonomnyj okrug",
                                                            "g.sankt-peterburg " = "sankt-peterburg",
                                                            "respublika severnaâ osetiâ - alaniâ" = "severnaâ osetiâ - alaniâ",
                                                            "respublika tatarstan \\(tatarstan\\)" = "tatarstan",
                                                            "respublika tyva" = "tyva",
                                                            "udmurtskaâ respublika" = "udmurtskaâ respublika",
                                                            "hanty-mansijskij avt. okrug-ûgra" = "hanty-mansijskij avtonomnyj okrug - ûgra",
                                                            "čečenskaâ respublika" = "čečenskaâ respublika",
                                                            "čuvašskaâ respublika \\- čuvašiâ" = "čuvašiâ",
                                                            "âmalo-neneckij avt. okrug" = "âmalo-neneckij avtonomnyj okrug"))

regions_counts_map_data <- subset(russia_adm_map, NAME %in% regions_counts$NAME)
regions_counts_map_data <- regions_counts_map_data %>%
  select(long, lat, group, NAME) %>%
  left_join(regions_counts, by="NAME")

#Default margins in inches:  par(mai = c(1, 0.8, 0.8, 0.4) + 0.02) #bottom, left, top and right

par(mai = c(.5, 0.8, 0.8, 0.4) + 0.02)

# Plot -- mobilization coefs
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

regions_counts_map <- ggplot(russia_adm_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(color="grey", opacity=0.5, fill="white") +
  coord_equal() + theme_opts +
  geom_polygon(data=regions_counts_map_data, aes(x = long, y = lat, group = group, 
                                         fill=as.numeric(n_cases_142_1))) +
  scale_fill_viridis_c(direction = -1, alpha = .5, option = "D", guide = guide_legend(title = "Mobilization"))

ggsave(regions_counts_map, file="case_count_142_1_map.tiff")

 #I am not clear on how to add labels to the map directly--need to calculate a lat-long point for each label. For now, 
 # adding 'in post' using image editor


