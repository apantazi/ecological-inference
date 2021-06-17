library(eiCompare)
library(sf)
library(ggpubr)
library(ggplot2)
library(tidyverse)

#2012-2020 votes with results+turnout by precinct
votes <- read_csv("C:/Users/Andrew/Documents/2012_2020_duval_turnout_votes.csv",col_types = "cDcdddddddddddddddddddddddc")

#precinct map
map <- read_sf("C:/Users/Andrew/Downloads/PRECINCT12031-2/PRECINCT12031.shp")

#join votes/turnout with map
vote_map <- full_join(map,votes,by="PRECINCT")


#plotting rules
options(repr.plot.width = 7.2, repr.plot.height = 6)


#### map precincts by white vote share,  but this is the 2020 shapefile, and they shifted precincts over the years ####
turnout_map <- ggplot() +
  geom_sf(data = vote_map, aes(fill = pct_white)) +
  facet_wrap(vars(Date))+
  scale_fill_continuous(limits = c(0, 1)) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 5)),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  guides(fill = guide_legend(
    title = "Fraction\nWhite",
    title.position = "top",
    title.size = 10
  ))

#### map precincts by dem vote share, but this is the 2020 shapefile, and they shifted precincts over the years ####

result_map <- ggplot() +
  geom_sf(data = vote_map, aes(fill = pct_dem)) +
  facet_wrap(vars(Date))+
  scale_fill_continuous(limits = c(0, 1)) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 5)),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  guides(fill = guide_legend(
    title = "Fraction\nDem",
    title.position = "top",
    title.size = 10
  ))

show(turnout_map)


#### results+turnout by election ####

votes12 <- votes %>% 
  filter(Date < "2013-01-01") #2012 presidential
votes14 <- votes %>% 
  filter(Date == as.Date("2014-11-04")) #2014 governor
votes15first <- votes %>% 
  filter(Date == as.Date("2015-03-24")) #2015 first-round mayoral
votes15second <- votes %>% 
  filter(Date == as.Date("2015-05-19")) #2015 mayoral runoff
votes16 <- votes %>% 
  filter(Date == as.Date("2016-11-08")) #2016 presidential
votes18 <- votes %>% 
  filter(Date == as.Date("2018-11-06")) #2018 gubernatorial
votes19 <- votes %>% 
  filter(Date == as.Date("2019-03-19")) #2019 tax collector (there wasn't a partisan mayoral race)
votes20 <- votes %>% 
  filter(Date == as.Date("2020-11-03")) #2020 presidential

#### what even is data ####

cands <- c("DEM_candidate_votes", "REP_candidate_votes", "OTHER_candidate_votes")
races <- c("White_Turnout", "Black_Turnout", "Hispanic_Turnout", "Other_race_Turnout")
total <- "totvote"
id <- "PRECINCT"


#### is the data good? yes it's good ####

votes <- resolve_missing_vals(data = votes,
                              cand_cols = cands,
                              race_cols = races,
                              totals_col = total,
                              na_action = "DROP")

votes12 <- dedupe_precincts(
  data = votes12,
  id_cols = id
)
votes14 <- dedupe_precincts(
  data = votes14,
  id_cols = id
)
votes15first <- dedupe_precincts(
  data = votes15first,
  id_cols = id
)
votes15second <- dedupe_precincts(
  data = votes15second,
  id_cols = id
)
votes16 <- dedupe_precincts(
  data = votes16,
  id_cols = id
)
votes18 <- dedupe_precincts(
  data = votes18,
  id_cols = id
)
votes19 <- dedupe_precincts(
  data = votes19,
  id_cols = id
)
votes20 <- dedupe_precincts(
  data = votes20,
  id_cols = id
)

#### standardized proportions of votes ####

votes12_ei <- stdize_votes_all(
  data = votes12,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)

votes14_ei <- stdize_votes_all(
  data = votes14,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)
votes15first_ei <- stdize_votes_all(
  data = votes15first,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)
votes15second_ei <- stdize_votes_all(
  data = votes15second,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)

votes16_ei <- stdize_votes_all(
  data = votes16,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)

votes18_ei <- stdize_votes_all(
  data = votes18,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)
votes19_ei <- stdize_votes_all(
  data = votes19,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)
votes20_ei <- stdize_votes_all(
  data = votes20,
  cand_cols = cands,
  race_cols = races,
  totals_col = total
)


cand_sums12 <- sum_over_cols(data = votes12_ei, cols = cands)
race_sums12 <- sum_over_cols(data = votes12_ei, cols = races)
table(cand_sums, race_sums)

#### plots ####

plot_12 <- plot_bivariate(
  data = votes12_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)

plot_14 <- plot_bivariate(
  data = votes14_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)

plot_15first <- plot_bivariate(
  data = votes15first_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)

plot_15second <- plot_bivariate(
  data = votes15second_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)


plot_16 <- plot_bivariate(
  data = votes16_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)

plot_18 <- plot_bivariate(
  data = votes18_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)

plot_19 <- plot_bivariate(
  data = votes19_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
)


plot_20 <- plot(plot_bivariate(
  data = votes20_ei,
  cand_cols = cands,
  race_cols = races,
  corrs=TRUE
), main = "2020-Presidential")

plot(plot_20,main="2020-Presidential")

arrange <- ggarrange(plot_12, plot_14, plot_15first, plot_15second, plot_16,plot_18,plot_19,plot_20) 


ggsave("arrangedplot.png", arrange, width = 30, height = 30)

#### Ecological Inferences ####

ei_results_iter_12 <- ei_iter(
  data = votes12_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_12)

ei_results_iter_14 <- ei_iter(
  data = votes14_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_14)

ei_results_iter_15first <- ei_iter(
  data = votes15first_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_15first)

ei_results_iter_15second <- ei_iter(
  data = votes15second_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_15second)

ei_results_iter_16 <- ei_iter(
  data = votes16_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_16)

ei_results_iter_18 <- ei_iter(
  data = votes18_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_18)

ei_results_iter_19 <- ei_iter(
  data = votes19_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_19)

ei_results_iter_20 <- ei_iter(
  data = votes20_ei,
  cand_cols = cands,
  race_cols = races,
  totals_col = total,
  name = "Iter"
)
summary(ei_results_iter_20)

cand_cols <- c('pct_dem','pct_rep','pct_other')
race_cols <- c('pct_white','pct_black','pct_non_bw')
totals_col <- "totvote"

ei_results <- ei_iter(votes20, cand_cols, race_cols, totals_col,
                      plots = TRUE, plot_path = "ei_plot.png"
)
ei_rxc_results <- ei_rxc(data=votes20,
                         cand_cols, race_cols, totals_col,
                         diagnostic = TRUE,
                         par_compute = TRUE,
                         verbose = TRUE,
                         plot_path = "ei_plot.png"
)
