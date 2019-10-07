library(tidyverse)
library(reshape2)
library(forcats)
require(edgebundleR)

med_dat <- tribble(
  ~Sector, ~Резиденты, ~Нерезиденты,
  #--|--|----
"ДГ",	1121, 1460,
"ХР",	941, 642,
"СТ",	11, 6,
"ИУ",	378, 241,
"ОУ",	1, 1,
"ГН",	8, 6,
"ПМ",	443, 230,
"УзБ", 169, 125,
"ФР",	429, 260,
"СМ",	88, 51,
"ВЛП", 798, 430,
"ЛД",	408, 241,
"АМ",	115, 85
)

med_dat2 <- melt(med_dat, id.vars = "Sector")

rat_colors <- c("lightgoldenrod1", "lightgoldenrod3")

ggplot(med_dat2, 
       aes(#x=Sector,
         x=fct_reorder2(Sector, variable, value, .desc = FALSE),
         y=value,
         fill=variable)) + 
  geom_bar(stat="identity", width=.7) +
  labs(x = NULL, y = NULL) +
  guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  scale_fill_manual(values=rat_colors) + 
  scale_x_discrete(expand = c(.4, 0)) +
  scale_y_continuous(limits = c(-500, 2600)) +
  coord_polar(start = pi)


ggplot(med_dat2, 
       aes(#x=Sector,
         x=fct_reorder2(Sector, variable, value, .desc = FALSE),
         y=value,
         fill=variable)) + 
  geom_bar(stat="identity", width=.7) +
  labs(x = NULL, y = NULL) +
  guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  scale_fill_manual(values=rat_colors) + 
  #scale_x_discrete(expand = c(.5, 0)) +
  #scale_y_continuous(limits = c(-500, 2600)) +
  scale_y_reverse(limits = c(3000, 0)) +
  coord_polar(start = pi)


med_dat_rat <- tribble(
  ~Sector, ~Резиденты, ~Нерезиденты,
  #--|--|----
  "ДГ", 43.43, 56.57,
  "ХР", 59.44, 40.56,
  "СТ", 64.71, 35.29,
  "ИУ", 61.07, 38.93,
  "ОУ", 50.00, 50.00,
  "ГН", 57.14, 42.86,
  "ПМ", 65.82, 34.18,
  "УзБ", 57.48, 42.52,
  "ФР", 62.26, 37.74,
  "СМ", 63.31, 36.69,
  "ВЛП", 64.98, 35.02,
  "ЛД", 62.87, 37.13,
  "АМ", 57.50, 42.50
)

med_dat2_rat <- melt(med_dat_rat, id.vars = "Sector")

rat_colors <- c("lightgoldenrod1", "lightgoldenrod3")

ggplot(med_dat2_rat, 
       aes(#x=Sector,
           x=fct_reorder2(factor(Sector), variable, value),
           y=value,
           fill=factor(variable))) + 
  geom_bar(stat="identity", width=.7) +
  labs(x = NULL, y = NULL) +
  guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  scale_fill_manual(values=rat_colors) + 
  #scale_x_discrete(expand = c(.5, 0)) +
  scale_y_continuous(limits = c(-20, 100), labels=NULL, breaks = NULL) +
  geom_text(aes(label=round(value)), size = 3, position = position_stack(vjust = 0.5)) +
  coord_polar(start = pi)


  #scale_y_continuous(limits = c(2600, -500)) +
  # + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # + theme(axis.text.x=element_text(vjust=1,hjust=0))
  