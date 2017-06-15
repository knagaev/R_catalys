rm(list=ls()) # очистим все переменные
library(ggplot2) #load first! (Wickham)
library(tidyverse) #load second!

setwd(".")
#Sys.setlocale("LC_CTYPE", "Russian_Russia.1251")

mm_file_name <- "data\\ВТД.Collection_Matrix_ver_0_16.xlsx"
facets_file_name <- "facets_VTD.csv"

tb_merged_matrix <- readxl::read_excel(mm_file_name, sheet = 'merged_master_families_matrix')
df_facets <- read_csv(facets_file_name)

tb_merged_matrix[is.na(tb_merged_matrix) & col(isna) %in% str_which(names(tb_merged_matrix), "^\\d.+")] <- 0
tb_merged_matrix <- tb_merged_matrix %>% drop_na(OldPriorYear)


tb_heat_years<-
  tb_merged_matrix %>% 
    select(OldPriorYear, matches("[1-9]+."), -starts_with("5.")) %>% 
    gather(key = "facet", value = 'indicator', -OldPriorYear) %>% 
    group_by(OldPriorYear, facet) %>% 
    summarise(qty = sum(indicator)) %>% 
    ungroup %>% 
    complete(OldPriorYear = full_seq(OldPriorYear, 1), facet, fill = list(qty = 0)) %>% 
    filter(OldPriorYear >= 1957) %>% 
    inner_join(df_facets, by = c("facet" = "facet_id"))

ggplot(tb_heat_years %>% 
    filter(startsWith(facet, "4.")), aes(OldPriorYear, facet)) +
  geom_tile(aes(fill = qty)) +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = guide_legend(title = "Количество")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(min(tb_heat_years$OldPriorYear), max(tb_heat_years$OldPriorYear), by = 5)) +
  ylab('Основание') + 
  xlab('Годы') +
  guides(title = "Количество")


  ###### попробовать

# Final plot of last exercise
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()

# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues") # from the RColorBrewer package

# 1 - Make a color range using colorRampPalette() and the set of blues
blue_range  <- colorRampPalette(blues)

# 2 - Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11))


