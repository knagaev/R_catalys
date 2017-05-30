rm(list=ls()) # очистим все переменные
library(ggplot2) #load first! (Wickham)
library(tidyverse) #load second!

#Sys.setlocale("LC_CTYPE", "Russian_Russia.1251")

mm_file_name <- "ВТД.Collection_Matrix_Baseline4.xlsx"
facets_file_name <- "facets_VTD.csv"

tb_merged_matrix <- readxl::read_excel(mm_file_name, sheet = 'merged_master_families_matrix')
df_facets <- read_csv(facets_file_name)

tb_heat_years<-
  tb_merged_matrix %>% 
    select(OldPriorYear, matches("[1-9]+."), -starts_with("5.")) %>% 
    gather(key = "facet", value = 'indicator', -OldPriorYear) %>% 
    group_by(OldPriorYear, facet) %>% 
    summarise(qty = sum(indicator)) %>% 
    ungroup %>% 
    complete(OldPriorYear = as.character(full_seq(as.integer(OldPriorYear), 1)), facet, fill = list(qty = 0))

ggplot(tb_heat_years %>% 
    filter(startsWith(facet, "4.")), aes(OldPriorYear, facet)) +
  geom_tile(aes(fill = qty)) +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = guide_legend(title = "Количество")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Основание') + 
  xlab('Годы') +
  guides(title = "Количество")
