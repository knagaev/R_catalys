library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)

mm_file_name <- "C:/PLR/catalys/ВТД.Collection_Matrix_Baseline4.xlsx"
facets_file_name <- "C:/PLR/catalys/facets_VTD.csv"

#mm_col_types <- readxl:::xlsx_col_types(mm_file_name)
#tb_merged_matrix <- readxl::read_excel(mm_file_name, sheet = 'merged_master_families_matrix', col_types = mm_col_types)
tb_merged_matrix <- readxl::read_excel(mm_file_name, sheet = 'merged_master_families_matrix')
df_facets <- read_csv(facets_file_name)

#base_columns <- tb_merged_matrix %>% 
#  select(matches("[1-9]+."))

#tb_merged_matrix %>% 
#  cbind(sapply(unique(substr(names(base_columns), 1, 2)), 
#               function(i) rowSums(base_columns[, startsWith(names(base_columns), i)])))

tb_full_years <- as_tibble(expand.grid(OldPriorYear = as.character(seq(from = min(as.integer(tb_merged_matrix$OldPriorYear)), 
                                     to = max(as.integer(tb_merged_matrix$OldPriorYear)))),
                           facet = str_subset(names(tb_merged_matrix), "[1-9]+."),
                            KEEP.OUT.ATTRS = FALSE,
                            stringsAsFactors = FALSE))



tb_heat_years<-
  tb_merged_matrix %>% 
    select(OldPriorYear, matches("[1-9]+."), -starts_with("5.")) %>% 
    mutate(OldPriorYear = str_trim(OldPriorYear)) %>% 
    gather(key = "facet", value = 'indicator', -OldPriorYear) %>% 
    group_by(OldPriorYear, facet) %>% 
    summarise(qty = sum(indicator)) #%>% 
    #complete(OldPriorYear = as.character(seq(from = min(as.integer(tb_merged_matrix$OldPriorYear)), to = max(as.integer(tb_merged_matrix$OldPriorYear)))), facet)
    #complete(OldPriorYear = full_seq(OldPriorYear), 1)
    #complete(OldPriorYear = as.character(full_seq(as.integer(OldPriorYear), 1)), facet)


tb_heat_years <-
  tb_heat_years %>% 
    right_join(tb_full_years, by = c("OldPriorYear", "facet"))

tb_heat_years %>% 
  filter(startsWith(facet, "4."))

ggplot(tb_heat_years %>% 
         filter(startsWith(facet, "4.")), aes(OldPriorYear, facet)) +
  geom_tile(aes(fill = qty)) 
  

# full_years <- as.character(full_seq(as.integer(tb_heat_years$OldPriorYear), 1))
# full_facets <- str_subset(names(tb_merged_matrix), "[1-9]+.")
completed_heat_years <-
  tb_heat_years %>%
  complete(OldPriorYear = as.character(full_seq(as.integer(.$OldPriorYear), 1)), facet)

tb_problems <- 
  tb_merged_matrix %>% 
    select(`BasePubl-DWPI`, starts_with("5.")) %>% 
    gather(key = "facet", value = 'indicator', -`BasePubl-DWPI`)

tb_solutions <- 
  tb_merged_matrix %>% 
    select(`BasePubl-DWPI`, matches("[1-9]+."), -starts_with("5.")) %>% 
    gather(key = "facet", value = 'indicator', -`BasePubl-DWPI`)

tb_heat_problems <-
  tb_problems %>% 
    inner_join(tb_solutions, by = "BasePubl-DWPI", suffix = c(".p", ".s")) %>% 
    group_by(facet.p, facet.s) %>% 
    summarise(qty = sum(indicator.p * indicator.s))

d3heatmap(mtcars %>% select(-model), scale = "column", colors = "Spectral")

tb_heat_years[is.na(tb_heat_years)] <- 0

t4 <- tb_heat_years %>% 
  filter(startsWith(facet, "4.")) %>% 
  spread(facet, qty)
rownames(t4) <- t4$OldPriorYear
t4$OldPriorYear <- NULL

d3heatmap(t4 %>% t, 
          colors = "Blues",
          dendrogram = 'none')

