library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)


mm_file_name <- "ВТД.Collection_Matrix_Baseline4.xlsx"
facets_file_name <- "facets_VTD.csv"

tb_merged_matrix <- readxl::read_excel(mm_file_name, sheet = 'merged_master_families_matrix')
df_facets <- read_csv(facets_file_name)

tb_heat_years<-
  tb_merged_matrix %>% 
  select(OldPriorYear, matches("[1-9]+."), -starts_with("5.")) %>% 
  gather(key = "facet", value = 'indicator', -OldPriorYear) %>% 
  group_by(OldPriorYear, facet) %>% 
  summarise(qty = sum(indicator)) #%>% 



#tb_heat_years <- readRDS("tb_heat_years.rds")

# все годы - вспомогательная для right_join - 44 штуки
all_years <- as.character(seq(from = min(as.integer(tb_heat_years$OldPriorYear)), 
                              to = max(as.integer(tb_heat_years$OldPriorYear))))

# все параграфы - вспомогательная для right_join - 88 штук
all_facets <- tb_heat_years %>% .$facet %>% unique()

# все комбинации года и параграфа - 3872 записи = 44 * 88
tb_full_years <- as_tibble(expand.grid(OldPriorYear = all_years,
                                       facet = all_facets,
                                       KEEP.OUT.ATTRS = FALSE,
                                       stringsAsFactors = FALSE))


# right_join отрабатывает правильно - 3872 записи 
tb_heat_years_join <-
  tb_heat_years %>% 
  right_join(tb_full_years, by = c("OldPriorYear", "facet"))

# complete попытка №1 - 143264 записи, размножаются годы, 3256 * 44 (3256 из начальной tb_heat_years и 44 года)
tb_heat_years_complete1 <-
  tb_heat_years %>% 
  complete(OldPriorYear = all_years, facet = all_facets)

# complete попытка №2 - те же 143264 записи
tb_heat_years_complete2 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(seq(from = min(as.integer(tb_heat_years$OldPriorYear)), to = max(as.integer(tb_heat_years$OldPriorYear)))), facet)

# complete попытка №3 - 3256 записей (как в начальной tb_heat_years)
tb_heat_years_complete3 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(full_seq(as.integer(.$OldPriorYear), 1)), facet = all_facets)

# при этом получение последовательности лет правильное - 44 года 
full_seq(as.integer(tb_heat_years$OldPriorYear), 1)

tb_heat_years_ungrouped <- tb_heat_years %>% ungroup()

