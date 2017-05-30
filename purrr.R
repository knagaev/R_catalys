# library(xml2)
# library(purrr)
# 
# main_path <- 'C:\\_in\\prod\\advprop\\advan\\'
# 
get_propvar <- function(xml_file)
{
  xml_text(xml_find_all(read_xml(xml_file), "//advp:PropertyVarietyId/wsrls:Id"))
}

res <- system.time(
pvis <- dir(main_path) %>%
  paste0(main_path, .) %>%
  map(get_propvar) %>%
  unlist %>%
  table
)
# 
# main_path <- './uc/'
# 
# 
# dir(main_path) %>% 
#   paste0(main_path, .) %>% 
#   map({~read_xml(.)} %>% 
#     map(~xml_text(xml_find_all(.,"//time/start_time")))) %>% 
#   unlist %>% 
#   table
# 
# 
# main_path <- 'C:\\_in\\prod\\advprop\\test\\'
# 
# 
# pvis <- dir(main_path) %>% 
#   paste0(main_path, .) %>% 
#   map({read_xml(.) %>% 
#         {map(~xml_text(xml_find_all(.,"//advp:PropertyVarietyId/wsrls:Id")))}}) %>% 
#   unlist %>% 
#   table


#vzr

main_path <- 'C:\\_in\\prod\\new_vzr\\01'

get_propvar <- function(xml_file)
{
  c(xml_text(xml_find_all(read_xml(xml_file), "//vzr:ProductId/wsrls:Id")), xml_file)
}

res <- system.time(
  pvis <- dir(main_path, full.names = TRUE, pattern = "*.xml") %>% 
  map(get_propvar) %>% 
  unlist %>% 
#  unlist %>% 
  matrix(., ncol = 2, byrow = TRUE) %>% 
#  t %>% 
  as_tibble
)

###############

main_path <- 'C:\\_in\\prod\\new_vzr\\01'

get_propvar <- function(xml_file)
{
  xml_text(xml_find_all(read_xml(xml_file), "//vzr:ProductId/wsrls:Id"))
}

res <- system.time(
  pvis <- dir(main_path, full.names = TRUE, pattern = "*.xml") %>% 
    map(get_propvar) %>% 
    unlist %>% 
    table
)


