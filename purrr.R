library(xml2)
library(purrr)

main_path <- 'C:\\_in\\prod\\advprop\\advan\\'


pvis <- dir(main_path) %>% 
  paste0(main_path, .) %>% 
  map(read_xml) %>% 
  map(~xml_text(xml_find_all(.,"//advp:PropertyVarietyId/wsrls:Id"))) %>% 
  unlist %>% 
  table


main_path <- './uc/'


dir(main_path) %>% 
  paste0(main_path, .) %>% 
  map(read_xml) %>% 
  map(~xml_text(xml_find_all(.,"//time/start_time"))) %>% 
  unlist %>% 
  table
