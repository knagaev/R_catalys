library(xml2)
library(purrr)
library(stringr)

main_path <- 'C:\\_in\\prod\\advprop\\advan\\'


pvis <- dir(main_path) %>% 
  paste0(main_path, .) %>% 
  map(read_xml(.) %>% 
    map(~xml_text(xml_find_all(.,"//advp:PropertyVarietyId/wsrls:Id")))) %>% 
  unlist %>% 
  table


dir(main_path) %>% 
  paste0(main_path, .) %>%
  map(~paste0(readLines(.), collapse = " ")) %>% 
  map(~str_match_all(., "<advp:PropertyVarietyId>.+?<wsrls:Id>(\\d+?)</wsrls:Id>")[[1]][,2]) %>%
  unlist %>% 
  table

xmls <- dir(main_path) %>% 
  paste0(main_path, .) %>% 
  map()

xmls %>% 
map(xml_find_all(., "//advp:PropertyVarietyId/wsrls:Id"))

xml_find_all(xmls[[1]], "//advp:PropertyVarietyId/wsrls:Id")

xmls %>% 
  map(print)

file_name <- "C:\\_in\\prod\\advprop\\test\\0010229298.01.0010229249.xml"

x <- read_xml("C:\\_in\\prod\\advprop\\test\\0010229298.01.0010229249.xml")
xml_find_all(x, "//advp:PropertyVarietyId/wsrls:Id")
stri_match_all_regex(x, '<advp:PropertyVarietyId>\\n.+?<wsrls:Id>(\\d+?)</wsrls:Id>')[[1]][,2]
stri_extract_all_regex(x, '(?<=<advp:PropertyVarietyId>\\n          <wsrls:Id>).*?(?=</wsrls:Id>)', perl = TRUE)

main_path <- 'C:\\_in\\prod\\advprop\\test\\'

get_re_propvar <- function(file_name)
{
  #x <- paste(readLines(file_name), collapse=" ")
  x <- readChar(file_name, file.info(file_name)$size)
  pattern <- "<advp:PropertyVarietyId>\\n.+?<wsrls:Id>(\\d+?)</wsrls:Id>"
  #str_match_all(x, pattern)[[1]][,2]
  #stri_match_all_regex(x, '<advp:PropertyVarietyId>\\n.+?<wsrls:Id>(\\d+?)</wsrls:Id>')[[1]][,2]
  stri_extract_all_regex(x, '(?<=<advp:PropertyVarietyId>\\n          <wsrls:Id>).*?(?=</wsrls:Id>)', perl = TRUE)
}

res <- system.time(
  pvis <- dir(main_path) %>% 
    paste0(main_path, .) %>% 
    map(get_re_propvar) %>% 
    unlist %>% 
    table
)

get_propvar <- function(xml_file)
{
  xml_text(xml_find_all(read_xml(xml_file), "//advp:PropertyVarietyId/wsrls:Id"))
}

xres <- system.time(
  xpvis <- dir(main_path) %>% 
    paste0(main_path, .) %>% 
    map(get_propvar) %>% 
    unlist %>% 
    table
)

get_propvar <- function(xml_file)
{
  xml_text(xml_find_all(read_xml(xml_file), "//wsrls:Type"))
}

xres <- system.time(
  xpvis <- dir(main_path) %>% 
    paste0(main_path, .) %>% 
    map(get_propvar) %>% 
    unlist %>% 
    table
)

ttt <- dir(main_path) %>% 
  paste0(main_path, .) %>%
  tibble(file_name = .) %>% 
  group_by(file_name) %>% 
  do(tibble(t = get_propvar(.$file_name))) %>% 
  .$t %>% 
  table

names(ttt) <- c("file_name")

xres <- system.time(
  xpvis <- dir(main_path) %>% 
    paste0(main_path, .) %>%
    tibble(file_name = .) %>% 
    rowwise() %>% 
    do(tibble(t = get_propvar(.$file_name))) %>% 
    .$t %>% 
    table
)

xres <- system.time(
  xpvis <- table(data.table(file_name = paste0(main_path, dir(main_path)))[, .(t = get_propvar(file_name)), by = file_name][, t])
)


xres <- system.time(
  xpvis <- table(data.table(file_name = paste0(main_path, dir(main_path)))[, .(t = get_re_propvar(file_name)), by = file_name][, t])
)


