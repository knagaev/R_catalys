library(xml2)
library(purrr)
library(stringr)
library(tidyverse) #load second!


#file_path <- "C:\\PLR\\catalys\\PsDocs\\PsDocs 2017-06-16\\0000038948_U1_20040710_RU\\PatentDocument.xml"
main_path <- "C:\\PLR\\catalys\\PsDocs\\PsDocs 2017-06-16"


get_propvar <- function(xml_file)
{
  id <- tail(str_split(dirname(xml_file), "/")[[1]], 1)
  
  xml <- read_xml(xml_file, encoding = "UTF-8")
  
  title <- unlist(xml_text(xml_find_all(xml, "//ru-b542")))
  description <- unlist(xml_text(xml_find_all(xml, "description")))
  claims <- unlist(xml_text(xml_find_all(xml, "claims")))
  abstract <- unlist(xml_text(xml_find_all(xml, "abstract")))

  tibble(
    id = id,
    title = ifelse(length(title) > 0, title, ""),
    description = ifelse(length(description) > 0, description, ""),
    claims = ifelse(length(claims) > 0, claims, ""),
    abstract = ifelse(length(abstract) > 0, abstract, ""))
}

xres <- system.time(
  psdocs <- dir(main_path, pattern = "*.xml", full.names = TRUE, recursive = TRUE) %>% 
    map(get_propvar) %>% 
    bind_rows
)
