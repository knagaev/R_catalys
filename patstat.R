library(tidyverse)

patstat <- read_csv("C:\\work\\others\\Patstat\\tls202_part01.txt")

#!patstat$row <- patstat %>% mutate(row = row_number())

p2 <- patstat %>% filter(appln_title_lg == "bg") %>% head()

lg <-distinct(appln_title_lg)

first_rows <- patstat %>% mutate(row = row_number()) %>% group_by(appln_title_lg) %>% slice(1)




patstat <- NULL
