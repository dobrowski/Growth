




library(MCOE)
library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(janitor)
library(ggthemes)



con <- MCOE::mcoe_sql_con()


drive_auth(email = "ddobrowski@montereycoe.org")
gs4_auth(token = drive_token())



sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"




growth <- tbl(con, "GROWTH") %>%
    collect()

schools <- tbl(con, "SCHOOLS") %>%
    collect() %>%
    select(CDSCode, District, School, City)



growth.mry <- growth %>%
    filter(str_starts(cds,"27")) %>%
    left_join(schools, by = c("cds" = "CDSCode")) %>%
    mutate(decilerank = as.numeric(decilerank))

growth.mry %>%
    filter(studentgroup == "ALL",
           rtype == "D",
           subject == "ela"
           ) %>%
    ggplot(aes(y = growthscore, x = reorder(District, growthscore) )) +
    geom_col() +
    facet_grid(~subject) +
    coord_flip() + 
    theme_hc()


