




library(MCOE)
library(tidyverse)
library(here)
library(googlesheets4)
library(janitor)
library(ggthemes)



con <- MCOE::mcoe_sql_con()

#### Old 2021 stuff ------
# 
# growth <- tbl(con, "GROWTH") %>%
#     collect()
# 
# schools <- tbl(con, "SCHOOLS") %>%
#     collect() %>%
#     select(CDSCode, District, School, City)
# 
# 
# 
# growth.mry <- growth %>%
#     filter(str_starts(cds,"27")) %>%
#     left_join(schools, by = c("cds" = "CDSCode")) %>%
#     mutate(decilerank = as.numeric(decilerank),
#            growthscore = as.numeric(growthscore))
# 
# 
# 
# growth.mry %>%
#     filter(studentgroup == "SWD",
#            rtype == "D",
#            subject == "math"
#            ) %>%
#     ggplot(aes(y = growthscore, x = reorder(District, growthscore) )) +
#     geom_col() +
#     facet_grid(~subject) +
#     coord_flip() + 
#     theme_hc()
# 
# grow.chart <- function(studentgroupt, level, subjectt){
#     
#     growth.mry %>%
#         filter(studentgroup == studentgroupt,
#                rtype == str_sub(level,1,1),
#                subject == subjectt,
#                !is.na(growthscore)
#         ) %>%
#         ggplot(aes(y = growthscore, x = reorder(District, growthscore) )) +
#         geom_col() +
#         facet_grid(~subject) +
#         coord_flip() +
#         theme_hc()
#     
# }
# 
# 
# grow.chart("SWD","District","math")
# 
# grow.chart("ALL","School","ela")
# 
# 
# 
# growth.by.group <- growth.mry %>%
#      filter(subject == "ela") %>%
#      group_by(studentgroup) %>% 
#      summarize(weighted.mean(growthscore, n_growthscores, na.rm = TRUE))
# 


### Growth from 2025   -------


# Sheet mannually entered by Cinthya
growth <- read_sheet("https://docs.google.com/spreadsheets/d/1U6vCQiIaEGCrAGWbfDAFPPJQz8rOAbZXnIbwtze_h2Y/edit?gid=0#gid=0")


pal <- c("Above" = "#8dd3c7", "Typical" = "#ffffb3", "Below" = "#bebada")

student.growth <- function(grouper = "All", ass = "ELA") {
    
    growth %>% 
        filter(Assessment == ass,
               str_detect(`Student Group` , grouper)
               ) %>%
        ggplot(aes(x = reorder(LEA, Growth), y = Growth, fill = `Growth Category`, label = Growth)) +
        geom_col() +
        geom_text() +
        coord_flip() +
        mcoe_theme  +
        scale_fill_manual(values = pal) +
        labs(title = paste0(grouper," Growth by District for ", ass ),
             subtitle = "Scores represent the typical growth of students with similar test scores in the previous grade level.  \nIf the error range crosses 0, then typical growth is assumed.")
    
}

student.growth("Students with Disabilities", "Math")

student.growth("All", "ELA")

student.growth("Filipino", "ELA")




lea.growth <- function(dist, ass = "ELA") {
    

growth %>% 
    filter(Assessment == ass,
           str_detect(LEA,dist)
           ) %>%
    ggplot(aes(x = reorder(`Student Group`, Growth), y = Growth, fill = `Growth Category`, label = Growth)) +
    geom_col() +
    geom_text() +
    coord_flip() +
    mcoe_theme +
        scale_fill_manual(values = pal) +
    labs(title = paste0(dist," Growth by Student Group for ", ass ),
         subtitle = "Scores represent the typical growth of students with similar test scores in the previous grade level.  \nIf the error range crosses 0, then typical growth is assumed.")

}

lea.growth("King City")

lea.growth("Washington")

lea.growth("Carmel", ass = "Math")

lea.growth("Mission")

lea.growth("Soledad")
