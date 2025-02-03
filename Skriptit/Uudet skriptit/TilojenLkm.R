source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

library(here)

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:263 )


x<-GTKdata %>% select(MAATILA_TUNNUS, Tuotantosuunta) %>% unique(.) %>% mutate(Laskuri = 1)  
y<-x %>% group_by(Tuotantosuunta)  %>% summarise(Tiloja =sum(Laskuri)) 
  
 
  