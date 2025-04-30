library(tidyverse);library(openxlsx);library(here);library(varhandle);library(stringr)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Imputoiduilla P-luvuilla täydennetty peltolohkoaineisto

library(readr)
Kavennettu_lohkodata_imputoituP <- read_delim("Data/Ravinnelaskennan_aineisto/Kavennettu_lohkodata_imputoituP.csv", 
                                              delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                              locale = locale(decimal_mark = ","), 
                                              trim_ws = TRUE)
library(readxl)
Turkistarhaus_kunnittain_2015 <- read_excel("Data/Ravinnelaskennan_aineisto/Turkistarhaus_kunnittain_2015.xlsx", 
                                            sheet = "2017")
Turkistarhaus_kunnittain_2015<-Turkistarhaus_kunnittain_2015 %>% slice(-77:-79)

#Peltolohkokoodeista 3 ekaa nroa eli kunnan koodi irti 

Kavennettu_lohkodata_imputoituP$Kuntakoodi<-substr(Kavennettu_lohkodata_imputoituP$PLTUNNUS, start = 1, stop = 3)

#Irrotetaan liikevaihtodatasta turkistarhausta sisältävien kuntien koodit. Tässä tapauksessa vain keskittymät.  

Turkiskunnat <- Turkistarhaus_kunnittain_2015 %>%  filter(!(is.na(Keskittymä)))
Turkiskunnat<-unique(Turkistarhaus_kunnittain_2015$Kuntakoodi)

Turkiskuntien_pellot<- Kavennettu_lohkodata_imputoituP %>% filter(Kuntakoodi %in% Turkiskunnat)

#Sitten kaikkien muiden paitsi turkiskuntien pellot

Muiden_kuntien_pellot<- Kavennettu_lohkodata_imputoituP %>% filter(!(Kuntakoodi %in% Turkiskunnat))

rm.all.but(c("Muiden_kuntien_pellot","Turkiskuntien_pellot"))

#Turkiskuntien P-luvut viljelykasveittain

turkisk_aggre<-Turkiskuntien_pellot %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku_turkiskunnat = mean(KESKIARVO_))

#Muiden kuntien vastaava aggregointi

muutk_aggre<- Muiden_kuntien_pellot %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku_muut_kunnat = mean(KESKIARVO_))


#Liitto

liitos<-left_join(muutk_aggre, turkisk_aggre, by=c("KASVIKOODI_lohkodata_reclass", "KASVINIMI_reclass"))

write.xlsx(liitos, file=here("P_lukuvertailu_turkiskunnat_muut.xlsx"))


