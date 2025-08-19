
#Katsotaan, korreloiko lannan fosforitonnit/ha - kerroin merkitsevästi tilatasolla keskimääräisen p-luvun kanssa

library(openxlsx);library(tidyverse);library(here)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Täältä viljavuusdatan P-lukutiedot: käytetään tätä eikä inputointia, 
#koska myös alkuperäinen validointivertailu (korrelaatio keskim. Pluku versus lannan fosfori, mutta tuotantosuuntatasolla,)
#tehtiin näistä


source_lines(here("Skriptit/Ravinnelaskenta/Pluku_keskiarvo_kasvit_tuotantosuunnat.R"),1:31)

Pluku_tilat<-Viljavuusdata %>% group_by(MAATILA_TUNNUS, ETOL) %>% summarise(Pluku_keskiarvo = mean(KESKIARVO_))
Viljelyala_tilat<- Viljavuusdata %>% group_by(MAATILA_TUNNUS, ETOL) %>% summarise(Viljelyala_ha = sum(KASVI_ALA_HA))

#Viljelyala ja keskimäär. P yhteen
Pluku_alat_tila<-inner_join(Pluku_tilat, Viljelyala_tilat, by=c("MAATILA_TUNNUS","ETOL"))
    
rm(Pluku_tilat, Viljelyala_tilat, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, Viljavuusdata)
#Täältä KAIKKIEN lantaa tuottavientilojen (ei vain viljavuustilojen) eläinmääräperusteinen lantafosforitonnit

source_lines(here("Skriptit/Ravinnelaskenta/Lannan_ravinteet_tiloittain.R"),1:45)

rm.all.but(c("Pluku_alat_tila","Elainmaarat"))

Fosforitonnit_tilat<-Elainmaarat %>% group_by(Tilatunnus) %>% summarise(Varastolannan_fosfori_kg_tiloittain = sum(Varastolannan_fosfori_kg))   
colnames(Fosforitonnit_tilat)[1]<-"MAATILA_TUNNUS"

Data<-inner_join(Fosforitonnit_tilat, Pluku_alat_tila, by="MAATILA_TUNNUS")

rm.all.but("Data")

#Lantaa per ha-kerroin

Data<-Data %>% mutate(Lantafosfori_kg_per_ha = Varastolannan_fosfori_kg_tiloittain/Viljelyala_ha)

#Tästä joukosta eläintilat irti, muut ulos

Elaintilat<-c("Maitotilat",
"Muut nautakarjatilat",
"Hevostilat",
"Lammas- ja vuohitilat",
"Sikatilat",
"Munatilat",
"Siipikarjatilat",
"Turkistarhaus")

Elaintiladata<-Data %>% filter(ETOL %in% Elaintilat)

#Tiloittaisen plukukeskiarvon ja lantafosfori-per-ha kertoimen välinen korrelaatio 

cor.test(Elaintiladata$Lantafosfori_kg_per_ha, Elaintiladata$Pluku_keskiarvo)

