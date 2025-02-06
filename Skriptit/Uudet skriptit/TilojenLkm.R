source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

library(here)
c
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:263 )


x<-GTKdata %>% select(MAATILA_TUNNUS, Tuotantosuunta) %>% unique(.) %>% mutate(Laskuri = 1)  
y<-x %>% group_by(Tuotantosuunta)  %>% summarise(Tiloja =sum(Laskuri)) 

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-"Tuotantosuunta"

Muuntoavain_tuotantosuunnat_tuotteet_ETOL<-Muuntoavain_tuotantosuunnat_tuotteet_ETOL %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))

x<-x %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                                           Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                                           Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                                           Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                                           Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                                           Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                                                                           Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                                                                           Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                                                                           .default = Tuotantosuunta))



z<-merge(x, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta")

z<-z %>% group_by(ETOL, ETOL_koodi)  %>% summarise(Tiloja =sum(Laskuri)) 


library(openxlsx)

Lukumaarat<-createWorkbook()
addWorksheet(Lukumaarat, "Tuotantosuunta_orig")
addWorksheet(Lukumaarat, "Tuotantosuunta_ETOL")
writeData(Lukumaarat, "Tuotantosuunta_orig", y)
writeData(Lukumaarat, "Tuotantosuunta_ETOL", z)
saveWorkbook(Lukumaarat, file=here("Output/AreaAggregates/Tilalukumaarat_gtk.xlsx"))
