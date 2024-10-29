library(tidyverse);library(here);library(varhandle)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:284)

rm.all.but("GTK_aggregointi_kaikki_maa")


#Tuotantosuunnat ETOL-tuotantosuunniksi, eli ns. tuotantosuuntaryhmiksi
library(readxl)
TSryhma <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                                                        sheet = "Tuotantosuunnat ryhmittäin")
colnames(TSryhma)[1]<-"Tuotantosuunta"


GTK_aggregointi_kaikki_maa<-GTK_aggregointi_kaikki_maa %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                   Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                   Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                   Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                   Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                   Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                   Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                   Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                   .default = Tuotantosuunta))

nrow(inner_join(GTK_aggregointi_kaikki_maa, TSryhma, by="Tuotantosuunta"))
Aineisto<-inner_join(GTK_aggregointi_kaikki_maa, TSryhma, by="Tuotantosuunta")

rm.all.but("Aineisto")


#Kasvit ETOL-tuotetasolle

Tuotteet <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                      sheet = "Kasvit_ETTL_koodeittain")
colnames(Tuotteet)<-c("ETTL","ETTL_nimike","Kasvikoodi","Kasvinimi") 

Aineisto<-inner_join(Aineisto, Tuotteet, by=c("Kasvikoodi"))
Aineisto$Kasvinimi.y<-NULL
colnames(Aineisto)[colnames(Aineisto)=="Kasvinimi.x"]<-"Kasvinimi"
sum(Aineisto$Peltoala_ha)

#Aggregointi

rm.all.but("Aineisto")

Aineisto<-Aineisto %>% group_by(ETTL,ETTL_nimike,ETOL_koodi,ETOL) %>% summarise(Peltoala_hehtaaria = sum(Peltoala_ha))
x<-Aineisto #backup
#Jakauma: tavoitteena laskea, miten tuotteen viljelyala (maalajia erittelemättä) jakautuu etol-toimialojen kesken.

Aineisto<-Aineisto %>% group_by(ETTL, ETTL_nimike) %>% mutate(Prosenttia = Peltoala_hehtaaria/sum(Peltoala_hehtaaria))

t<-Aineisto %>% filter(ETTL_nimike == "Vehnä")
sum(t$Prosenttia)

write.xlsx(Aineisto, here("Output/AreaAggregates/Viljelyalajakauma_gtk_peltodata.xlsx"))
