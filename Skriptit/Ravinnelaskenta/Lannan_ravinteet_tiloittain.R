#27.11.24 HV
#Varastolannan (pelloille levitettävän) sisältämien typpi- ja fosforimassan laskenta tilakohtaisesti
#Eläinmäärien ja Biomassa-atlaksen lannan ravinnesisältökerrointen<

library(tidyverse);library(varhandle)

#Data sisään. Eläinsuojalantaa ei levitetä pelloille, jätetään siksi ulos.

library(readxl)
Elainmaarat <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                                     sheet = "Eläimet_tiloittain_cleaned")

Ravinnekertoimet <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                                     sheet = "Ravinnekertoimet_cleaned", range = "A1:D9000")

Ravinnekertoimet<-filter(Ravinnekertoimet,complete.cases(Ravinnekertoimet))
Ravinnekertoimet$Eläinluokan_nro<-as.character(Ravinnekertoimet$Eläinluokan_nro)

Elaintyyppien_koodit <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                               sheet = "Eläintyyppien_koodit")
Elaintyyppien_koodit$Eläinluokan_nro<-as.character(Elaintyyppien_koodit$Eläinluokan_nro)

#Muutetaan eläintyyppien nimet niiden koodeiksi. 

colnames(Elainmaarat)[2:length(Elainmaarat)]<-seq(17,50, 1)

#Pitkä muoto eläinmääriin, kertoimen liittämistä varten
Elainmaarat<-pivot_longer(Elainmaarat, cols = 2:length(Elainmaarat), values_to = "Eläintä", names_to = "Eläinluokan_nro")

#Eläinluokkien nimet liitetään
nrow(inner_join(Elainmaarat, Elaintyyppien_koodit, by="Eläinluokan_nro"))
Elainmaarat<-inner_join(Elainmaarat, Elaintyyppien_koodit, by="Eläinluokan_nro")
Elainmaarat<-select(Elainmaarat,1,2,4,3)

#Liitetään ravinnekertoimet
nrow(inner_join(Elainmaarat, Ravinnekertoimet, by="Eläinluokan_nro"))
Elainmaarat<-inner_join(Elainmaarat, Ravinnekertoimet, by="Eläinluokan_nro")
Elainmaarat$Eläinluokka.y<-NULL

#Laskenta: eläinmäärä x kerroin

Elainmaarat<-Elainmaarat %>% mutate(Varastolannan_typpi_kg = Eläintä*LantaV_TN,
                       Varastolannan_fosfori_kg = Eläintä*LantaV_TP)

rm.all.but("Elainmaarat")


#Aggregointi tilatasolle

Ravinteet_tiloittain<- Elainmaarat %>% group_by(Tilatunnus) %>% summarise(Varastolannan_typpi_kg = sum(Varastolannan_typpi_kg),
                                                                          Varastolannan_fosfori_kg = sum(Varastolannan_fosfori_kg)) 

#Tilojen viljelyalat liitetään. Nämä laskettu aikaisemmin maankäytön GHG-päästökäsistä varten. 
#Tässä on mukana kaikki tilat, mukaanlukien kaikki sellaisetkin joilla eläimiä ei ole.

library(readxl);library(usefun)
Tilojen_viljelyala <- read_excel("Output/AreaAggregates/Tilojen_viljelyala.xlsx")
colnames(Tilojen_viljelyala)[1]<-c("Tilatunnus")
rm.all.but(c("Ravinteet_tiloittain","Tilojen_viljelyala"))

nrow(inner_join(Ravinteet_tiloittain, Tilojen_viljelyala, by="Tilatunnus"))

#Tsekataan puuttuuko tilakoodeja

Vaje<-unique(filter(Ravinteet_tiloittain, !(Tilatunnus %in% Tilojen_viljelyala$Tilatunnus)))

#Tätä n. 1200 tilakoodin joukkoa ei löydy myöskään peltolohkojen paikkatietodatasta. Ts. ei peltoviljelyä. 

Yhdistetty_data<-inner_join(Ravinteet_tiloittain, Tilojen_viljelyala, by="Tilatunnus")


#Liitetään tuotantosuunta

library(readxl)
Tuotantosuunnat <- read_excel("Data/Korjatut_tuotantosuunnat_20032024.xlsx")

nrow(inner_join(Yhdistetty_data, Tuotantosuunnat, by="Tilatunnus"))
Yhdistetty_data<-inner_join(Yhdistetty_data, Tuotantosuunnat, by="Tilatunnus")
colnames(Yhdistetty_data)[colnames(Yhdistetty_data) == "Tuotantosuunta_muutos"]<-"Tuotantosuunta"
colnames(Yhdistetty_data)[4:6]<-c("Eloperaista_ha","Mineraalia_ha","Yhteensa_ha")


nrow(inner_join(Vaje, Tuotantosuunnat, by="Tilatunnus"))
Vaje<-inner_join(Vaje, Tuotantosuunnat, by="Tilatunnus")

rm.all.but(c("Vaje","Yhdistetty_data"))
gc()


#Laskenta: lannan ravinnetta hehtaaria kohden. 

Yhdistetty_data<-Yhdistetty_data %>% mutate(TOTN_kg_ha = Varastolannan_typpi_kg/Yhteensa_ha,
                           TOTP_kg_ha = Varastolannan_fosfori_kg/Yhteensa_ha)


#Työkirjan luonti ja tallennus

Tuloskooste<-createWorkbook()
addWorksheet(Tuloskooste, "Lannan_ravinteet_tiloittain")
writeData(Tuloskooste, "Lannan_ravinteet_tiloittain", Yhdistetty_data)
addWorksheet(Tuloskooste, "Ei_peltolohkoissa")
writeData(Tuloskooste, "Ei_peltolohkoissa", Vaje)

saveWorkbook(Tuloskooste, here("Output/Ravinnedata/Lannan_ravinteet_tiloittain.xlsx"))
