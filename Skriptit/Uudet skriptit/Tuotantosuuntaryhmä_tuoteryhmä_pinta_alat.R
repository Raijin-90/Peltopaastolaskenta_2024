
TS <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", sheet = "Tuotantosuunnat ryhmittäin")
colnames(TS)[1]<-"Tuotantosuunta"
PROD <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", sheet = "Kasvit tuotertyhmittäin")


#Raivaamaton
#Crop/mineral
Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% 
  pivot_longer(cols=6:length(Cropland_korotettu_mineraalimaa), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% merge(TS, by="Tuotantosuunta")

Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivaamaton_mineraalimaa_cropland = sum(hehtaaria))



#Grass/mineral

Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa %>% 
  pivot_longer(cols=6:length(Grassland_korotettu_mineraalimaa), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa %>% merge(TS, by="Tuotantosuunta")

Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivaamaton_mineraalimaa_grassland = sum(hehtaaria))



#Cropland/elop

Cropland_korotettu_elop<-Cropland_korotettu_elop %>% 
  pivot_longer(cols=6:length(Cropland_korotettu_elop), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Cropland_korotettu_elop<-Cropland_korotettu_elop %>% merge(TS, by="Tuotantosuunta")

Cropland_korotettu_elop<-Cropland_korotettu_elop %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivaamaton_elop_cropland = sum(hehtaaria))


#Grassland/elod

Grassland_korotettu_elop<-Grassland_korotettu_elop %>% 
  pivot_longer(cols=6:length(Grassland_korotettu_elop), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Grassland_korotettu_elop<-Grassland_korotettu_elop %>% merge(TS, by="Tuotantosuunta")

Grassland_korotettu_elop<-Grassland_korotettu_elop %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivaamaton_elop_grassland = sum(hehtaaria))




#RAIVIOT

#Cropland mineral


Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% 
  pivot_longer(cols=6:length(Cropland_korotettu_mineraalimaa_raivio), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% merge(TS, by="Tuotantosuunta")

Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivattu_mineraalimaa_cropland = sum(hehtaaria))


#Grassland mineral

Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% 
  pivot_longer(cols=6:length(Grassland_korotettu_mineraalimaa_raivio), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% merge(TS, by="Tuotantosuunta")

Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivattu_mineraalimaa_grassland = sum(hehtaaria))


#Cropland elop


Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% 
  pivot_longer(cols=6:length(Cropland_korotettu_elop_raivio), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% merge(TS, by="Tuotantosuunta")

Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivattu_elop_cropland = sum(hehtaaria))



#Grassland elop


Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% 
  pivot_longer(cols=6:length(Grassland_korotettu_elop_raivio), names_to = "Tuotantosuunta", values_to = "hehtaaria") %>%
  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                    Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                    Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                    Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                    Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                    Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                    Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                    Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                    .default = Tuotantosuunta))
Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% merge(TS, by="Tuotantosuunta")

Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% group_by(ETOL, Tuoteryhmä) %>% summarise(Raivattu_elop_grassland = sum(hehtaaria))




#YHDISTÄMINEN
#VILJELTY
a<-Cropland_korotettu_mineraalimaa
b<-Cropland_korotettu_elop

sum(a$Raivaamaton_mineraalimaa_cropland)+sum(b$Raivaamaton_elop_cropland)


Crop<-merge(a,b, by=c("ETOL", "Tuoteryhmä"), all=T)
Crop[is.na(Crop)]<-0
colnames(Crop)[3:4]<-c("Raivaamaton_mineral", "Raivaamaton_elop")

sum(Crop$Raivaamaton_mineral)+sum(Crop$Raivaamaton_elop)


a<-Grassland_korotettu_mineraalimaa
b<-Grassland_korotettu_elop

sum(a$Raivaamaton_mineraalimaa_grassland)+sum(b$Raivaamaton_elop_grassland)

Grass<-merge(a,b, by=c("ETOL", "Tuoteryhmä"), all=T)
colnames(Grass)[3:4]<-c("Raivaamaton_mineral", "Raivaamaton_elop")
Grass[is.na(Grass)]<-0


sum(Grass$Raivaamaton_mineral)+sum(Grass$Raivaamaton_elop)


#rbind, koska Grassin kaikki tuotteet ovat sellaisia joita Crop ei lainkaan sisällä. Tuotantosuunnat ovat samat

sum(Grass$Raivaamaton_mineral)+sum(Grass$Raivaamaton_elop)+sum(Crop$Raivaamaton_mineral)+sum(Crop$Raivaamaton_elop)

Raivaamaton<-rbind(Crop, Grass)

sum(Raivaamaton$Raivaamaton_mineral)+sum(Raivaamaton$Raivaamaton_elop)





#RAIVATTU

a<-Cropland_korotettu_mineraalimaa_raivio
b<-Cropland_korotettu_elop_raivio

sum(a$Raivattu_mineraalimaa_cropland)+sum(b$Raivattu_elop_cropland)


Crop<-merge(a,b, by=c("ETOL", "Tuoteryhmä"), all=T)
colnames(Crop)[3:4]<-c("Raivattu_mineral", "Raivattu_elop")
Crop[is.na(Crop)]<-0
sum(Crop$Raivattu_mineral)+sum(Crop$Raivattu_elop)


a<-Grassland_korotettu_mineraalimaa_raivio
b<-Grassland_korotettu_elop_raivio

sum(a$Raivattu_mineraalimaa_grassland)+sum(b$Raivattu_elop_grassland)

Grass<-merge(a,b, by=c("ETOL", "Tuoteryhmä"), all=T)
colnames(Grass)[3:4]<-c("Raivattu_mineral", "Raivattu_elop")
Grass[is.na(Grass)]<-0
sum(Grass$Raivattu_elop)+sum(Grass$Raivattu_mineral)


#rbind, koska Grassin kaikki tuotteet ovat sellaisia joita Crop ei lainkaan sisällä. Tuotantosuunnat ovat samat

sum(Grass$Raivattu_mineral)+sum(Grass$Raivattu_elop)+sum(Crop$Raivattu_mineral)+sum(Crop$Raivattu_elop)

Raivattu<-rbind(Crop, Grass)

sum(Raivattu$Raivattu_mineral)+sum(Raivattu$Raivattu_elop)

sum(Raivaamaton$Raivaamaton_mineral)+sum(Raivaamaton$Raivaamaton_elop)



sum(Raivattu$Raivattu_mineral)+sum(Raivattu$Raivattu_elop)+
sum(Raivaamaton$Raivaamaton_mineral)+sum(Raivaamaton$Raivaamaton_elop)


library(openxlsx)
Data<-createWorkbook()
addWorksheet(Data, "Raivattu")
writeData(Data, "Raivattu", Raivattu)
addWorksheet(Data, "Ei_Raivattu")
writeData(Data, "Ei_Raivattu", Raivaamaton)

saveWorkbook(Data,here("Output/Yksinkertaistettu_intensiteettilaskenta/Viljav_alat_tuote_tuotantosuunta_raivaus_raivaamaton.xlsx"),overwrite = T)
