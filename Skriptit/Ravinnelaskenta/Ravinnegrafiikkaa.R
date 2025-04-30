library(tidyverse)

Data<-Graffaa %>% select(ETOL, Peltojen_suhteellinen_kuormittavuus) %>% mutate(ETOL = case_when(ETOL == "Hedelmien viljely" ~ "Fruits",
                                     ETOL == "Hevostilat" ~ "Horse",
                                     ETOL == "Hunajatuotanto" ~ "Honey",
                                     ETOL == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
                                     ETOL == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
                                     ETOL == "Lammas- ja vuohitilat" ~ "Sheep & goat",
                                     ETOL == "Maitotilat" ~ "Dairy",
                                     ETOL == "Mallasohran viljely" ~ "Malt barley",
                                     ETOL == "Marjojen viljely" ~ "Berries",
                                     ETOL == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
                                     ETOL =="Munatilat" ~ "Egg-laying poultry",
                                     ETOL =="Muut nautakarjatilat" ~ "Other cattle",
                                     ETOL == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
                                     ETOL == "Perunan viljely" ~ "Potato",
                                     ETOL == "Rehukasvien viljely" ~ "Fodder crops & pastures",
                                     ETOL == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
                                     ETOL == "Siipikarjatilat" ~ "Poultry",
                                     ETOL == "Sikatilat" ~ "Pig",
                                     ETOL == "Sokerijuurikkaan viljely" ~ "Sugar beet",
                                     ETOL == "Tarhaherneen viljely" ~ "Garden pea",
                                     ETOL == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
                                     ETOL == "Turkistarhaus" ~ "Fur",
                                     ETOL == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
                                     ETOL == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
                                     .default = ETOL)
                                     ) %>% arrange(desc(Peltojen_suhteellinen_kuormittavuus))

Eläimet<- c("Horse", "Sheep & goat", "Dairy","Egg-laying poultry","Other cattle","Poultry","Pig ","Fur") 




library(viridisLite)

Data %>% filter(ETOL %in% Eläimet) %>%  ggplot(aes(x= ETOL, y = Peltojen_suhteellinen_kuormittavuus, fill=ETOL)) +
  scale_fill_viridis_d(alpha=0.9, begin=0.1, end=0.8)+
  geom_bar(stat="identity")+
  ylab("relative P loading, %")+
  xlab("Farm type")+
  geom_hline(yintercept = 100, size=1.5)+
  geom_text(aes(label = paste0(round(Peltojen_suhteellinen_kuormittavuus, 0),"","%")), vjust = 1.5, colour = "whitesmoke")
  


Data %>% filter(!(ETOL %in% Eläimet)) %>%  ggplot(aes(x= ETOL, y = Peltojen_suhteellinen_kuormittavuus, fill=ETOL)) +
  scale_fill_viridis_d(alpha=0.9, begin=0.1, end=0.8)+
  geom_bar(stat="identity")+
  ylab("relative P loading, %")+
  xlab("Farm type")+
  geom_hline(yintercept = 100, size=1.5)+
  geom_text(aes(label = paste0(round(Peltojen_suhteellinen_kuormittavuus, 0),"","%")), vjust = 1.5, colour = "whitesmoke")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

        