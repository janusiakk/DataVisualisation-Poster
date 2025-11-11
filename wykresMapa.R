###
### Tworzenie  wykresów
### 
### Mapka województw ze wskaźnikiem gradientu
###

library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

wojewodztwa<- st_read("PRG_jednostki_administracyjne_2024\\A01_Granice_wojewodztw.shp",quiet=TRUE)
View(wojewodztwa)
dane <- read.csv("Energia_elektryczna_wg_woj_2015-2023.csv", sep=";",
                 fileEncoding = "Windows-1250")
ludzie <- read.csv("Ludność_według_płci_i_województw_-_stan_w_dniu_31.12.2023_roku.csv",
                   sep=";",fileEncoding = "Windows-1250")
ludzie <- ludzie %>% select(Województwo, Ogółem) %>% mutate(Województwo=tolower(Województwo))

filtr <- c("Produkcja energii elektrycznej: OZE (w tym elektrownie wodne z członami pompowymi; łącznie ze współspalaniem) (GWh)",
           "Produkcja energii elektrycznej: ogółem (GWh)")

View(ludzie)

###
### Wskaźnik = oze*1000/liczba ludzi
### Docenić województwa, które produkują dużo, ale tak ogólnie
### Tutaj dodaąc ile człowiek zużywa GWH
### Człowiek ok 1600 kWh (sprawdzić) czyli 1000 osób 1600MWh = 1,6 GWh
### Oczyiwście to energia w gospodarstwie domowym, a nie ogólnie potrzebna
### no i jest to sama energia elektryczna
###

df<-dane %>% mutate(Zmienna=case_when(
  Zmienna==filtr[1]~"OZE",
  Zmienna==filtr[2]~"Calosc",
  .default =""), Województwo=tolower(Województwo)) %>% 
  filter(Rok==2023,Zmienna %in% c("OZE","Calosc"),Województwo!="ogółem  kraj") %>%
  select(!Rok) %>% pivot_wider(values_from = Wartość, names_from = Zmienna) %>% 
  left_join(ludzie,by="Województwo") %>% 
  mutate(OZE=as.numeric(gsub(",",".",OZE)), Calosc=as.numeric(gsub(",",".",Calosc))) %>% 
  mutate(wsk=OZE*1000/Ogółem)


wojewodztwa %>% left_join(df,by=c("JPT_NAZWA_"="Województwo")) %>% ggplot() +
  geom_sf(aes(fill=wsk),color="black",name="GWh z OZE na 1000 mieszkańców") +
  scale_fill_gradient2(low="red",high="green", mid="yellow",midpoint=1.6,
                       name="MWh/m") +
  labs(title = "Energia w GWh z OZE przypadająca na\n1000 mieszkańców względem województw.")+
  theme(plot.background = element_rect(fill="darkblue"),
        panel.background = element_rect(fill="darkblue"),
        plot.title = element_text(colour="white", hjust=0.5),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill="darkblue"),
        legend.text = element_text(colour="white"),
        legend.ticks = element_line(colour = "black"),
        legend.title = element_text(colour = "white"))
