###
### Tworzenie prototypów wykresów
### 
### Obrazowanie miksu eergetyczengo Polski na przestrzeni lat
###

### Wczytuję biblioteki
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

### Wczytuję dane
kopalne <- read.csv2("Kopalne.csv",sep=";",fileEncoding = "Windows-1250")
oze <- read.csv2("OZE.csv",sep=";", fileEncoding = "Windows-1250")
kopalneOze <- read.csv("KopalneIOZE.csv",sep=";",fileEncoding = "Windows-1250")


View(oze)
View(kopalne)

View(kopalneOze)

str(kopalneOze)

###
### Eksploracja Danych
###

kopalneOze <- kopalneOze %>% mutate(Wartość = as.numeric(gsub(",",".",Wartość))) %>% 
  mutate(Wartość = ifelse(is.na(Wartość),0, Wartość))


###
### Do stworzenia kresek poziomów
###
kreski<-kopalneOze %>% select(Rok,Wartość,Source) %>% filter(Rok>2009, Source !="All", 
                      Source %in% c("fotowoltaika","woda","wiatr","biogaz i biomasa")) %>%
  select(Rok, Wartość) %>% group_by(Rok) %>% summarise(Suma=sum(Wartość))

###
### Do fct_relevel by energia była dobrze ułożona
###
poziomy=c("Pozostałe"="pozostałe","Gaz ziemny"="gaz_ziemny", "Węgiel brunatny"="węgiel_brunatny",
          "Węgiel_kamienny"="węgiel_kamienny","Woda"="woda","Biogaz i biomasa"="biogaz i biomasa",
          "Wiatr"="wiatr","Fotowoltaika"="fotowoltaika")

###
###Kod wykresu
###
kolumnowy<-kopalneOze %>% select(Rok, Wartość, Source) %>% filter(Source != "All") %>%
  filter(Rok>2009) %>% mutate(Source=fct_relevel(Source,poziomy)) %>% 
  ggplot(aes(x=Rok,y=Wartość,fill=Source)) + geom_col() +
  scale_fill_discrete(palette = c("orange","purple","brown","black",
                                  "blue","green","lightblue","yellow"),name="Źródło")+
labs(title="Rozkład produkcji energii elektrycznej w Polsce\nze względu na źródło w latach 2010 - 2023 w GWh.",
     x="Rok",y="Produkcja w GWh") +
  theme(plot.background = element_rect(fill="darkblue"),
        panel.background = element_rect(fill="darkblue"),
        plot.title = element_text(colour="white", hjust=0.5),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(colour ="white"),
        legend.background = element_rect(fill="darkblue"),
        legend.text = element_text(colour="white"),
        legend.ticks = element_line(colour = "black"),
        legend.title = element_text(colour = "white"),
        axis.title = element_text(colour = "white")) +
    geom_segment(data=kreski,aes(x=Rok-0.5, y=Suma, xend=Rok+0.5, yend=Suma),
                 color="white",linewidth = 0.8, inherit.aes = FALSE)

kolumnowy

###
### Wykrey waflowe - nie używane
###
rok <- 2005

kwadraty<-kopalneOze %>% filter(Rok==rok, Source!="All") %>% mutate(procent=round(Wartość/sum(Wartość)*100)) %>% 
  uncount(procent) %>% mutate(
    x = rep(1:10, length.out = n()),
    y= rep(1:10, each=10,length.out = n())
  )

kwadraty %>% ggplot(aes(x,y,fill=Source)) +geom_tile(color="black") +
  coord_equal() + scale_fill_brewer(palette = "Set3")+theme_void()+
  labs(title=paste("Rozkład produkcji energii elektrycznej względem\n źródeł w roku ",as.character(rok)))

###
### Śmieci
###

kopalneOze%>% select(Rok, Wartość, Source) %>% pivot_wider(names_from=Source, values_from = Wartość)






  