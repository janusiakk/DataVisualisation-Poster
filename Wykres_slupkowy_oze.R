oze <- data.frame(
  Rok = c(2023,2020,2017,2014,2011),
  Solar = c(11107.09, 1957.92, 165.46, 6.89, 0.18),
  Wind = c(24176.36, 15800.05, 14909.04, 7675.63, 3204.55),
  Bio = c(6374.22, 6932.76, 5308.56, 9161, 7149), 
  Hydro= c(2409.51, 2118.34, 2559.58, 2182.45, 2331.38 )
)

library(ggplot2)
library(tidyr)


df_long <- pivot_longer(oze, cols = c(Solar, Wind, Bio, Hydro), 
                        names_to = "Kategoria", 
                        values_to = "Wartość")
df_long$Rok <- factor(df_long$Rok)

ggplot(df_long, aes(x = Rok, y = Wartość, fill = Kategoria)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    name = "Źródło energii",
    labels = c("Biopaliwa stałe", "Energia Wodna", "Energia słoneczna","Energia wiatrowa"),
    values = c("green","blue",   "yellow", "lightblue")) +
  labs(title = "Produkcja energii z oze",
       x = "Rok", y = "Ilość energii w GWh") +

  
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "darkblue", color = NA),
    plot.background = element_rect(fill = "darkblue", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(color = "white", size = 16, face = "bold"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")

  )
