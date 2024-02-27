library(readr)
library(tidyverse)
library(scales)
library(ggrepel)


format(Sys.time(), "%Y-%m-%d %X")


dp <- read_csv("Dominancia_Prestigio.csv") |> 
  rowwise() |> 
  mutate(Dominancia = sum(i03, i05, i07, i09, (8-i10), i11, (8-i12), i16)/8,
         Prestigio = sum(i01, (8-i02), i04, (8-i06), i08, i13, i14, i15, (8-i17))/9)

dpLong <- dp |> 
  select(num, Dominancia, Prestigio) |> 
  pivot_longer(cols = Dominancia:Prestigio, names_to = "Variable", values_to = "Puntaje") 

dpLongMax <- dpLong |> 
  group_by(Variable) |> 
  mutate(Percentil = ntile(Puntaje, 100)) |> 
  ungroup() |> 
  filter(num == max(num, na.rm=TRUE))

ggplot(dpLong, aes(y = Puntaje, x = Variable)) +
  geom_violin(aes(fill = Variable, color = Variable), alpha = 0.1) +
  geom_boxplot(aes(color = Variable), width = 0.1) +
  geom_jitter(alpha = 0.1, width = 0.1) +
  geom_point(data = dpLongMax, aes(color = Variable), size = 5, color = "black") +
  geom_label_repel(data = dpLongMax, 
                   aes(label = paste0("Tú: ", Percentil, "%"), 
                       fill = Variable, color = Variable), 
                  color = "black",
                  #arrow = arrow(type = "closed", ends = "first"),
                  point.padding = NA,
                  box.padding = 0.5) +
  ylim(c(1,7)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "") +
  theme(legend.position = "none") 
  
