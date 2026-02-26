# Tworzenie tabeli ----
getwd()
kraje_1 = read.table("kraje_makro_1.csv", header = TRUE , sep="," , dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header = TRUE , sep=",", dec=".")
colnames(kraje_2)=c("X","Kod","Panstwo","Kontynent","Procent populacji mieszkajacej w miastach","Procent dostepnosci do internetu")
kraje_2$Kontynent = as.factor(kraje_2$Kontynent)
gsub("&","and","kraje_2$Kontynent")
kraje = merge(kraje_1,kraje_2,by.x="Kod",by.y="Kod")
kraje$Panstwo.y = NULL
kraje$X.y = NULL
kraje$X=NULL
kraje
#Funkcje dplyr ----
summary(kraje)
library(dplyr)
#install.packages("dplyr")
kraje = kraje%>%
  mutate(PKB_per_capita = PKB / Populacja)
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)
colnames(kraje)=c("Kod","Panstwo","PKB","Populacja","Przyrost_populacji","Kontynent","Procent_populacji_mieszkajacej_w_miastach","Procent_dostepnosci_do_internetu", "PKB_per_capita","Populacja_mln")
kraje %>%
  #select("Panstwo","PKB_per_capita","Procent_populacji_mieszkajacej_w_miastach","Procent_dostepnosci_do_internetu")
  arrange(desc(Przyrost_populacji))
kraje%>%
  filter(PKB>1e12) %>%
  arrange(desc(PKB)) %>%
  select(Panstwo,PKB,PKB_per_capita)
ponad_srednia_PKB = kraje %>%
  group_by(Kontynent) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))
ponad_srednia_PKB
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm=TRUE))
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))
kraje%>%
  summarise(
    sredna_populacja = mean(Populacja, na.rm=TRUE))
kraje %>%
  summarise(liczba_krajow=n())
kraje %>%
  group_by(Kontynent) %>%
  summarise(liczba_krajow = n())
kraje%>%
  group_by(Kontynent) %>%
  summarise(
    liczba_krajow = n(),
    mean_internetu = mean(Procent_dostepnosci_do_internetu, na.rm = TRUE),
    mean_urbanizacji = mean(Procent_dostepnosci_do_internetu, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_internetu))
