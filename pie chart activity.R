
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(cowplot)


###### mise en forme du jeu de donnée pour avoir les groupes d'activités 

requin <- read.csv("attacks.csv") 

## choisir les colonnes d'intérêt
req <- requin %>%
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.") %>%
  filter(Year >= 1930)


##############################
# netoyage du jeu de donnée
#############################


req$Country <- tolower(req$Country)

req_pie <- req %>%
  filter(Country %in% c("usa", "australia", "south africa"))
req_pie[which(req_pie$Fatal..Y.N. !="Y"),]$Fatal..Y.N.  <- "Autres"
req_pie[which(req_pie$Type == "Questionable"),]$Type <- "Invalid"

# netoyage du jeu de donnée

# diving 
for (i in (1:nrow(req_pie))) {
  if (grepl("diving", req_pie$Activity[i]) | (grepl("Diving", req_pie$Activity[i])) |
      (grepl("Dived", req_pie$Activity[i]))) {
    req_pie$Activity[i] <- "Diving"
  } 
}

# fishing
for (i in (1:nrow(req_pie))) {
  if (grepl("fishing", req_pie$Activity[i]) | (grepl("Fishing", req_pie$Activity[i])) |
      (grepl("lobsters", req_pie$Activity[i])) |
      grepl("fish", req_pie$Activity[i]) |
      grepl("bichiques", req_pie$Activity[i]) |
      grepl("Casting", req_pie$Activity[i]) |
      grepl("sardines", req_pie$Activity[i]) |
      grepl("Crabbing", req_pie$Activity[i]) |
      grepl("Clamming", req_pie$Activity[i]) |
      grepl("turtle", req_pie$Activity[i]) |
      grepl("prawns", req_pie$Activity[i])|
      grepl("net", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "fishing"
  } 
}

# air or sea disaster
for (i in (1:nrow(req_pie))) {
  if (grepl("disaster", req_pie$Activity[i]) | grepl("Disaster", req_pie$Activity[i])| 
      (grepl("air", req_pie$Activity[i])) | (grepl("plane", req_pie$Activity[i]))
      | (grepl("757", req_pie$Activity[i]))|
      grepl("hurricane", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "Air/Sea disaster"
  } 
}

# boat/ship accident de bateau 
for (i in (1:nrow(req_pie))) {
  if (grepl("Boat", req_pie$Activity[i]) | (grepl("boat", req_pie$Activity[i]))
      | (grepl("ship", req_pie$Activity[i])) | (grepl("Fell", req_pie$Activity[i]))
      | (grepl("Vessel", req_pie$Activity[i])) |
      grepl("sank", req_pie$Activity[i])|
      grepl("promotional film ", req_pie$Activity[i]) |
      grepl("Ferry", req_pie$Activity[i])|
      grepl("ferry", req_pie$Activity[i])|
      grepl("capsiz", req_pie$Activity[i]) |
      grepl("MV Dona Marilyn", req_pie$Activity[i]) |
      grepl("schooner Elizabeth", req_pie$Activity[i]) |
      grepl("Towing", req_pie$Activity[i]) |
      grepl("Tzenny Chandris", req_pie$Activity[i]) |
      grepl("Wreck", req_pie$Activity[i]) |
      grepl("wreck", req_pie$Activity[i]) |
      grepl("dinghy", req_pie$Activity[i]) |
      grepl("yacht", req_pie$Activity[i]) |
      grepl("Chumming", req_pie$Activity[i]) |
      grepl("raft", req_pie$Activity[i]) |
      grepl("Tagging", req_pie$Activity[i]) |
      grepl("overboard", req_pie$Activity[i]) |
      grepl("free the shark", req_pie$Activity[i]) |
      grepl("rescue a shark", req_pie$Activity[i]) 
  ) {
    req_pie$Activity[i] <- "boat/ship accident"
  } 
}

# sport 
for (i in (1:nrow(req_pie))) {
  if (grepl("surf", req_pie$Activity[i]) | grepl("Surf", req_pie$Activity[i]) |(grepl("boarding", req_pie$Activity[i]))|
      grepl("Boarding", req_pie$Activity[i]) |
      (grepl("batching", req_pie$Activity[i]))| (grepl("Bathing", req_pie$Activity[i])) |
      (grepl("bathing", req_pie$Activity[i])) |
      (grepl("Swim", req_pie$Activity[i]))| (grepl("swim", req_pie$Activity[i])) |
      (grepl("fell", req_pie$Activity[i])) | (grepl("Paddling", req_pie$Activity[i]))| 
      (grepl("Paddling", req_pie$Activity[i])) | (grepl("water", req_pie$Activity[i]))
      | (grepl("Snorkeling", req_pie$Activity[i]))
      | (grepl("Sitting", req_pie$Activity[i])) | 
      grepl("Happy Jack", req_pie$Activity[i]) |
      grepl("Bending", req_pie$Activity[i]) |
      grepl("Canoe", req_pie$Activity[i]) |
      grepl("paddling", req_pie$Activity[i]) |
      grepl("Kaya", req_pie$Activity[i]) |
      grepl("Lifesaving", req_pie$Activity[i]) |
      grepl("Snorkeling", req_pie$Activity[i]) |
      grepl("Stamding", req_pie$Activity[i])|
      grepl("Wading", req_pie$Activity[i]) |
      grepl("Wash", req_pie$Activity[i]) |
      grepl("Workong", req_pie$Activity[i])| 
      grepl("McIver", req_pie$Activity[i]) |
      grepl("Standing", req_pie$Activity[i]) |
      grepl("Crossing the bay", req_pie$Activity[i])|
      grepl("Walking", req_pie$Activity[i]) |
      grepl("kill a shark", req_pie$Activity[i]) |
      grepl("Removing shark", req_pie$Activity[i]) |
      grepl("Riding", req_pie$Activity[i]) |
      grepl("Board sailing", req_pie$Activity[i])|
      grepl("Exercising", req_pie$Activity[i]) |
      grepl("Feeding sharks", req_pie$Activity[i]) |
      grepl("Filming", req_pie$Activity[i]) |
      grepl("ski", req_pie$Activity[i]) |
      grepl("Jumping", req_pie$Activity[i]) |
      grepl("Paddle", req_pie$Activity[i]) |
      grepl("Play", req_pie$Activity[i]) |
      grepl("Resting", req_pie$Activity[i])  |
      grepl("floatation device", req_pie$Activity[i])|
      grepl("Sculling", req_pie$Activity[i]) |
      grepl("Splashing", req_pie$Activity[i]) |
      grepl("Dr. Marais", req_pie$Activity[i]) |
      grepl("Wrangling", req_pie$Activity[i]) |
      grepl("Photographing", req_pie$Activity[i]) |
      grepl("SUP", req_pie$Activity[i]) |
      grepl("Kakay", req_pie$Activity[i]) 
      
  ) {
    req_pie$Activity[i] <- "sport/ activity" }
}



# adrift
for (i in (1:nrow(req_pie))) {
  if (grepl("Adrift", req_pie$Activity[i]) | grepl("adrift", req_pie$Activity[i]) |
      grepl("Floating", req_pie$Activity[i])|
      grepl("3 men & 2 boys", req_pie$Activity[i]) |
      grepl("float", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "Adrift"
  } 
}

# murder /  war 
for (i in (1:nrow(req_pie))) {
  if (grepl("Murder", req_pie$Activity[i]) ||
      grepl("submarine", req_pie$Activity[i]) |
      grepl("American", req_pie$Activity[i]) |
      grepl("torpedoed", req_pie$Activity[i]) |
      grepl("Torpedoed", req_pie$Activity[i]) |
      grepl("soldiers", req_pie$Activity[i]) |
      grepl("Escaping", req_pie$Activity[i]) |
      grepl("Japanese", req_pie$Activity[i]) |
      grepl("Japan", req_pie$Activity[i]) |
      grepl("Sinking", req_pie$Activity[i]) |
      grepl("SS", req_pie$Activity[i]) |
      grepl("foundered", req_pie$Activity[i])  |
      grepl("US", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "murder /  war "
  } 
}


##############################
# 1e graphe: répartition des types d'attaques
#############################

# on se concentre sur les 3 pays les plus touchés
#pie1
req_pie1 <- req_pie %>%
  group_by(Type) %>%
  summarise(Effectif=n(), Pourcentage = Effectif/3480, 
            ypos = (cumsum(Pourcentage) - 0.5*Pourcentage)) 
req_pie1 <- req_pie1[-1,]

req_pie1$Type
df1 <- req_pie1 %>% 
  mutate(csum = rev(cumsum(rev(Pourcentage))), 
         pos = Pourcentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Pourcentage/2, pos))

# pie 1 Répartion des Types d'attaques
pie <- ggplot(data = req_pie1, aes(x="", y=Pourcentage, fill= Type)) +
  geom_bar(stat="identity", width=1, color= "white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values = c("Boat" = "#DAF7A6",
                               "Invalid" = "#E5E4E2",
                               "Provoked" = "#FFA500",
                               "Sea Disaster" = "#E97451",     
                               "Unprovoked" = "#6495ED")[]) +
  geom_label_repel(data = df1,
                   aes(y = pos, label = paste0(round(Pourcentage*100,1), "%")),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Type")) +
  labs(title = "Répartition des Types d'attaques") +
  theme(plot.title = element_text(size=9),
        legend.text = element_text(size = 5))
# geom_text(aes(label = Type),
#           position = position_stack(vjust = 0.5), color="white")
pie


##############################
# 2e graphe: conséquences des attaques
#############################

req_pie2 <- req_pie %>%
  filter(Type =="Unprovoked", Activity!="") %>%
  group_by(Activity) %>%
  summarise(Effectif=n(), Pourcentage = Effectif/2448)

df2 <- req_pie2 %>% 
  mutate(csum = rev(cumsum(rev(Pourcentage))), 
         pos = Pourcentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Pourcentage/2, pos))

req_pie2$Activity

pie2 <- ggplot(data = req_pie2, aes(x="", y=Pourcentage, fill= Activity)) +
  geom_bar(stat="identity", width=1, color= "white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values = c("Adrift" = "#B6D8F2",
                               "Air/Sea disaster" = "#5784BA",
                               "boat/ship accident" = "#7C4C53",
                               "Diving" = "#00A0B0",
                               "fishing" = "#2a9d8f",
                               "murder /  war " = "#BD3100",
                               "sport/ activity" = "#F5CB5C")) +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(round(Pourcentage*100,1), "%")),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Activity")) +
  labs(title = "Activités des victimes lors de l'attaque") +
  theme(plot.title = element_text(size=9),
        legend.text = element_text(size = 5))



pie2


##############################
# 3z graphe: activité des victimes en cours
#############################


req_pie3 <- req_pie %>%
  filter(Type =="Unprovoked", Activity!="") %>%
  group_by(Fatal..Y.N.) %>%
  summarise(Effectif=n(), Pourcentage = Effectif/2444, 
            ypos = (cumsum(Pourcentage) - 0.5*Pourcentage)) 


req_pie3$Fatal..Y.N.[which(req_pie3$Fatal..Y.N. =="Y")] <- "Mortel"
req_pie3$Fatal..Y.N.[which(req_pie3$Fatal..Y.N. =="Autres")] <- "Non Mortel"

df3 <- req_pie3 %>% 
  mutate(csum = rev(cumsum(rev(Pourcentage))), 
         pos = Pourcentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Pourcentage/2, pos))
df3$pos[2] <- 0.95
pie3 <- ggplot(data = req_pie3, aes(x="", y=Pourcentage, fill= Fatal..Y.N.)) +
  geom_bar(stat="identity", width=1, color= "white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_manual(values = c("Mortel" = "#FC4E00",
                               "Non Mortel" = "#D4D3DC")) +
  geom_label_repel(data = df3,
                   aes(y = pos, label = paste0(round(Pourcentage*100,1), "%")),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Type")) +
  labs(title = "Intensité de dégâts des attaques 'unprovoked'") +
  theme(plot.title = element_text(size=9),
        legend.text = element_text(size = 5))
pie3



##############################
# plot des 3 en meme temps
#############################

plot_grid(pie, pie3,"",pie2, ncol = 2, nrow = 2)
# 
# 
# Row
# -------------------------------------
# 
#   ### Description des attaques les plus récurrentes
# 
#   ```{r, include=FALSE}
# plot_grid(pie)
# ```
# 
# Row {.tabset .tabset-fade}
# -------------------------------------
# 
#   ### Activities
# 
#   ```{r, include=FALSE}
# plot_grid(pie2)
# ```
# 
# ### Intensité
# 
# 
# ```{r, include=FALSE}
# plot_grid(pie3)
# ```






