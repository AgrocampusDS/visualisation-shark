
## mise en forme du jeu de donnée 

require(dplyr)

requin <- read.csv("attacks.csv") 
dim(requin) #  25723    24

req <- requin %>% 
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.") %>%
  filter(Year >= 1930) 
dim(req) # 5054    6

sum(is.na(req)) # 0 donc pas de na 

