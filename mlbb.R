#data is webscraped using SelectorGadget from the following sites:
# https://mobile-legends.fandom.com
# https://zathong.com/

#install.packages("rvest")
#install.packages("writexl")
#install.packages("readxl")
#install.packages("caret")
library(rvest)
library(writexl)
library(readxl)
library(caret)


#Data Prep for Spider Chart

url <- read_html( "https://mobile-legends.fandom.com/wiki/List_of_heroes")
hero <- url %>% html_nodes("td:nth-child(2)") %>% html_text()
role <- url %>% html_nodes("td:nth-child(4)") %>% html_text()
specialty <- url %>% html_nodes("td:nth-child(5)") %>% html_text()
laning <- url %>% html_nodes("td:nth-child(6)") %>% html_text()

#combine data 
df <- cbind(hero, role, specialty, laning)
df <- as.data.frame(df)
df$hero <- gsub("[\r\n]", "", df$hero)

stats <- read_excel("hero_stats.xlsx")
hero_stats <- merge(stats, df, by.x = "Stats", by.y = "hero", all = T)
hero_stats <- merge(stats, df, by.x = "Stats", by.y = "hero", all.x = T)
hero_stats$Mana <- NULL #removed as not all heroes have mana
hero_stats$`Magic Defense` <- NULL #removed as data is sparse

#min max normalisation
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

hero_stats[,c(2:6)] <- as.data.frame(lapply(hero_stats[,c(2:6)], minMax))
write_xlsx(hero_stats, "hero_stats_lane.xlsx" )



#Data for heroes strong and weak against

heroes = data.frame(df$hero)
heroes[30,] <- "yisunshin" 
heroes[37,] <- "lapulapu" 
heroes[61,] <- "change" 
heroes[83,] <- "x-borg"
heroes[94,] <- "popol"
heroes[95,] <- "yu-zhong" 
heroes[96,] <- "luoyi"

team = data.frame()
for (i in heroes[(1:117),]){
  url <- read_html(paste("https://zathong.com/", i, "-build-mobile-legends/", sep = ""))
  strong_against <- paste(url %>% html_nodes("#gallery-6 a") %>% html_text(), collapse = ', ')
  team <- rbind(team, strong_against)
  
}

team2 = data.frame()
for (i in heroes[(1:117),]){
  url <- read_html(paste("https://zathong.com/", i, "-build-mobile-legends/", sep = ""))
  weak_against <- paste(url %>% html_nodes("#gallery-7 a") %>% html_text(), collapse = ', ')
  team2 <- rbind(team2, weak_against)
  
}


team_df = cbind(heroes[(1:117),], team, team2)
colnames(team_df) = c("hero", "strong_against", "weak_against")
team_df$strong_against = gsub(", , ", ", ", team_df$strong_against)
team_df$strong_against = gsub("^, ", "", team_df$strong_against)
team_df$weak_against = gsub(", , ", ", ", team_df$weak_against)
team_df$weak_against = gsub("^, ", "", team_df$weak_against)

df[30,1] <- "yisunshin" 
df[37,1] <- "lapulapu" 
df[61,1] <- "change" 
df[83,1] <- "x-borg"
df[94,1] <- "popol"
df[95,1] <- "yu-zhong" 
df[96,1] <- "luoyi" 

team_df <- merge(team_df, df, by = 'hero', all.x = T)
write_xlsx(team_df, "team_strong_weak.xlsx")
