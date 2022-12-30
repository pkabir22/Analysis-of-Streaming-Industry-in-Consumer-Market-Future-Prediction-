library(dplyr)
library(readxl)
library("xlsx")
popu <- read_xlsx("C:/Users/user/Downloads/Data Visulization Project/Global Population/Global Population.xlsx")
# popu <- read.csv("C:/Users/user/Downloads/Data Visulization Project/Global Population/Global Population.csv")

p <- popu %>% select(-one_of("Country code","Variant","Parent code","Notes"))
head(p)
colnames(p)
colnames(p)[2] <- "Region_Title"
colnames(p)[3] <- 'Region_Type'
colnames(p)[4] <- "Year"

list(unique(p$Region_Type)) 
list(unique(p$Region_Title))
unique(p$Year)

p_cleaned <- p %>% filter(Region_Type=="Country/Area") %>% filter(Year>=2017) %>% select(-"0-4")
p_cleaned[,c(5:24)] <- as.data.frame(apply(p_cleaned[c(5:24)],2,as.numeric))
p_cleaned$total_p <- rowSums(p_cleaned[c(5:24)])
p_c <- p_cleaned %>% select(-c(5:24)) %>% select(-Index)

tv_pe<- read_xlsx("C:/Users/user/Downloads/Data Visulization Project/R workbook/TV_Penetration.xlsx")
colnames(tv_pe)
colnames(tv_pe)[1] <- "Region_Title" 
tv_pene <- tv_pe %>% select(c(1:6))

n <- merge(p_c,tv_pene,by="Region_Title")
head(n)
n$user_count_2017 <- n[5]*n[4]
n$user_count_2018 <- n[6]*n[4]
n$user_count_2019 <- n[7]*n[4]
n$user_count_2020 <- n[8]*n[4]
n$user_count_2021 <- n[9]*n[4]

tv_re<- read_xlsx("C:/Users/user/Downloads/Data Visulization Project/R workbook/TV_Revenue.xlsx")
colnames(tv_re)[1] <- "Region_Title" 
tv_reve <- tv_re %>% select(c(1:6))
tv_reve[2] <- as.data.frame(apply(tv_reve[2],2,as.numeric))

tv_model <- merge(n,tv_reve,by="Region_Title")
tv_model$RPU_2017 <- (tv_model$`2017.y`*1000000)/tv_model$user_count_2017
tv_model$RPU_2018 <- (tv_model$`2018.y`*1000000)/tv_model$user_count_2018
tv_model$RPU_2019 <- (tv_model$`2019.y`*1000000)/tv_model$user_count_2019
tv_model$RPU_2020 <- (tv_model$`2020.y`*1000000)/tv_model$user_count_2020
tv_model$RPU_2021 <- (tv_model$`2021.y`*1000000)/tv_model$user_count_2021

tv_md <- tv_model[c(1:24)]

write.csv((tv_model),file = "b.csv",col.names = T)
head(tv_model)
View(tv_model)

################################
library(tidyr)

show <- read.csv("C:/Users/user/Downloads/Data Visulization Project/R workbook/tv_shows.csv")
head(show,10)
as.numeric(show$IMDb)
separate(show, col=IMDb, into=c('IMDb_Score', 'IMDb_Scale'), sep='/',na.rm=TRUE)
