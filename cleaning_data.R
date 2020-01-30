setwd("~/Documents/github/coralreef/")

library(dplyr)
library(tidyr)

data <- read.csv("coralreef_gender.csv",header=TRUE, stringsAsFactors = FALSE)
data <- read.csv("coralreef_gender_nature_science_correct.csv", header=TRUE, stringsAsFactors = FALSE)

#Split authors and assign author number order
s <- strsplit(data$AU, split="; ")
df <- data.frame(ID=rep(data$ID, sapply(s, length)), AU = unlist(s))
df <- df %>% group_by(ID) %>% mutate(AO=row_number())
colnames(df) <- c("ID","AU","AO")
df$ID <- as.integer(df$ID)
df$AU <- as.character(df$AU)

d <- strsplit(data$AF, split="; [",fixed=TRUE)
df2 <- data.frame(ID=rep(data$ID, sapply(d, length)), AF=unlist(d))
df3 <- df2 %>% separate(AF, c("AU","AF"), sep="\\] ")
f <- strsplit(df3$AU, split="; ")
df4 <- data.frame(ID=rep(df3$ID, sapply(f,length)), AF=rep(df3$AF, sapply(f, length)), AU=unlist(f))
df4[] <- lapply(df4, gsub, pattern="[", replacement="",fixed=TRUE)
df4$ID <- as.integer(df4$ID)
df4 <- df4 %>% group_by(ID)

df5 <- left_join(df,df4,by=c("AU","ID"))

data2 <- data %>% select(-AU,-AF)
data2 <- left_join(data2,df5,by="ID")

df_final <- data2 %>% separate(AF, c("IN","CO"),sep=",\\s*(?=[^,]+$)", perl=TRUE)

write.csv(df_final, "coralreef_gender_cleaned_nature_science.csv")

##fixing author affiliation issue with AF formatting issue
check <- df3 %>% filter(is.na(AF))
check <- check %>% select(ID) %>% distinct()
data_check <- left_join(check,data,by=c("ID")) 
data <- data_check
#Split authors and assign author number order
s <- strsplit(data$AU, split="; ")
df <- data.frame(ID=rep(data$ID, sapply(s, length)), AU = unlist(s))
df <- df %>% group_by(ID) %>% mutate(AO=row_number())
colnames(df) <- c("ID","AU","AO")
df$ID <- as.integer(df$ID)
df$AU <- as.character(df$AU)

d <- strsplit(data$AF, split="; ",fixed=TRUE)
df2 <- data.frame(ID=rep(data$ID, sapply(d, length)), AF=unlist(d))
df3 <- df2 %>% separate(AF, c("AU","AF"), sep="\\] ")
f <- strsplit(df3$AU, split="; ")
df4 <- data.frame(ID=rep(df2$ID, sapply(f,length)), AF=rep(df3$AF, sapply(f, length)), AU=unlist(f))
df4[] <- lapply(df4, gsub, pattern="[", replacement="",fixed=TRUE)
df4$ID <- as.integer(df4$ID)
df4 <- df4 %>% group_by(ID)

df5 <- left_join(df,df4,by=c("AU","ID"))

data2 <- data %>% select(-AU,-AF)
data2 <- left_join(data2,df5,by="ID")

df_final <- data2 %>% separate(AF, c("IN","CO"),sep=",\\s*(?=[^,]+$)", perl=TRUE)
df_mod_final <- left_join(dd,data_mod,by=c("ID","AU"))

write.csv(df_mod_final,"coralreef_gender_cleaned_1481-1674.csv")
