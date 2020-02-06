library(tidyverse)
library(plyr)
library(ggplot2)
library(readr)

name <- "PBS-DLS 50:50"

# two diffrent files options

# file <- "5050 1"
# PVA <- "PVA 72k"

file <- "5050"
PVA <- "PVA 13k"

# 100 RPM 55

mydir1 = paste0("data/",PVA,"/100 ",file,"")
myfiles1 = list.files(path=mydir1, pattern="*.csv", full.names=TRUE)
myfiles1

dat_csv1 = ldply(myfiles1, read_csv)
dat_csv1


dc100 <- dat_csv1 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 100) %>% 
  select(d, RPM)

write.csv(dc100,"Alles 5050\\dc100.csv", row.names = FALSE)

# 200 RPM 55

mydir2 = paste0("data/",PVA,"/200 ",file,"")
myfiles2 = list.files(path=mydir2, pattern="*.csv", full.names=TRUE)
myfiles2

dat_csv2 = ldply(myfiles2, read_csv)
dat_csv2


dc200 <- dat_csv2 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 200) %>% 
  select(d, RPM)


write.csv(dc200,"Alles 5050\\dc200.csv", row.names = FALSE)

# 300 RPM 55

mydir3 = paste0("data/",PVA,"/300 ",file,"")
myfiles3 = list.files(path=mydir3, pattern="*.csv", full.names=TRUE)
myfiles3

dat_csv3 = ldply(myfiles3, read_csv)
dat_csv3


dc300 <- dat_csv3 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 300) %>% 
  select(d, RPM)


write.csv(dc300,"Alles 5050\\dc300.csv", row.names = FALSE)

# 400 RPM 55

mydir4 = paste0("data/",PVA,"/400 ",file,"")
myfiles4 = list.files(path=mydir4, pattern="*.csv", full.names=TRUE)
myfiles4

dat_csv4 = ldply(myfiles4, read_csv)
dat_csv4


dc400 <- dat_csv4 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 400) %>% 
  select(d, RPM)


write.csv(dc400,"data/Alles 5050\\dc400.csv", row.names = FALSE)

# 100 RPM 73

mydir5 = "100 7030 1"
myfiles5 = list.files(path=mydir5, pattern="*.csv", full.names=TRUE)
myfiles5

dat_csv5 = ldply(myfiles5, read_csv)
dat_csv5


dc100 <- dat_csv5 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 100) %>% 
  select(d, RPM)


write.csv(dc100,"Alles 7030\\dc100.csv", row.names = FALSE)

# 200 RPM 73

mydir6 = "200 7030 1"
myfiles6 = list.files(path=mydir6, pattern="*.csv", full.names=TRUE)
myfiles6

dat_csv6 = ldply(myfiles6, read_csv)
dat_csv6


dc200 <- dat_csv6 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 200) %>% 
  select(d, RPM)

write.csv(dc200,"Alles 7030\\dc200.csv", row.names = FALSE)

# 300 RPM 73

mydir7 = "300 7030 1"
myfiles7 = list.files(path=mydir7, pattern="*.csv", full.names=TRUE)
myfiles7

dat_csv7 = ldply(myfiles7, read_csv)
dat_csv7


dc300 <- dat_csv7 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 300) %>% 
  select(d, RPM)

write.csv(dc300,"Alles 7030\\dc300.csv", row.names = FALSE)

# 400 RPM 73

mydir8 = "400 7030 1"
myfiles8 = list.files(path=mydir8, pattern="*.csv", full.names=TRUE)
myfiles8

dat_csv8 = ldply(myfiles8, read_csv)
dat_csv8


dc400 <- dat_csv8 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(d = sqrt(Rq)*2) %>% 
  filter(d >=1) %>% 
  mutate(d, RPM = 400) %>% 
  select(d, RPM)

write.csv(dc400,"Alles 7030\\dc400.csv", row.names = FALSE)

# Dla wszytkich pomiarów PBS DLS 50 50

mydir9 = "Alles 5050"
myfiles9 = list.files(path=mydir9, pattern="*.csv", full.names=TRUE)
myfiles9

alles_5050 = ldply(myfiles9, read_csv)
alles_5050

# Dla wszytskich pomiraów PBS DLS 70 30

mydir10 = "Alles 7030"
myfiles10 = list.files(path=mydir10, pattern="*.csv", full.names=TRUE)
myfiles10

alles_7030 = ldply(myfiles10, read_csv)
alles_7030


q <- ggplot(data = alles_5050, aes(x=as.character(RPM), y=d, fill=RPM)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "area")+
  xlab("RPM")+
  ylab("Capsule size [µm]")+
  theme(legend.position = "none")+
  ggtitle("Capsule density depending on the mixing speed for PBS50 / DLS50")


w <- ggplot(data = alles_7030, aes(x=as.character(RPM), y=d, fill=RPM)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "area")+
  xlab("RPM")+
  ylab("Capsule size [µm]")+
  theme(legend.position = "none")+
  ggtitle("Capsule density depending on the mixing speed for PBS70 / DLS30")

