library(tidyverse)
library(plyr)
library(ggplot2)
library(readr)

name <- "PBS-DLS 50:50"

# two diffrent files options

file <- "5050 CPt"
PVA <- "PVA 72k"
# # 
# file <- "7030"
# PVA <- "PVA 13k"

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

write.csv(dc100,"currentdata/AllData/dc100.csv", row.names = FALSE)

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


write.csv(dc200,"currentdata/AllData/dc200.csv", row.names = FALSE)

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


write.csv(dc300,"currentdata/AllData/dc300.csv", row.names = FALSE)

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


write.csv(dc400,"currentdata/AllData/dc400.csv", row.names = FALSE)

mydir5 = "currentdata/AllData/"
myfiles5 = list.files(path=mydir5, pattern="*.csv", full.names=TRUE)
Alles <- ldply(myfiles5, read_csv)

ggplot(data = Alles, aes(x=as.character(RPM), y=d, fill=RPM)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin(scale = "area")+
  xlab("rpm")+
  ylab("Wielkość mikrosfer [µm]")+
  theme(legend.position = "none")+
  ggtitle("Zależność wielkości kapsułek od prędkości mieszania W/O")

ggsave(filename = paste0("figures/",PVA,"",file,"rpmsize.png"), dpi = 600)

