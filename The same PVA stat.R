library(tidyverse)
library(plyr)
library(ggplot2)
library(readr)
library(xlsx)

name <- "PBS-DLS 70:30"

# two diffrent files options

# file <- "5050 1"
# PVA <- "PVA 72k"

file <- "7030"
PVA <- "PVA 13k"

# 100 RPM 

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


# 200 RPM 

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



# 300 RPM 

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



# 400 RPM 

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


tab1 <- merge(dc100, row.names = RPM, dc200, row.names = RPM, all = T)
tab2 <- merge(tab1, row.names = RPM, dc300, row.names = RPM, all = T)
tab3 <- merge(tab2, row.names = RPM, dc400, row.names = RPM, all = T)


kwantyl <- ggplot(tab3, aes(sample = d)) +
  stat_qq() + 
  stat_qq_line()+
  ggtitle("QQ plot")+
  facet_wrap( ~RPM)

ggsave(filename = paste0("figures/",PVA,"",file,"QQPLOT.png"), width = 15, height = 10, scale = 0.5, dpi = 600)

stat <- kruskal.test(tab3$d~tab3$RPM)


write.xlsx(as.data.frame(stat), paste0("stat/", file,"kruskaltest.xlsx"))


library(FSA)

DT <- dunnTest(tab3$d~as.factor(tab3$RPM),
               method="bh")

x <- as.data.frame.table(DT)


write.xlsx(x, paste0("stat/", file,"dunntest.xlsx"))
