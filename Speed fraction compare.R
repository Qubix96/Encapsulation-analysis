library(plyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(summarytools)
library(viridis)
library(hrbrthemes)


name <- "PBS-DLS 70:30"

# two diffrent files options

file <- "7030 CPt"
PVA <- "PVA 72k"

# file <- "5050"
# PVA <- "PVA 13k"

# Łączenie plików i oblicznie średnicy dla 100 RPM 

mydir = paste0("data/",PVA,"/100 ",file,"")
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)
dat_csv

dc <- dat_csv %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_40 <- dat_csv %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_40 <- dat_csv %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_40 <- dat_csv %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q <- count(dc)
w <-count(dcmniejsze20_40)
e <- count(dcwieksze20_40)
r <- count(dc20_40)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze <- w/q

"Powyżej 40:"
wieksze <- e/q

"Frakcja 20-40:"
frakcja <- r/q

mniejsze <- as.data.frame(mniejsze) %>% 
  mutate(n, name = "< 20µm")

wieksze <- as.data.frame(wieksze) %>% 
  mutate(n, name = "> 40µm")

frakcja <- as.data.frame(frakcja) %>% 
  mutate(n, name = "20-40µm")

tab3 <- merge(mniejsze, row.names = name, frakcja, row.names = name, all = T)
tab4 <- merge(tab3, row.names = name, wieksze, row.names = name, all = T)
tab4 <- tab4 %>% 
  rename(fraction = n)

# Łączenie plików i oblicznie średnicy dla 200 RPM 

mydir1 = paste0("data/",PVA,"/200 ",file,"")
myfiles1 = list.files(path=mydir1, pattern="*.csv", full.names=TRUE)
myfiles1

dat_csv1 = ldply(myfiles1, read_csv)
dat_csv1

dc1 <- dat_csv1 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_401 <- dat_csv1 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_401 <- dat_csv1 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_401 <- dat_csv1 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q1 <- count(dc1)
w1 <-count(dcmniejsze20_401)
e1 <- count(dcwieksze20_401)
r1 <- count(dc20_401)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze1 <- w1/q1

"Powyżej 40:"
wieksze1 <- e1/q1

"Frakcja 20-40:"
frakcja1 <- r1/q1

mniejsze1 <- as.data.frame(mniejsze1) %>% 
  mutate(n, name = "< 20µm")

wieksze1 <- as.data.frame(wieksze1) %>% 
  mutate(n, name = "> 40µm")

frakcja1 <- as.data.frame(frakcja1) %>% 
  mutate(n, name = "20-40µm")

tab31 <- merge(mniejsze1, row.names = name, frakcja1, row.names = name, all = T)
tab41 <- merge(tab31, row.names = name, wieksze1, row.names = name, all = T)
tab41 <- tab41 %>% 
  rename(fraction = n)


# Łączenie plików i oblicznie średnicy dla 300 RPM 

mydir2 = paste0("data/",PVA,"/300 ",file,"")
myfiles2 = list.files(path=mydir2, pattern="*.csv", full.names=TRUE)
myfiles2

dat_csv2 = ldply(myfiles2, read_csv)
dat_csv2

dc2 <- dat_csv2 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_402 <- dat_csv2 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_402 <- dat_csv2 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_402 <- dat_csv2 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q2 <- count(dc2)
w2 <-count(dcmniejsze20_402)
e2 <- count(dcwieksze20_402)
r2 <- count(dc20_402)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze2 <- w2/q2

"Powyżej 40:"
wieksze2 <- e2/q2

"Frakcja 20-40:"
frakcja2 <- r2/q2

mniejsze2 <- as.data.frame(mniejsze2) %>% 
  mutate(n, name = "< 20µm")

wieksze2 <- as.data.frame(wieksze2) %>% 
  mutate(n, name = "> 40µm")

frakcja2 <- as.data.frame(frakcja2) %>% 
  mutate(n, name = "20-40µm")

tab32 <- merge(mniejsze2, row.names = name, frakcja2, row.names = name, all = T)
tab42 <- merge(tab32, row.names = name, wieksze2, row.names = name, all = T)
tab42 <- tab42 %>% 
  rename(fraction = n)


# Łączenie plików i oblicznie średnicy dla 400 RPM 

mydir3 = paste0("data/",PVA,"/400 ",file,"")
myfiles3 = list.files(path=mydir3, pattern="*.csv", full.names=TRUE)
myfiles3

dat_csv3 = ldply(myfiles3, read_csv)
dat_csv3

dc3 <- dat_csv3 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_403 <- dat_csv3 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_403 <- dat_csv3 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_403 <- dat_csv3 %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q3 <- count(dc3)
w3 <-count(dcmniejsze20_403)
e3 <- count(dcwieksze20_403)
r3 <- count(dc20_403)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze3 <- w3/q3

"Powyżej 40:"
wieksze3 <- e3/q3

"Frakcja 20-40:"
frakcja3 <- r3/q3

mniejsze3 <- as.data.frame(mniejsze3) %>% 
  mutate(n, name = "< 20µm")

wieksze3 <- as.data.frame(wieksze3) %>% 
  mutate(n, name = "> 40µm")

frakcja3 <- as.data.frame(frakcja3) %>% 
  mutate(n, name = "20-40µm")

tab33 <- merge(mniejsze3, row.names = name, frakcja3, row.names = name, all = T)
tab43 <- merge(tab33, row.names = name, wieksze3, row.names = name, all = T)
tab43 <- tab43 %>% 
  rename(fraction = n)

df1 <- data.frame(x=c("100rpm","200rpm","300rpm", "400rpm"), y=c(mniejsze$n, mniejsze1$n, mniejsze2$n, mniejsze3$n))


# ggplot(df1, aes(x=x, y=y)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
#   theme_ipsum() +
#   scale_y_continuous(limits = c(0,1)) +
#   ylab("Proportion") +
#   xlab("Prędkość mieszania") +
#   ggtitle(paste0("Zawartość frakcji <20 µm ",name,""))
# 
# ggsave(filename = paste0("figures/",PVA,"",file,"frakcji20.png"), width = 15, height = 10, scale = 0.5, dpi = 600)

# dla 20-40 ?m for 

df2 <- data.frame(x=c("100rpm","200rpm","300rpm", "400rpm"), y=c(frakcja$n, frakcja1$n, frakcja2$n, frakcja3$n))


# ggplot(df2, aes(x=x, y=y)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
#   theme_ipsum() +
#   scale_y_continuous(limits = c(0,1)) +
#   ylab("Proporcja") +
#   xlab("Prędkość mieszania") +
#   ggtitle(paste0("Zawartość frakcji 20-40 µm ",name,""))
# 
# ggsave(filename = paste0("figures/",PVA,"",file,"frakcji2040.png"), width = 15, height = 10, scale = 0.5, dpi = 600)

# dla >40 ?m for

df3 <- data.frame(x=c("100rpm","200rpm","300rpm", "400rpm"), y=c(wieksze$n, wieksze1$n, wieksze2$n, wieksze3$n))


# ggplot(df3, aes(x=x, y=y)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
#   theme_ipsum() +
#   scale_y_continuous(limits = c(0,1)) +
#   ylab("Proporcja") +
#   xlab("Prędkość mieszania") +
#   ggtitle(paste0("Zawartość frakcji >40 µm ",name,""))
# 
# ggsave(filename = paste0("figures/",PVA,"",file,"frakcji40.png"), width = 15, height = 10, scale = 0.5, dpi = 600)

df1$Wielkość <- as.character('< 20µm')

df2$Wielkość <- as.character("20-40µm")

df3$Wielkość <- as.character("> 40µm")

df <- rbind(df1, df2, df3)

ggplot(data = df, aes(x = x, y = y, fill = Wielkość)) +
  geom_col(position = "dodge2") +
  ylab("Proporcja") +
  xlab("Prędkość mieszania") +
  ggtitle("Zawartość frakcji") +
  theme_bw()

ggsave(filename = paste0("figures/", file,"frakcje.png"), width = 15, height = 10, scale = 0.5, dpi = 600)
