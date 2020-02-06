library(plyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(viridis)
library(hrbrthemes)

file <- "5050"
name <- "PBS-DLS 50:50"

# PVA 13k-23k

# Łączenie plików i oblicznie średnicy dla 100 RPM 

mydir = paste0("data/PVA 13k/100 ",file,"")
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

mydir1 = paste0("data/PVA 13k/200 ",file,"")
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

mydir2 = paste0("data/PVA 13k/300 ",file,"")
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

mydir3 = paste0("data/PVA 13k/400 ",file,"")
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

# PVA 72K
# Łączenie plików i oblicznie średnicy dla 100 RPM 

mydiri = paste0("data/PVA 72k/100 ",file," 1")
myfilesi = list.files(path=mydiri, pattern="*.csv", full.names=TRUE)
myfilesi

dat_csvi = ldply(myfilesi, read_csv)
dat_csvi

dci <- dat_csvi %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_40i <- dat_csvi %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_40i <- dat_csvi %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_40i <- dat_csvi %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

qi <- count(dci)
wi <-count(dcmniejsze20_40i)
ei <- count(dcwieksze20_40i)
ri <- count(dc20_40i)

"Stusunek zwartości:"

"Poniżej 20:"
mniejszei <- wi/qi

"Powyżej 40:"
wiekszei <- ei/qi

"Frakcja 20-40:"
frakcjai <- ri/qi

mniejszei <- as.data.frame(mniejszei) %>% 
  mutate(n, name = "< 20µm")

wiekszei <- as.data.frame(wiekszei) %>% 
  mutate(n, name = "> 40µm")

frakcjai <- as.data.frame(frakcjai) %>% 
  mutate(n, name = "20-40µm")

tab3i <- merge(mniejszei, row.names = name, frakcjai, row.names = name, all = T)
tab4i <- merge(tab3i, row.names = name, wiekszei, row.names = name, all = T)
tab4i <- tab4i %>% 
  rename(fraction = n)

# Łączenie plików i oblicznie średnicy dla 200 RPM 

mydir1i = paste0("data/PVA 72k/200 ",file," 1")
myfiles1i = list.files(path=mydir1i, pattern="*.csv", full.names=TRUE)
myfiles1i

dat_csv1i = ldply(myfiles1i, read_csv)
dat_csv1i

dc1i <- dat_csv1i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_401i <- dat_csv1i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_401i <- dat_csv1i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_401i <- dat_csv1i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q1i <- count(dc1i)
w1i <-count(dcmniejsze20_401i)
e1i <- count(dcwieksze20_401i)
r1i <- count(dc20_401i)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze1i <- w1i/q1i

"Powyżej 40:"
wieksze1i <- e1i/q1i

"Frakcja 20-40:"
frakcja1i <- r1i/q1i

mniejsze1i <- as.data.frame(mniejsze1i) %>% 
  mutate(n, name = "< 20µm")

wieksze1i <- as.data.frame(wieksze1i) %>% 
  mutate(n, name = "> 40µm")

frakcja1i <- as.data.frame(frakcja1i) %>% 
  mutate(n, name = "20-40µm")

tab31i <- merge(mniejsze1i, row.names = name, frakcja1i, row.names = name, all = T)
tab41i <- merge(tab31i, row.names = name, wieksze1i, row.names = name, all = T)
tab41i <- tab41i %>% 
  rename(fraction = n)


# Łączenie plików i oblicznie średnicy dla 300 RPM 

mydir2i = paste0("data/PVA 72k/300 ",file," 1")
myfiles2i = list.files(path=mydir2i, pattern="*.csv", full.names=TRUE)
myfiles2i

dat_csv2i = ldply(myfiles2i, read_csv)
dat_csv2i

dc2i <- dat_csv2i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_402i <- dat_csv2i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_402i <- dat_csv2i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_402i <- dat_csv2i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q2i <- count(dc2i)
w2i <-count(dcmniejsze20_402i)
e2i <- count(dcwieksze20_402i)
r2i <- count(dc20_402i)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze2i <- w2i/q2i

"Powyżej 40:"
wieksze2i <- e2i/q2i

"Frakcja 20-40:"
frakcja2i <- r2i/q2i

mniejsze2i <- as.data.frame(mniejsze2i) %>% 
  mutate(n, name = "< 20µm")

wieksze2i <- as.data.frame(wieksze2i) %>% 
  mutate(n, name = "> 40µm")

frakcja2i <- as.data.frame(frakcja2i) %>% 
  mutate(n, name = "20-40µm")

tab32i <- merge(mniejsze2i, row.names = name, frakcja2i, row.names = name, all = T)
tab42i <- merge(tab32i, row.names = name, wieksze2i, row.names = name, all = T)
tab42i <- tab42i %>% 
  rename(fraction = n)


# Łączenie plików i oblicznie średnicy dla 400 RPM 

mydir3i = paste0("data/PVA 72k/400 ",file," 1")
myfiles3i = list.files(path=mydir3i, pattern="*.csv", full.names=TRUE)
myfiles3i

dat_csv3i = ldply(myfiles3i, read_csv)
dat_csv3i

dc3i <- dat_csv3i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

dc20_403i <- dat_csv3i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >=20, Size <=40) %>% 
  mutate(Size, name = "20-40µm")

dcmniejsze20_403i <- dat_csv3i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size <20) %>% 
  mutate(Size, name = "<20µm")

dcwieksze20_403i <- dat_csv3i %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2)%>% 
  select(Size) %>% 
  filter(Size >40) %>% 
  mutate(Size, name = ">40µm")

q3i <- count(dc3i)
w3i <-count(dcmniejsze20_403i)
e3i <- count(dcwieksze20_403i)
r3i <- count(dc20_403i)

"Stusunek zwartości:"

"Poniżej 20:"
mniejsze3i <- w3i/q3i

"Powyżej 40:"
wieksze3i <- e3i/q3i

"Frakcja 20-40:"
frakcja3i <- r3i/q3i

mniejsze3i <- as.data.frame(mniejsze3i) %>% 
  mutate(n, name = "< 20µm")

wieksze3i <- as.data.frame(wieksze3i) %>% 
  mutate(n, name = "> 40µm")

frakcja3i <- as.data.frame(frakcja3i) %>% 
  mutate(n, name = "20-40µm")

tab33i <- merge(mniejsze3i, row.names = name, frakcja3i, row.names = name, all = T)
tab43i <- merge(tab33i, row.names = name, wieksze3i, row.names = name, all = T)
tab43i <- tab43i %>% 
  rename(fraction = n)
# Wykres curves

# dla <20 µm for 
df1 <- data.frame(x=c("100rpm*","200rpm*","300rpm*", "400RPM*","100rpm**","200rpm**","300rpm**", "400rpm**"), 
                  y=c(mniejsze$n, mniejsze1$n, mniejsze2$n, mniejsze3$n,mniejszei$n, mniejsze1i$n, mniejsze2i$n, mniejsze3i$n))


ggplot(df1, aes(x=x, y=y)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Proportion") +
  xlab("Speed") +
  ggtitle(paste0("Zawartość frakcji <20 µm ",name,""))
  
ggsave(filename = paste0("figures/", file,"frakcji20.png"), dpi = 600)

# dla 20-40 µm for 

df2 <- data.frame(x=c("100rpm*","200rpm*","300rpm*", "400rpm*","100rpm**","200rpm**","300rpm**", "400rpm**"), 
                  y=c(frakcja$n, frakcja1$n, frakcja2$n, frakcja3$n,frakcjai$n, frakcja1i$n, frakcja2i$n, frakcja3i$n))


ggplot(df2, aes(x=x, y=y)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Proporcja") +
  xlab("Prędkość") +
  ggtitle(paste0("Zawartość frakcji 20-40 µm ",name,""))
  
ggsave(filename = paste0("figures/", file,"frakcji2040.png"), dpi = 600)
# dla >40 µm for

df3 <- data.frame(x=c("100rpm*","200rpm*","300rpm*", "400rpm*","100rpm**","200rpm**","300rpm**", "400rpm**"), 
                  y=c(wieksze$n, wieksze1$n, wieksze2$n, wieksze3$n, wiekszei$n, wieksze1i$n, wieksze2i$n, wieksze3i$n))


ggplot(df3, aes(x=x, y=y)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Proporcja") +
  xlab("Prędkość mieszania") +
  ggtitle(paste0("Zawartość frakcji >40 µm ",name,""))


ggsave(filename = paste0("figures/", file,"frakcji40.png"), dpi = 600)