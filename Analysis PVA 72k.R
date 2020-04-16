library(plyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(xlsx)

# Łączenie plików i oblicznie średnicy
file <- "after 100 7030"
name <- "PBS-DLS 70:30"

mydir = paste0("data/PVA 72k//", file, "")
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)
dat_csv

dc <- dat_csv %>% 
  mutate(Rq = Area/3,141592) %>% 
  mutate(Size = sqrt(Rq)*2) %>% 
  filter(Size >=1) %>% 
  select(Size)

s <- summary(dc)

# Histogram

h <- ggplot(dc, aes(x = Size)) +
  geom_histogram(binwidth = 10,
                 color="black",
                 fill="lightblue",
                 linetype="dashed") +
  xlab("Rozmiar mikrosfer [µm]")+
  ylab("Ilość") +
  scale_x_continuous(name = "Rozmiar mikrosfer [µm]", breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0,100))+
  ggtitle(paste0("Rozkład wielkości kapsułek dla " ,name,"")) +
  theme_light()

ggsave(filename = paste0("figures/", file,"hist.png"), width = 15, height = 10, scale = 0.5, dpi = 600)

x <- summarytools::descr(dc)

write.xlsx(x, paste0("stat/", file,"podstawowe.xlsx"))

dev.off()



# Wyliczanie frakcji 

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

tab1 <- merge(dc20_40, row.names = name, dcwieksze20_40, row.names = name, all = T)
tab2 <- merge(tab1, row.names = name, dcmniejsze20_40, row.names = name, all = T)


# Violinboxplot

b <- ggplot(tab2, aes(x=name, y=Size, fill=name)) +
  geom_violin(width=2) +
  geom_boxplot(width=0.3, color="black", alpha=0.8) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(name = "Wielkość [µm]", breaks = c(0,10,20,30,40,50,60,70,80,90), limits = c(0,100)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle(paste0("Ilość kapsułek dla poszczególnych frakcji ",name,"")) +
  xlab("Gęstość")+
  ylab("Wielkość [µm]")

ggsave(filename = paste0("figures/", file,"violinbox.png"), width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()
# Procentowa zawartość

q <- count(dc)
w <- count(dcmniejsze20_40)
e <- count(dcwieksze20_40)
r <- count(dc20_40)

summary(q)
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

# circlepolot




pct <- round(tab4$n*100)
lbs <- paste(tab4$name, pct)
lbs <- paste(lbs,"%", sep = "")

png(filename = paste0("figures/",file,"piechart.png"), units="px", width=2000, height=1600, res=300)
pie(tab4$n,labels=lbs, col = c("gold", "red", "orange"),
      main=paste0("Procentowa zawartość frakcji ",name,""))
dev.off()


# Wywoływanie 
# b - violin plot; 
# h - histogram; 
# p - piechart, 
# s -podstawowe statystyki