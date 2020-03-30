library(tidyverse)
library(readxl)

data <- read_xlsx("release xlsx/regresja.xlsx")


model0 <- lm(formula = C~ABS0, data = data)
summary(model0)
plot(model0)

model73 <- lm(formula = C~ABS73, data = data)
summary(model73)

model55 <- lm(formula = C~ABS55, data = data)
summary(model55)


plot0 <- ggplot(data, aes(x = C, y = ABS0))+
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = F) +
  xlab("CPt concentrate") +
  ylab("Absorbance") +
  ggtitle("CPt in water regression model") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
  

ggsave(filename = "figures/regresja/plot0.png", width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()

plot73 <- ggplot(data, aes(x = C, y = ABS73))+
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = F) +
  xlab("CPt concentrate") +
  ylab("Absorbance") +
  ggtitle("CPt in PBS-DLS 70:30 supernatant regression model") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave(filename = "figures/regresja/plot73.png", width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()

Plot55 <- ggplot(data, aes(x = C, y = ABS55))+
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = F) +
  xlab("CPt concentrate") +
  ylab("Absorbance") +
  ggtitle("CPt in PBS-DLS 50:50 supernatant regression model") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

ggsave(filename = "figures/regresja/plot55.png", width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()

data1 <- read_xlsx("release xlsx/regresion formula.xlsx")



extraction <- ggplot(data = data1) + 
  geom_line(aes(x = Day, y = extABS73, color='70:30')) +
  geom_line(aes(x = Day, y = extABS55, color='50:50')) +
  geom_point(aes(x = Day, y = extABS73, color='70:30'), size = 3)+
  geom_point(aes(x = Day, y = extABS55, color='50:50'), size =3) +
  ggtitle("Profil uwalniania leku dla PBS-DLS") +
  labs(color = "Polimer") + 
  xlim(0,40) +
  xlab("Czas [dni]")+
  ylab("Uwolniony lek [%]") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
 
  
ggsave(filename = "figures/regresja/comparePBSDLS.png", width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()

extraction 

data2 <- data1 %>% 
  select(rpm, proc, labels)

data2 <- drop_na(data2)


ggplot(data = data2, aes(x = rpm, y = proc, fill = labels, na.rm = T))+ 
  geom_bar(stat = "identity", position = "dodge", na.rm = T) +
  ggtitle("Ilość zenkapsulowanego leku dla PBS-DLS") +
  xlab("rpm")+
  ylab("Zenkapsulowany lek [%]") +
  scale_fill_discrete(name = "Polimer") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))
 
  
ggsave(filename = "figures/regresja/compareRPM.png", width = 15, height = 10, scale = 0.5, dpi = 600)
dev.off()
