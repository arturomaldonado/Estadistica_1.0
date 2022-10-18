library(rio)
pop = import("Populist speech data.xlsx")

mean(pop$score)
median(pop$score)
sd(pop$score)

library(ggplot2)
ggplot(pop, aes(x=score))+
  geom_histogram()+
  xlab("Score de populismo")+
  ylab("Frecuencia")

table(pop$region)
pop$region2 = as.factor(pop$region) #caracter a factor. Faltan los levels
pop$region3 = as.numeric(pop$region2) #factor a numérico

round(prop.table(table(pop$region2))*100, 1)

library(tidyverse)
library(ggrepel)
ggplot(pop, aes(y=score, x=region2))+
  geom_boxplot()+
  ylab("Score de populismo")+
  xlab("Región")+
  geom_text_repel(data=. %>% #Esto no es necesario para el examen
              group_by(region2) %>%  
              filter(score %in% boxplot.stats(score, coef=1.4)$out),
            aes(label=country, y=score), nudge_x=0.1, colour="red", size=2, hjust=0) +
  theme_minimal()

#Medias por región
mean(pop$score[pop$region3==1])
mean(pop$score[pop$region3==2])
mean(pop$score[pop$region3==3])
mean(pop$score[pop$region3==4])

#Medianas por región
median(pop$score[pop$region3==1])
median(pop$score[pop$region3==2])
median(pop$score[pop$region3==3])
median(pop$score[pop$region3==4])

#Rangos intercuartiles por región
IQR(pop$score[pop$region3==1])
IQR(pop$score[pop$region3==2])
IQR(pop$score[pop$region3==3])
IQR(pop$score[pop$region3==4])

#Intervalos de confianza total y por región
library(lsr)
ciMean(pop$score)
ciMean(pop$score[pop$region3==1])
ciMean(pop$score[pop$region3==2])
ciMean(pop$score[pop$region3==3])
ciMean(pop$score[pop$region3==4])

#Gráfico de intervalos de confianza por presidente/primer ministro
library(Rmisc)
pop.pr <- group.CI(score~president, pop)
graf <- ggplot(pop.pr, aes(x=president, y=score.mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=score.lower, ymax=score.upper), width=0.2)+
  geom_text(aes(label=paste(round(score.mean, 1))), vjust=-4, size=3)+
  xlab("Presidente") + ylab("Score de populismo")
graf

ç#Gráfico de intervalos de confianza por región
pop.region <- group.CI(score~region2, pop)
graf1 <- ggplot(pop.region, aes(x=region2, y=score.mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=score.lower, ymax=score.upper), width=0.2)+
  geom_text(aes(label=paste(round(score.mean, 1))), vjust=-4, size=3)+
  xlab("SRegión") + ylab("Score de populismo")
graf1

#Gráfico de intervalos de confianza por ideología
table(pop$left_right)
pop.lr <- group.CI(score~left_right, pop)
graf2 <- ggplot(pop.lr, aes(x=left_right, y=score.mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=score.lower, ymax=score.upper), width=0.2)+
  geom_text(aes(label=paste(round(score.mean, 1))), vjust=-4, size=3)+
  xlab("Ideología") + ylab("Score de populismo")
graf2

#Faltaría las pruebas de significancia

#Para Score de populismo por presidente/primer ministro
t.test()

#Para Score de populismo por región

anova = aov()
summary(anova)

#Para Score de populismo por ideología
anova2 = aov()
summary(anova2)

