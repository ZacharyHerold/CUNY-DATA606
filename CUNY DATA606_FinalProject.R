library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)


## Cleaning the Class of 2018 TOEFL data

ibt18 <- data.frame(read_excel("C:/Users/ZacharyHerold/Documents/DATA606/Final.Project/Class18.ibt.xlsx", stringsAsFactors=FALSE))
ibt18 <- ibt18[c(2,5,7,8,9,10,11)] 
names(ibt18) <- c("ID", "attempts.ibt", "ibt.R", "ibt.L","ibt.S", "ibt.W","ibt.ttl")
ibt18$ID <- factor(ibt18$ID)
ibt18$attempts.ibt <- as.numeric(ibt18$attempts.ibt)

ibt18sum <- ibt18 %>%
  group_by(ID) %>%
  summarise(no.att.ibt = max(attempts.ibt), max.R = max(na.omit(ibt.R)), max.L = max(na.omit(ibt.L)), max.S = max(na.omit(ibt.S)), max.W = max(na.omit(ibt.W)), max.ibt = max(ibt.ttl), avg.ibt = mean(ibt.ttl))

ibt18sum$year <- rep("2018", nrow(ibt18sum))

ibt18sum$max.R


## Cleaning the Class of 2017 TOEFL data

ibt17 <- data.frame(read_excel("C:/Users/ZacharyHerold/Documents/DATA606/Final.Project/Class17.ibt.xlsx"))

ibt17sum <- ibt17 %>%
  group_by(ID) %>%
  summarise(no.att.ibt = max(attempt), max.R = max(na.omit(R)), max.L = max(na.omit(L)), max.S = max(na.omit(S)), max.W = max(na.omit(W)), max.ibt = max(ttl), avg.ibt = mean(ttl))

ibt17sum$year <- rep("2017", nrow(ibt17sum))

ibt17sum



## Cleaning the Class of 2018 SAT data

sat18 <- data.frame(read_excel("C:/Users/ZacharyHerold/Documents/DATA606/Final.Project/Class18.sat.xlsx"))
sat18 <- sat18[-c(5,37,38,39),]
sat18$attempt <- as.numeric(sat18$attempt)
sat18$V <- sat18$T - sat18$M

colnames(sat18)[1] <- "ID"
sat18sum <- sat18 %>%
  group_by(ID) %>%
  summarise(no.att.sat = max(attempt), max.V = max(na.omit(V)), max.sat = max(T), avg.sat = mean(T))

sat18sum$max.V


## Cleaning the Class of 2017 SAT data
sat17 <- data.frame(read_excel("C:/Users/ZacharyHerold/Documents/DATA606/Final.Project/Class17.sat.xlsx"))

sat17$attempt <- as.numeric(sat17$attempt)
sat17$R <- as.numeric(sat17$R)
sat17$G <- as.numeric(sat17$G)
sat17$M <- as.numeric(sat17$M)
sat17$V[sat17$V == 0] <- NA
sat17$V[39] <- 580

str(sat17)

sat17sum <- sat17 %>%
  group_by(ID) %>%
  summarise(no.att.sat = max(attempt), max.V = max(na.omit(V)), max.sat = max(ttl), avg.sat = mean(ttl))



## Merging Class Data

class18 <- merge(ibt18sum, sat18sum, by = "ID")
nrow(class18)

class17 <- merge(ibt17sum, sat17sum, by = "ID")
nrow(class17)



## CLass of 2017 Regression of SAT max on TOEFL max

m_ibt17 <- lm(class17$max.sat ~ class17$max.ibt)
summary(m_ibt17)

p1 <- ggplot(class17, aes(x=max.L,y=max.sat, colour = "blue", size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p1


## TOEFL Reading = Multiple R-squared:  0.697
## TOEFL Total (Max) = Multiple R-squared:  0.6033
## TOEFL Writing = Multiple R-squared:  0.4885
## TOEFL Listening = Multiple R-squared:  0.4724
## TOEFL Speaking = Multiple R-squared:  0.1739



## CLass of 2018 Regression of SAT max on TOEFL max

m_ibt18 <- lm(class18$max.sat ~ class18$max.R)
summary(m_ibt18)

p2 <- ggplot(class18, aes(x=max.R,y=max.sat, colour = "blue", size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p2


## TOEFL Writing = Multiple R-squared:  0.6767
## TOEFL Reading = Multiple R-squared:  0.5039
## TOEFL Total (Max) = Multiple R-squared:  0.4758
## TOEFL Listening = Multiple R-squared:  0.3419
## TOEFL Speaking = Multiple R-squared:  0.07806



## CLass of 2017 Regression of SAT Verbal max on TOEFL max

m_ibt17 <- lm(class17$max.V ~ class17$max.ibt)
summary(m_ibt17)


p1 <- ggplot(class17, aes(x=max.W,y=max.sat, colour = "blue", size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p1


## TOEFL Reading = Multiple R-squared:  0.7174
## TOEFL Total (Max) = Multiple R-squared:  0.5523
## TOEFL Writing = Multiple R-squared:  0.4622



## CLass of 2018 Regression of SAT max on TOEFL max

m_ibt18 <- lm(class18$max.V ~ class18$max.ibt)
summary(m_ibt18)

## TOEFL Writing = Multiple R-squared:  0.7109
## TOEFL Total (Max) = Multiple R-squared:  0.5695
## TOEFL Reading = Multiple R-squared:  0.5039





ibt.sat <- rbind(class17, class18)
str(ibt.sat)

m_sat.all <- lm(max.sat ~ no.att.ibt + max.R + max.L + max.S + max.W + max.ibt + avg.ibt + year + no.att.sat, data = ibt.sat)
summary(m_sat.all)

m_sat.all2 <- lm(max.sat ~ max.R + max.L + max.S + max.W + max.ibt + avg.ibt + year, data = ibt.sat)
summary(m_sat.all2)

m_sat.all3 <- lm(max.sat ~ max.R + max.S + max.W + year, data = ibt.sat)
summary(m_sat.all3)

## Adjusted R-squared:  0.7616 

m_sat.all4 <- lm(max.sat ~ max.R + max.S + max.W, data = ibt.sat)
summary(m_sat.all4)

## Adjusted R-squared:  0.7455 



p2 <- ggplot(ibt.sat, aes(x=max.R,y=max.sat, colour = year, size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p2

p3 <- ggplot(ibt.sat, aes(x=max.S,y=max.sat, colour = year, size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p3

p4 <- ggplot(ibt.sat, aes(x=max.W,y=max.sat, colour = year, size = 2)) +
  geom_point()  + stat_smooth(method=lm, se=T, size = 1)
p4





ggplot(ibt.sat, aes(x=factor(year), y = max.ttl)) +
  geom_histogram(bins = 12, fill="blue", colour="black", alpha =0.6, )

ggplot(ibt17sum, aes(x=max.ibt)) +
  geom_histogram(bins = 12, fill="red", colour="black", alpha =0.6)

ggplot(ibt17sum, aes(x=avg.ibt)) +
  geom_histogram(bins = 12, fill="darkgreen", colour="black", alpha =0.6)


