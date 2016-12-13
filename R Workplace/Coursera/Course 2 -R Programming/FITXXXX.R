
#Start of code

install.packages("devtools")
library("devtools")
devtools::install_github("rstudio/EDAWR")
library(EDAWR)

install_github("easyGgplot2", "kassambara")
library("easyGgplot2")

install.packages("dplyr")
library("dplyr")

install.packages("tidyr")
library("tidyr")

install.packages("psych")
library("psych")

require(ggplot2)
require(plyr)
require(MASS)
require(foreign)
require(grid)


Tr.csv <- read.csv("training.csv",header = TRUE)









#Data acuqistion
S0.csv <- read.csv("Suicide, Number of Death,Rate,Proportion.csv",header = TRUE)

# Make a new colomun for year (as factor)
S0.csv$Year.f <- factor(S0.csv$Year)

#Group data
S0.csv$Group <- S0.csv$Age.Group

levels(S0.csv$Group) <- list(
    G1 = c("15-19", "20-24"),
    G2 = c("25-29", "30-34", "35-39", "40-44"),
    G3 = c("45-49", "50-54", "55-59", "60-64"),
    G4 = c("65-69", "70-74", "75-79", "80-84", "85-Over"))

#Data exploration
ggplot(S0.csv) + aes(x=Year.f , y=No, color=Gender) + geom_point() + facet_grid(Gender ~ Age.Group)

ggplot(S0.csv) + aes(x=Year.f , y=No, color=Gender) + geom_point() + facet_wrap( ~ Age.Group, nrow = 3)

ggplot(S0.csv) + aes(x=Year.f, y=No, fill=Year.f) + geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=No), vjust=1.6, color="white",position = position_dodge(0.9), size=3.5) +
    scale_fill_brewer(palette="Paired") + theme_minimal() + facet_grid(Gender ~ Age.Group)

ggplot(data = S0.csv) + aes(x = Age.Group, y = No, colour=Gender) + geom_point() + geom_path()

describeBy(S0.csv$No, group = list(S0.csv$Age.Group, S0.csv$Gender))

# Data filtering
S1.male <- S0.csv %>%
    dplyr::filter(Gender == 'Male') %>%
    dplyr::select(one_of(c("Age.Group", "Year", "Year.f", "No")))

S1.female <- S0.csv %>%
    dplyr::filter(Gender == 'Female') %>%
    dplyr::select(one_of(c("Age.Group", "Year", "Year.f", "No")))

#Descriptive Analysis
ggplot(data = S1.male) + aes(x = Age.Group, y = No, colour=Year.f) + geom_point()  + ylim(0, 300)
ggplot(data = S1.female) + aes(x = Age.Group, y = No, colour=Year.f) + geom_point() + ylim(0, 300)


ggplot(data=S1.male)+aes(x=Age.Group, y=No, fill=Year.f) + geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=No), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) +
    scale_fill_brewer(palette="Paired") + theme_minimal()

ggplot(S1.male) + aes(x=Year.f , y=No) + geom_point() + facet_wrap( ~ Age.Group, nrow=3)
ggplot(S1.female) + aes(x=Year.f , y=No) + geom_point() + facet_wrap( ~ Age.Group, nrow=3)

ggplot(S1.male) + aes(x=Year.f , y=No, fill=Year.f) + geom_bar(stat="identity", position=position_dodge()) +
    facet_wrap( ~ Age.Group, nrow=3) + scale_fill_brewer(palette="Paired")


# Date filtering and selecting
S2.male <- S0.csv %>%
    dplyr::filter(S0.csv$Gender == 'Male' & S0.csv$Year < 2008) %>%
    dplyr::select(one_of(c("Age.Group", "Year", "Year.f", "No")))

S2.female <- S0.csv %>%
    dplyr::filter(S0.csv$Gender == 'Female' & S0.csv$Year < 2008) %>%
    dplyr::select(one_of(c("Age.Group", "Year", "Year.f", "No")))

# Change levels:
S2.male$Group <- S2.male$Age.Group

levels(S2.male$Group) <- list(
    G1 = c("15-19", "20-24"),
    G2 = c("25-29", "30-34", "35-39", "40-44"),
    G3 = c("45-49", "50-54", "55-59", "60-64"),
    G4 = c("65-69", "70-74", "75-79", "80-84", "85-Over"))

S2.female$Group <- S2.female$Age.Group

levels(S2.female$Group) <- list(
    G1 = c("15-19", "20-24"),
    G2 = c("25-29", "30-34", "35-39", "40-44"),
    G3 = c("45-49", "50-54", "55-59", "60-64"),
    G4 = c("65-69", "70-74", "75-79", "80-84", "85-Over"))

#Analysis Groups
S3.male <- S2.male %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

S3.female <- S2.female %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

S3.male$Year.f <- factor(S3.male$Year)
S3.female$Year.f <- factor(S3.female$Year)

ggplot(S3.male) + aes(x=Year.f , y=Avg) + geom_point() + facet_wrap( ~ Group)
ggplot(S3.female) + aes(x=Year.f , y=Avg) + geom_point() + facet_wrap( ~ Group)

S3.male$Gender<- rep("Male", 28)
S3.female$Gender<- rep("Female", 28)

ggplot(bind_rows(S3.male,S3.female)) + aes(x=Year.f , y=Avg, color=Gender) + geom_point() + facet_wrap( ~ Group)

ggplot() +
    geom_point(data=S3.male, aes(x=Year, y=Avg)) +
    geom_smooth(data=S3.male, aes(x=Year, y=Avg), fill="blue", colour="darkblue", size=1)+
    facet_wrap( ~ Group)

# Statistical Test
# Aggregation Group G1
S4.male <- S2.male %>%
    dplyr::filter(S2.male$Group == "G1") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
M1.G1 <- glm.nb(data = S4.male, Avg ~ Year)
M2.G1 <- data.frame(Year = c(2008,2009))
M2.G1$Pred <- predict(M1.G1, M2.G1 ,type = "response")
M2.G1$Group <- rep("G1",2)
M2.G1$Year.f <- factor(M2.G1$Year)

# Aggregation Group G2
S4.male <- S2.male %>%
    dplyr::filter(S2.male$Group == "G2") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
M1.G2 <- glm.nb(data = S4.male, Avg ~ Year)
M2.G2 <- data.frame(Year = c(2008,2009))
M2.G2$Pred <- predict(M1.G2, M2.G2 ,type = "response")
M2.G2$Group <- rep("G2",2)
M2.G2$Year.f <- factor(M2.G2$Year)

# Aggregation Group G3
S4.male <- S2.male %>%
    dplyr::filter(S2.male$Group == "G3") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
M1.G3 <- glm.nb(data = S4.male, Avg ~ Year)
M2.G3 <- data.frame(Year = c(2008,2009))
M2.G3$Pred <- predict(M1.G3, M2.G3 ,type = "response")
M2.G3$Group <- rep("G3",2)
M2.G3$Year.f <- factor(M2.G3$Year)

# Aggregation Group G4
S4.male <- S2.male %>%
    dplyr::filter(S2.male$Group == "G4") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
M1.G4 <- glm.nb(data = S4.male, Avg ~ Year)
M2.G4 <- data.frame(Year = c(2008,2009))
M2.G4$Pred <- predict(M1.G4, M2.G4 ,type = "response")
M2.G4$Group <- rep("G4",2)
M2.G4$Year.f <- factor(M2.G4$Year)

#Test on 2008
t.test(S0.csv$No[S0.csv$Group == "G1" & S0.csv$Year == 2008 & S0.csv$Gender == "Male"],
       mu = M2.G1$Pred[M2.G1$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G2" & S0.csv$Year == 2008 & S0.csv$Gender == "Male"],
       mu = M2.G2$Pred[M2.G2$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G3" & S0.csv$Year == 2008 & S0.csv$Gender == "Male"],
       mu = M2.G3$Pred[M2.G3$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G4" & S0.csv$Year == 2008 & S0.csv$Gender == "Male"],
       mu = M2.G4$Pred[M2.G4$Year == 2008])

#Test on 2009
t.test(S0.csv$No[S0.csv$Group == "G1" & S0.csv$Year == 2009 & S0.csv$Gender == "Male"],
       mu = M2.G1$Pred[M2.G1$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G2" & S0.csv$Year == 2009 & S0.csv$Gender == "Male"],
       mu = M2.G2$Pred[M2.G2$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G3" & S0.csv$Year == 2009 & S0.csv$Gender == "Male"],
       mu = M2.G3$Pred[M2.G3$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G4" & S0.csv$Year == 2009 & S0.csv$Gender == "Male"],
       mu = M2.G4$Pred[M2.G4$Year == 2009])


# Aggregation Group G1
S4.female <- S2.female %>%
    dplyr::filter(S2.female$Group == "G1") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
F1.G1 <- glm.nb(data = S4.female, Avg ~ Year)
F2.G1 <- data.frame(Year = c(2008,2009))
F2.G1$Pred <- predict(F1.G1, F2.G1 ,type = "response")
F2.G1$Group <- rep("G1",2)
F2.G1$Year.f <- factor(F2.G1$Year)

# Aggregation Group G2
S4.female <- S2.female %>%
    dplyr::filter(S2.female$Group == "G2") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
F1.G2 <- glm.nb(data = S4.male, Avg ~ Year)
F2.G2 <- data.frame(Year = c(2008,2009))
F2.G2$Pred <- predict(F1.G2, F2.G2 ,type = "response")
F2.G2$Group <- rep("G2",2)
F2.G2$Year.f <- factor(F2.G2$Year)

# Aggregation Group G3
S4.female <- S2.female %>%
    dplyr::filter(S2.female$Group == "G3") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
F1.G3 <- glm.nb(data = S4.female, Avg ~ Year)
F2.G3 <- data.frame(Year = c(2008,2009))
F2.G3$Pred <- predict(F1.G3, F2.G3 ,type = "response")
F2.G3$Group <- rep("G3",2)
F2.G3$Year.f <- factor(F2.G3$Year)

# Aggregation Group G4
S4.female <- S2.female %>%
    dplyr::filter(S2.female$Group == "G4") %>%
    dplyr::group_by(Group,Year) %>%
    dplyr::summarise(Avg = mean(No))

# Negative Bionomial
F1.G4 <- glm.nb(data = S4.female, Avg ~ Year)
F2.G4 <- data.frame(Year = c(2008,2009))
F2.G4$Pred <- predict(F1.G4, F2.G4 ,type = "response")
F2.G4$Group <- rep("G4",2)
F2.G4$Year.f <- factor(F2.G4$Year)

#Test on 2008
t.test(S0.csv$No[S0.csv$Group == "G1" & S0.csv$Year == 2008 & S0.csv$Gender == "Female"],
       mu = F2.G1$Pred[F2.G1$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G2" & S0.csv$Year == 2008 & S0.csv$Gender == "Female"],
       mu = F2.G2$Pred[F2.G2$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G3" & S0.csv$Year == 2008 & S0.csv$Gender == "Female"],
       mu = F2.G3$Pred[F2.G3$Year == 2008])

t.test(S0.csv$No[S0.csv$Group == "G4" & S0.csv$Year == 2008 & S0.csv$Gender == "Female"],
       mu = F2.G4$Pred[F2.G4$Year == 2008])

#Test on 2009
t.test(S0.csv$No[S0.csv$Group == "G1" & S0.csv$Year == 2009 & S0.csv$Gender == "Female"],
       mu = F2.G1$Pred[F2.G1$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G2" & S0.csv$Year == 2009 & S0.csv$Gender == "Female"],
       mu = F2.G2$Pred[F2.G2$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G3" & S0.csv$Year == 2009 & S0.csv$Gender == "Female"],
       mu = F2.G3$Pred[F2.G3$Year == 2009])

t.test(S0.csv$No[S0.csv$Group == "G4" & S0.csv$Year == 2009 & S0.csv$Gender == "Female"],
       mu = F2.G4$Pred[F2.G4$Year == 2009])


#Data acuqistion for Spearsman test
S1.csv <- read.csv("Unemployment Rate,State,Suicide Number.csv",header = TRUE)

ggplot(S1.csv) + aes(S1.csv$Rate.Unemployment, S1.csv$No.Suicide, color=S1.csv$Gender) + geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm)

#Spearman's rank correlation rho all, male, and female
cor.test(S1.csv$Rate.Unemployment, S1.csv$No.Suicide, method = "spearman")

cor.test(S1.csv$Rate.Unemployment[S1.csv$Gender == "Male"], S1.csv$No.Suicide[S1.csv$Gender == "Male"],
         method = "spearman")

cor.test(S1.csv$Rate.Unemployment[S1.csv$Gender == "Female"], S1.csv$No.Suicide[S1.csv$Gender == "Female"],
         method = "spearman")


#End of code
