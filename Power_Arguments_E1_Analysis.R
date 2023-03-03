data <- read.csv("~/Desktop/Arguments & Power /Survival Game study/R scripts/RAW_DATA_Arguments_Power_E1 copy.csv", sep=";")
data<-na.omit(data)
data<-as.data.frame(data)
#condition: powerful == 1; powerless == 2
condition<- data$player.id_in_group
pair<-data$pair
#rating: average of 3 batches of independent ratings of arguments
rating<-data$raters_average
#power: manipulation check 
power<-data$power
#self_rating: rating of own arguments quality
self_rating<-data$self_arguments_quality
#other_rating: rating of the other participants' arguments quality
other_rating<-data$other_arguments_quality
#wordcount: number of words in the argument
wordcount<-data$Wordcount
gender<-data$gender
#inter-rater reliability test between 3 batches of raters
icc_data<-Ratings.for.ICC...Sheet1 <- read.csv("~/Desktop/Arguments & Power /Survival Game study/Ratings for ICC - Sheet1.csv")
library(irr)
icc (icc_data, model = c("twoway"), type = c("consistency"), unit = c("average"))
#inter-rater reliability is good; ICC=0.738
#manipulation check
t.test(power ~ condition, data = data)
#manipulation works; m(powerful)= 4.90; m(powerless)= 3.18; p<0.001
#descriptive statistics
library(psych)
powerful<-data$player.id_in_group == 1
powerless<-data$player.id_in_group == 2
describe(rating)
describe(rating[powerful])
describe(rating[powerless])
describe(power)
describe(power[powerful])
describe(power[powerless])
describe(wordcount)
describe(wordcount[powerful])
describe(wordcount[powerless])
describe(self_rating)
describe(self_rating[powerful])
describe(self_rating[powerless])
describe(other_rating)
describe(other_rating[powerful])
describe(other_rating[powerless])
#H1: Powerless participants will produce arguments that are independently rated as higher quality than the arguments produced by powerful participants.
library(lme4)
library(lmerTest)
H1model<-lmer(rating ~ condition + (1 | pair), data = data)
summary(H1model)
#(1 | pair) is singular --> simlpified model
H1model_simple<-lm(rating ~ condition, data = data)
summary(H1model_simple)
#no effect, p>0.05
# In an exploratory analysis, we will test whether this is due to participants in the powerless condition writing longer arguments by controlling for the number of words.
H1_wordcount_model <- lm(rating ~ condition + wordcount, data = data)
summary(H1_wordcount_model)
#fixed effect of word count is significant (p < 0.001), so longer arguments are associated with higher ratings.
#RQ2:Do powerful participants rate their arguments as better than those of powerless participants relative to the ratings of the independent raters?
RQ2_model<-lm(self_rating ~ condition * rating + other_rating, data = data)
summary(RQ2_model)
#no effect
#RQ3:Do powerful participants rate the other player’s arguments lower than powerless participants, relative to the independent raters?
RQ3_model<-lm(self_rating ~ condition * rating + other_rating, data = data)
summary(RQ3_model)
#no effect
#explorative analysis: power ~ gender 
gender_power <- lm(power ~ gender * condition, data = data)
summary(gender_power)
