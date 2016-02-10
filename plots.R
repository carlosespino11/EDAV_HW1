library(devtools)

install_github("carlosespino11/ggthemes")
library(ggthemes)

library(ggplot2)
library(reshape)

setwd("~/Desktop/EDAV/EDAV_HW1")
survey <- read.csv("survey_clean.csv")
attach(survey)

head(survey)

## Gender Plot
c <- ggplot(survey, aes(factor(gender)))
c + geom_bar() + ggtitle('Gender Composition') + labs(x='', y='Count')

## Editor Plot
ggplot(survey) + geom_bar(aes(x = primaryeditor, fill=program)) + coord_flip() +
  labs(title='Text Editor by Program', x= 'Editor', y='')

## Gender in Tech Plot
genderTech <- read.csv('gender-tech.csv')
attach(genderTech)

# Sort by Percent Women, ascending
genderTech <- genderTech[order(percentWomen),]

# Melt the gender columns
genderTech <- melt(genderTech, id=c('Company'))

genderTech$Company <- factor(genderTech$Company, 
                            levels=rev(c('STAT 4701: EDAV','eBay', 'Apple', 'Pinterest', 'Google',
                                     'LinkedIn', 'Yahoo', 'Facebook', 'Twitter'))) 

ggplot(data = genderTech, aes(x = Company, y = value, fill = variable)) + coord_flip() +
  geom_bar(stat = "identity") + 
  labs(title='Gender in Tech Workforce', x='', y='') +
  scale_y_continuous(label=function(x) {return(paste(x,'%'))}) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_discrete(labels=c('Women', 'Men')) +
  theme_fivethirtyeight() +   
  scale_color_tableau()

#reorder(Company, value)

## playing
ggplot(data=survey, aes(x=program, y=waitlist)) + geom_point()
ggplot(data=survey, aes(x=program, y=exp.Rgraphics)) + geom_point()
ggplot(data=survey, aes(x=exp.Rgraphics)) + geom_bar()

qplot(x=Var1, y=Var2, data=melt(cor(attitude)), fill=value, geom="tile")
