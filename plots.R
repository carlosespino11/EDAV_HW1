library(ggplot2)
library(reshape)

setwd("~/Desktop/EDAV/EDAV_HW1")
survey <- read.csv("survey_clean.csv")
attach(survey)

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

ggplot(data = genderTech, aes(x = Company, y = value, fill = variable)) + coord_flip() +
  geom_bar(stat = "identity") + 
  labs(title='Gender in Tech Workforce', x='', y='Percent') +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_discrete(labels=c('Women', 'Men'))


