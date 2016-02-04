#data clean
setwd("~/Desktop/EDAV/EDAV_HW1")
survey <- read.csv("Survey+Response.csv")

#naming vars and removing empty columns
names(survey) <- c("waitlist","program","tools","exp.Rmodeling","b5","b6","b7","b8","b9","b10","b11","gender","primaryeditor","exp.Rgraphics","exp.Radvanced","exp.documentation","exp.Matlab","exp.Github","b19","b20","b21","b22","b23","b24","b25","b26","b27","b28","b29","b30","b31","b32","b33","b34","b35","b36","b37","b38")
survey <- survey[,c(-5:-11,-19:-38)]

#dummy variables for each language/tool in tools
tooldummies = c()
toolList <- c("Github","Excel","SQL","RStudio","ggplot2","shell", "C/C","Python","LaTeX","(grep)","Sweave/knitr","XML","Web: html css js","dropbox","google drive","SPSS","Stata")
for(t in toolList){
  tooldummies <- cbind(tooldummies,grepl(t,survey$tools))
}
tooldummies <- cbind(tooldummies,(grepl("R,",survey$tools)==TRUE | (grepl("R",survey$tools)==TRUE & grepl("RStudio",survey$tools)==FALSE)))
colnames(tooldummies) <- c("GitHub","Excel","SQL","RStudio","ggplot2","shell", "C","Python","LaTeX","grep","Sweave","XML","Web","dropbox","googledrive","SPSS","Stata","R")
survey <- cbind(survey,tooldummies)

#quick summary info
summary(survey)
par(las = 2)
par(mar=c(5,5,5,5))
barplot(apply(survey[,12:29],2,mean),ylab = "Proportion of Class",ylim = c(0,1))
library(corrplot)
corrplot(cor(survey[,12:29]),method='square')

# Reset the graphics
dev.off()

# weak negative correlations between stata and github
# strong positive correlation between r and rstudio (of course)
# strong positive correlation between dropbox and googledrive
# strong positive correlation between shell and github, SQL, grep, SML, Web
# medium positive correlation between web and sweave
# no correlation between r and python
# no correlation between r and excel
# no correlation between c and r
# no correlation between xml and stata

### Word Cloud
library(tm)
library(SnowballC)
library(wordcloud)

# Create a corpus
surveyCorpus <- Corpus(VectorSource(survey$tools))

# Convert corpus to a plain text document
surveyCorpus <- tm_map(surveyCorpus, PlainTextDocument)

# Remove all punctuation and stopwords
surveyCorpus <- tm_map(surveyCorpus, removePunctuation)
surveyCorpus <- tm_map(surveyCorpus, removeWords, stopwords('english'))

# Perform stemming
surveyCorpus <- tm_map(surveyCorpus, stemDocument)

# Plot the wordcloud
wordcloud(surveyCorpus, max.words = 100, random.order = FALSE)


### Barplot of gender with ggplot2

library(ggplot2)

attach(survey)
plot(gender)

# TODO: Remove missing value and 'doesn't matter'

c <- ggplot(survey, aes(factor(gender)))

# By default, uses stat="bin", which gives the count in each category
c + geom_bar() + ggtitle('Gender Composition of STAT 4701') + labs(x=' ', y='Number of Students')


### Exploration

pairs(survey)
pairs(survey[1:5])

# How big was the waitlist?
plot(waitlist, main='Waitlist')

# Which programs are most represented?
par(mfrow=c(1,1), oma=c(5,1,0,0))
plot(program, las=2)

dev.off()

# With which tools do students feel most confident?
par(mfrow=c(3, 2))
plot(exp.Rmodeling, main='R Modeling')
plot(exp.Rgraphics, main='R Graphics')
plot(exp.Radvanced, main='R Advanced')
plot(exp.documentation, main='Documentation')
plot(exp.Matlab, main='Matlab')
plot(exp.Github, main='GitHub')

dev.off()

# Note: percentages would be better for this
#       it'd be better to put "None" first too. 

# Which fields have the most experts?
summary(exp.Rmodeling)
summary(exp.Rgraphics)
summary(exp.Radvanced)
summary(exp.documentation)
summary(exp.Matlab)
summary(exp.Github)

# Notes:
#   R modeling has the most experts (11) (followed by documentation with 6)
#   R modeling has the most confident (60) (followed by R graphics with 34)
#     This is double the average confidence for the other 5 categories
#   The category in which the largest amount of people have no experience is Matlab,
#     followed by documentation. This is interesting, because documentation also
#     has the 2nd most amount of experts. It's like this is saying, if you do it
#     you do it well. Or you don't do it at all.
#     

# Notes:
#   Almost half of the class has a little experience in each field

# Most popular text editor?
plot(primaryeditor, las=2)
# TODO: merge sublime, textwrangler, ipython



## Ideas:
#   - explore the gender theme further
#       - could compare to dataset of gender composition in all graduate courses, in STEM graduate courses at Columbia
#   - R Markdown
#       - simple, nice looking graphics. basically, a report of our class. (lacking in creativity, no interaction)




