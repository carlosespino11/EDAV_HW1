#data clean
setwd("/Users/Ryan/Desktop/EDAV/")
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
