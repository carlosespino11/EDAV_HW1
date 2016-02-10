library(devtools)

install_github("carlosespino11/ggthemes")
install_github('ramnathv/rCharts')

library(rCharts)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggthemes)
library(manipulate)
library(grid)
library(gridExtra)
library(tidyr)
library(lubridate)

setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW1/EDAV_HW1/")
source("utils.R")


survey = read.csv("Survey+Response.csv") %>% clean_data()

learn_interest = read.csv("learn_interest_google.csv")

learn_interest$Week = ymd(unlist(lapply(as.character(learn_interest$Week), function(x) strsplit(x," - ")[[1]][1])))

learn_interest_long = learn_interest%>% gather( term, count,  starts_with("learn")) %>% 
  mutate(year = year(Week), month = month(Week)) %>% group_by(year, month, term) %>% 
  summarize(count = sum(count) ) %>% mutate(date = ymd(paste(year,month,"01",sep="-")))


google_search_trend = ggplot(filter(learn_interest_long,year>=2005)) + geom_line(aes(x = date, y =count, color = term))+
  theme_fivethirtyeight() + 
  theme(axis.text.y  = element_blank(),axis.title.y  = element_blank())+
  scale_color_tableau() + 
  labs(title="Google search trends over time")

ggsave("google_search_trend.png", google_search_trend, scale= 1.5)

ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor), fill=reorder_size(gender)))+  
  scale_fill_tableau(name = "gender") +
  labs(title="text editor by gender", x = "text editor") +
  theme_fivethirtyeight()

ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor), fill=reorder_size(gender)), position="dodge")+  
  scale_fill_tableau(name = "gender") +
  coord_polar()+
labs(title="text editor by gender", x = "text editor") +
  theme_fivethirtyeight()


ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor))) + facet_grid(program~.) +
  theme_fivethirtyeight() +  
  labs(title="text editor by program", x = "editor")

ggplot(survey)+ aes(y = number_tools, x = program, fill = program) + geom_boxplot()+ 
  stat_summary(fun.data =give.n, geom = "text" ) +
  theme_fivethirtyeight() +  
  scale_fill_tableau()
  labs(title="# of tools by program", y = "tools")

ggplot(filter(survey, gender != "Unknown")) + aes(y = number_tools, x = reorder_size(gender),fill=reorder_size(gender)) + 
  geom_boxplot()+
  theme_fivethirtyeight() + 
  scale_fill_tableau(name = "gender")+
  labs(title="# of tools by gender", x = "gender", y = "tools")

# 
# ggplot(data=survey,aes(x=number_tools,fill=gender)) + 
#   geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
#   geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
#   # scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
#   coord_flip()+
#   theme_fivethirtyeight() + scale_fill_tableau()

ggplot(data=survey,aes(x=program,fill=reorder_size(gender))) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10)) + 
  coord_flip()+
  theme_fivethirtyeight() + 
  scale_fill_tableau(name="gender") + 
  labs(title = "Gender by program")




# manipulate(plot_exp_gender(var) ,
#   var=picker("exp.Radvanced", "exp.Rgraphics","exp.documentation", "exp.Matlab","exp.Github"))


experience = survey %>% gather( language,experience,  starts_with("exp")) %>% 
  mutate(experience = factor(experience, levels= c("None" ,"A little",  "Confident", "Expert")))
levels(experience$language) = c("R Modeling", "R Graphics", " R advanced", 
                                "documentation", "Matlab", "Github")


exp_gender = ggplot(experience,aes(x=experience, fill = gender)) + 
  geom_bar(data = dplyr::filter(experience, gender=="Female")) + 
  geom_bar(data = dplyr::filter(experience, gender=="Male"), aes(y=..count..*(-1))) + 
  # scale_y_continuous(breaks=seq(-40,40,10)) +
  # labs(title=var)+
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau(name = "gender")+ 
  facet_wrap(~language,ncol = 3)
exp_gender
ggsave("exp_gender.png",exp_gender, scale = 1.7)

freq_editor = survey %>% group_by(primaryeditor, program) %>% summarize(Freq = n())
n1 <- nPlot(Freq ~ primaryeditor, group = "program", data = freq_editor, 
            type = 'multiBarHorizontalChart')
n1

avg_experience = experience %>% mutate(experience = as.numeric(experience)-1)%>%
  group_by(gender, language) %>% summarize( experience = mean(experience))
ggplot(filter(avg_experience, gender!="Unknown")) + geom_bar(stat="identity",aes(x = language, y = experience,fill = gender),position = "dodge",alpha = .7)+ 
  labs(title="experience by gender", x = "experience") +
  theme_fivethirtyeight()+
  scale_fill_tableau()+
  coord_polar() 
