library(devtools)

# Run if you don't have ggthemes installed
#install_github("carlosespino11/ggthemes")

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
genderTech <- read.csv('gender-tech.csv')


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
  labs(title="Text Editor by Program", x = "editor")

ggplot(survey)+ aes(y = number_tools, x = program, fill = program) + geom_boxplot()+ 
  stat_summary(fun.data =give.n, geom = "text" ) +
  theme_fivethirtyeight() + 
  theme(legend.position="none")+
  scale_fill_tableau()+
  labs(title="# of tools by program", y = "tools")
  
exp_by_program  = ggplot(survey)+ aes(y = experience_programming, x =program, fill = program) + geom_boxplot()+ 
    stat_summary(fun.data =give.n, geom = "text" ) +
    theme_fivethirtyeight() +  
    theme(legend.position="none")+
    scale_fill_tableau()+
    labs(title="Experience by program", y = "experience", x="program")
exp_by_program  
ggsave("exp_by_program.png", exp_by_program)

exp_tools= ggplot(survey) + aes(y = experience_programming, x = number_tools) +
  geom_point()+ geom_smooth(method = "lm")+ 
  theme_fivethirtyeight() +  
  scale_fill_tableau() + 
  labs(title="experience vs tools", x = "tools", y="experience")
exp_tools
ggsave("exp_tools.png", exp_tools)


exp_dist_program = ggplot(survey, aes(x = experience_programming))  + geom_density( aes(y=..count..))  + geom_bar(aes(fill=program),alpha = .7)+
  facet_wrap(~program,ncol=3)+ 
  theme_fivethirtyeight() +  
  scale_fill_tableau() + 
  theme(legend.position="none")+
  labs(title="Experience Distribution by Program", x = "experience")
exp_dist_program
ggsave("exp_dist_program.png", exp_dist_program)

exp_dist = ggplot(survey, aes(x = experience_programming))  + geom_density( aes(y=..count..))  + geom_bar(alpha = .7)+
  theme_fivethirtyeight() +  
  scale_fill_tableau() + 
  labs(title="Experience Distribution", x = "experience")
exp_dist
ggsave("exp_dist.png", exp_dist)


tool_dist_program = ggplot(survey, aes(x = number_tools))  + geom_density( aes(y=..count..))  + geom_bar(aes(fill=program),alpha = .7)+
  facet_wrap(~program,ncol=3)+ 
  theme_fivethirtyeight() +  
  scale_fill_tableau() + 
  theme(legend.position="none")+
  labs(title="# of Tools Distribution by Program", x = "tools")

tool_dist = ggplot(survey, aes(x = number_tools))  + geom_density( aes(y=..count..))  + geom_bar(alpha = .7)+
  theme_fivethirtyeight() +  
  scale_fill_tableau() + 
  labs(title="# of Tools Distribution", x = "tools")
tool_dist
ggsave("tool_dist.png", tool_dist)

tools_gender_density = ggplot(survey, aes(x = number_tools)) +
  geom_density(data = dplyr::filter(survey,gender == "Male"), aes(fill=gender), alpha = .5) +
  geom_density(data = dplyr::filter(survey,gender == "Female"), aes(fill=gender,y=..density..*(-1)), alpha = .5) +
  coord_flip() + theme_fivethirtyeight() + scale_fill_tableau(name = "gender")+
  labs(title="# of Tools by Gender Distribution", x = "tools")
tools_gender_density

ggsave("tools_gender_density.png", tools_gender_density)

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

class_dist = ggplot(data=survey,aes(x=program,fill=gender)) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10)) + 
  coord_flip()+
  theme_fivethirtyeight() + 
  scale_fill_tableau(name="gender") + 
  labs(title = "Distribution of the Class by Program and Gender")
ggsave("class_dist.png",  class_dist)

ggplot(survey)+ aes(y = number_tools, x = program, fill = program) + geom_boxplot()+ 
  stat_summary(fun.data =give.n, geom = "text" ) +
  theme_fivethirtyeight() +  
  scale_fill_tableau()

# manipulate(plot_exp_gender(var) ,
#   var=picker("exp.Radvanced", "exp.Rgraphics","exp.documentation", "exp.Matlab","exp.Github"))


experience = survey %>% gather( language,experience,  starts_with("exp.")) %>% 
  mutate(experience = factor(experience)) 

levels(experience$experience) = c("None" ,"A little",  "Confident", "Expert")
levels(experience$language) = c("R Modeling", "R Graphics", " R advanced", 
                                "documentation", "Matlab", "Github")




exp_gender = ggplot(experience,aes(x=experience, fill = gender)) + 
  geom_bar(data = dplyr::filter(experience, gender=="Female")) + 
  geom_bar(data = dplyr::filter(experience, gender=="Male"), aes(y=..count..*(-1))) + 
  # scale_y_continuous(breaks=seq(-40,40,10)) +
  # labs(title=var)+
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau(name = "gender")+ 
  facet_wrap(~language,ncol = 3) +
  labs(title="Experience by Program")+
  theme(axis.title.y  = element_blank())
exp_gender
ggsave("exp_gender.png",exp_gender, scale = 1.7)

tools_exp = ggplot(experience) + geom_boxplot(aes(x = experience, y = experience_programming), fill = "lightgrey")  + 
  labs(title="Tool Counts vs Experience Level", y = "# of programs")+theme_fivethirtyeight()+
  facet_wrap(~language,ncol = 3)
ggsave("tools_exp.png", tools_exp)

freq_editor = survey %>% group_by(primaryeditor, program) %>% summarize(Freq = n())
n1 <- nPlot(Freq ~ primaryeditor, group = "program", data = freq_editor, 
            type = 'multiBarHorizontalChart')
n1

avg_experience = experience %>% mutate(experience = as.numeric(experience)-1)%>%
  group_by(language, program) %>% summarize( experience = sum(experience))


ggplot(filter(avg_experience, gender!="Unknown")) + geom_bar(stat="identity",aes(x = language, y = experience,fill = gender),position = "dodge",alpha = .7)+ 
  labs(title="experience by gender", x = "experience") +
  theme_fivethirtyeight()+
  scale_fill_tableau()+
  coord_polar() 

library(reshape)

genderTech <- genderTech[order(genderTech$percentWomen),]

genderTech <- melt(genderTech, id=c('Company'))

genderTech$Company <- factor(genderTech$Company, 
                             levels=rev(c('STAT 4701: EDAV','eBay', 'Apple', 'Pinterest', 'Google',
                                          'LinkedIn', 'Yahoo', 'Facebook', 'Twitter')))

gender_tech = ggplot(data = genderTech, aes(x = Company, y = value, fill = variable)) + coord_flip() +
  geom_bar(stat = "identity") + 
  labs(title='Gender in Tech Workforce', x='', y='') +
  scale_y_continuous(label=function(x) {return(paste(x,'%'))}) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_tableau(labels=c('Women', 'Men')) +
  theme_fivethirtyeight() 
ggsave("gender_tech.png", gender_tech)
