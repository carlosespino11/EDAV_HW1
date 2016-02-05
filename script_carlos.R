library("devtools")
install_github("carlosespino11/ggthemes")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggthemes)
library(manipulate)
library(grid)
library(gridExtra)
library(tidyr)

setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW1/EDAV_HW1/")
source("utils.R")


survey = read.csv("Survey+Response.csv") %>% clean_data()


ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor), fill=reorder_size(gender)))+  
  scale_fill_manual(values= c("#107FC9","#FE4365","grey"), "gender") +
  labs(title="text editor by gender", x = "text editor") +
  theme_fivethirtyeight()


ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor))) + facet_grid(program~.) +
  theme_fivethirtyeight() +  
  labs(title="text editor by program", x = "editor")

ggplot(survey)+ aes(y = number_tools, x = program) + geom_boxplot(fill="grey")+ 
  stat_summary(fun.data =give.n, geom = "text" ) +
  theme_fivethirtyeight() +  
  labs(title="# of tools by program", y = "tools")

ggplot(filter(survey, gender != "Unknown")) + aes(y = number_tools, x = reorder_size(gender),fill=reorder_size(gender)) + 
  geom_boxplot()+
  theme_fivethirtyeight() + 
  scale_fill_manual(values= c("#107FC9","#FE4365"), "gender")+
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
  theme_fivethirtyeight() + scale_fill_manual(values= c("#107FC9","#FE4365"), "gender")




manipulate(plot_exp_gender(var) ,
  var=picker("exp.Radvanced", "exp.Rgraphics","exp.documentation", "exp.Matlab","exp.Github"))


grid.arrange(plot_exp_gender("exp.Radvanced"), plot_exp_gender("exp.Rgraphics"), 
             plot_exp_gender("exp.documentation"), plot_exp_gender("exp.Matlab"),
             plot_exp_gender("exp.Github"), ncol=3)




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
  theme_fivethirtyeight() + scale_fill_manual(values= c("#FE4365","#107FC9"), "gender")+ 
  facet_wrap(~language,ncol = 3)
exp_gender
ggsave("exp_gender.png",exp_gender, scale = 1.7)
