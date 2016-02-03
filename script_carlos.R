library("devtools")
install_github("carlosespino11/ggthemes")

library(ggplot2)
library(ggthemes)
library(reshape)
library(dplyr)
library(ggthemes)
library(manipulate)

levels(survey$gender)
levels(survey$gender) <- c("doesn't matter", "doesn't matter", "he/him", "she/her")
levels(survey$gender) <- c("Unknown", "Male", "Female")

survey = survey %>% mutate(primaryeditor = factor(
                    ifelse(grepl("[Aa]tom",primaryeditor),
                           "Atom",
                    ifelse(grepl("[Ss]ublime",primaryeditor ),
                           "Sublime Text",
                    ifelse(grepl("[Vv]i",primaryeditor),
                           "vi/vim",
                    ifelse(grepl("[Ww]rangler",primaryeditor ),
                           "TextWrangler",
                    ifelse(grepl("[Rr][Ss]tudio",primaryeditor ),
                           "Rstudio",
                    ifelse(grepl("[Pp]ython | jupyter | ipynb",primaryeditor ),
                           "iPython",
                    ifelse(grepl("[Mm]ate", primaryeditor ),
                           "TextMate",
                    ifelse(grepl("[Ww]ebstorm", primaryeditor ),
                           "Webstorm",
                    ifelse(grepl("[Xx]code", primaryeditor ),
                          "Xcode",
                    ifelse(grepl("[Ss]tata", primaryeditor ),
                           "Stata",
                    ifelse(grepl("[Nn]otepad", primaryeditor ),
                            "notepad++", "Other"
                    )
                    )
                    )
                    )
                    )
                    )
                    )
                    )
                    )
                    )
)), number_tools = sapply(strsplit(as.character(tools),","), length)
)
# levels(survey$program) = c("Applied Math", "MS IDSE", "Cert IDSE", 
#                            "MS IDSE","MS IDSE","MS IDSE",
#                            "Other", "Other", "PhD. BMI",
#                            "QMSS", "QMSS","MS Stat")

levels(survey$program) = c("Other Masters", "MS IDSE", "Cert IDSE", 
                           "MS IDSE","MS IDSE","MS IDSE",
                           "Other Masters", "Other PhD", "Other PhD",
                           "QMSS", "QMSS","MS Stat")
ggplot(survey)+ geom_bar(aes(fill = GitHub, x = reorder_size(gender)), position = "fill")

ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor))) + facet_grid(.~gender)

ggplot(survey)+ geom_bar(aes(x = reorder_size(program))) 

ggplot(survey)+ geom_bar(aes(x = reorder_size(primaryeditor), fill=program)) + 
  theme_fivethirtyeight() + scale_fill_tableau()

ggplot(survey)+ geom_boxplot(aes(y = number_tools, x = program), position = "dodge")+
  theme_fivethirtyeight() + scale_fill_hc()+ labs(title="# of Tools by program")

ggplot(survey)+ geom_bar(aes(x= primaryeditor), position = "dodge")+ 
  facet_grid(number_tools~ primaryeditor)+
  theme_fivethirtyeight() + scale_fill_tableau()  



ggplot(data=survey,aes(x=number_tools,fill=gender)) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  # scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau()

ggplot(data=survey,aes(x=program,fill=gender)) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10)) + 
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau()

ggplot(data=survey,aes(x=exp.Rgraphics,fill=gender)) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10)) + labs(title="R graphics proficiency") + 
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau() 



ggplot(data=survey,aes(x=exp.Radvanced,fill=gender)) + 
  geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
  geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10)) + 
  coord_flip()+
  theme_fivethirtyeight() + scale_fill_tableau()
 )



manipulate(
  ggplot(data=survey,aes_string(x=var, fill = "gender")) + 
    geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
    geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
    # scale_y_continuous(breaks=seq(-40,40,10)) +
    labs(title=var)+
    coord_flip()+
    theme_fivethirtyeight() + scale_fill_tableau() ,
  
  var=picker("exp.Radvanced", "exp.Rgraphics","exp.documentation", "exp.Matlab","exp.Github"))

