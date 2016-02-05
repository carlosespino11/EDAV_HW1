reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

clean_data <- function(survey){
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
  return(survey)
}

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


plot_exp_gender = function(var) {
  ggplot(data=survey,aes_string(x=var, fill = "gender")) + 
    geom_bar(data = dplyr::filter(survey, gender=="Female")) + 
    geom_bar(data = dplyr::filter(survey, gender=="Male"), aes(y=..count..*(-1))) + 
    # scale_y_continuous(breaks=seq(-40,40,10)) +
    labs(title=var)+
    coord_flip()+
    theme_fivethirtyeight() + scale_fill_manual(values= c("#107FC9","#FE4365"), "gender")
} 