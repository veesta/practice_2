library(tidyverse)

#create an analytic data file 

raw_data <- read_csv(file="raw_data.csv")

#fix missing data 

raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999"))

#check any missing variables - seems to be all there
#View (raw_data)

#use str(raw_data) to see if sex is already a categorical variable, it isn't, make it one 

categorical_variables <- select(raw_data, sex)

categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1, "Female"=2)


#what does this do: 
#sex <- categorical_variables$sex

#View (categorical_variables)

#group the adjectives under their titles and then check for out of range values

pa_affect_items <- select (raw_data, afraid, angry, anxious, ashamed)
na_affect_items <- select (raw_data, delighted, elated, enthusiastic, excited)

#make variables even if there is only one column of data (Note capitals)
#parco wrote it like this: extraversion <- raw_data$Extraversion
extraversion <- select (raw_data, Extraversion)
neuroticism <- select (raw_data, Neuroticism)

#check for out of range items (all adjectives from 0 to 3), extra/neuroto (0 to 24)

psych::describe(pa_affect_items)
psych::describe(na_affect_items)
psych::describe(extraversion)
psych::describe(neuroticism)

#na_affect - anxious has a 5 value, so fix it

is_bad_value <- na_affect_items<0 | na_affect_items>3
na_affect_items[is_bad_value] <- NA

psych::describe(na_affect_items)

#if there were any reverse keyed items youd fix them here 

#obtain a scaled score - calc mean for each person based on items 
#this is where you relable to "this data file should have the following final variable names" 

pos_affect <- psych::alpha(as.data.frame(pa_affect_items), check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(na_affect_items), check.keys=FALSE)$scores

#no need to calculate scale scores for extraversion and neuroticism 
#extraversion and neuroticism are not made up of other varaibles, so you don't need to get a mean for them
#pos_affect for exp. is made up of 4 different adjectives, so you need to average them and get a score 

analytic_data <- cbind(categorical_variables,pos_affect,neg_affect,extraversion,neuroticism)

#save dataas RData file to preserve labels for readers

save(analytic_data, file="practice.quiz.1.analytic_data.RData")

#save as excel csv file 

write_csv(analytic_data,path="practice.quiz.1.analytic_data.csv")

------
#create male and female subsets for analytic data

#to use select/filter commands, you need to download library (dplyr)

library(dplyr)

#option1: 

analytic_data_male <- cbind(pos_affect,neg_affect,neuroticism,extraversion)

select(analytic_data,pos_affect,neg_affect,neuroticism,extraversion)

analytic_data_male <- filter(analytic_data,sex=="Male")

View(analytic_data_male)


#option 2: 

#filter the data and save it then try to remove the sex column with cbind 

analytic_data_male <- filter(analytic_data,sex=="Male")

write_csv(analytic_data_male,path="hopefully.saves.male.csv")

#follow instructions on correlations package
male.data <- read_csv("hopefully.saves.male.csv")

#says could not find function <-<- 
analytic_data_male <- select=c(male.data,pos_affect,neg_affect,neuroticism,extraversion)

analytic_data_male <- cbind(pos_affect,neg_affect,neuroticism,extraversion)

select=c(male.data,pos_affect,neg_affect,neuroticism,extraversion)

View(analytic_data_male)
#still spits out 3800 entries -- UGH 

#---------------------------------------------------------------------------------------

#ATTEMPTS: OCT 14: 
#first select the data you want to use 

#my.data <- read_csv("practice.quiz.1.analytic_data.csv")

#entering the following brings error that says all select inputs must resolve to integer column position 
#analytic_data <- select(my.data,pos_affect,neg_affect,neuroticism,extraversion)

#works but has no function 
#select=c(my.data, pos_affect,neg_affect,neuroticism,extraversion)
#analytic_data <- cbind(categorical_variables,pos_affect,neg_affect,extraversion,neuroticism)

#filter(raw_data,sex=="Male") 

#analytic_data_male <- select(my.data,pos_affect,neg_affect,extraversion,neuroticism)
  
#cbind(categorical_variables,pos_affect,neg_affect,extraversion,neuroticism)
#                      select=c(pos_affect,neg_affect,extraversion,neuroticism)

#sex_female <- filter(raw_data,sex=="Female")

#select=c(pos_affect,neg_affect,extraversion,neuroticism)

#analytic_data_male <- cbind(filter(raw_data,sex=="Male"),select=c(pos_affect,neg_affect,extraversion,neuroticism)analytic_data_male <- cbind(filter(raw_data,sex=="Male"),select=c(pos_affect,neg_affect,extraversion,neuroticism))

#doesnt work
#analytic_data_male <- cbind(filter(raw_data,sex=="Male"), pos_affect,neg_affect,extraversion,neuroticism)

#View(analytic_data_male)

#this is how you use the filter function... filter (where is it saved under, whats the title == specifics)

#filter(categorical_variables, sex == "Male")

#this does not continue onto the next line, the above line needs to be incorporated below somehow


#the following works to filter the data for sure, but not sure about selecting the data  

#analytic_data_male <- filter(categorical_variables, sex == "Male")

 #                     select=c(pos_affect,neg_affect,extraversion,neuroticism)
                      
                      
#this is from the apa tables handout
                      #goggles.men <‐ filter(goggles,gender=="Male")
                      #goggles.women <‐ filter(goggles,gender=="Female")
                      #apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.men,filename="Table9_APA.doc",table.number = 9)
                      #apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.women,filename="Table10_APA.doc",table.number = 10)

#View(analytic_data)

#View(analytic_data_male)
         
       
#the following doesn't work                                               
#analytic_data_male <- cbind(filter(categorical_variables, sex == "Male")
                            
#select=c(pos_affect,neg_affect,extraversion_items,neuroticism_items)
            

#ideas from online: 
#analytic_data <- (categorical_variables,pos_affect,neg_affect,extraversion_items,neuroticism_items)

#filter(mammals, species == "Balaena mysticetus")









#--------------------------------------------- OTHER STEPS FROM OVERALL DATA ------------------------
#create apa correlation tables for overall - load library(apaTables)

library(apaTables)

#goggles.men <‐ filter(goggles,gender=="Male")
#goggles.women <‐ filter(goggles,gender=="Female")
#apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.men,filename="Table9_APA.doc",table.number = 9)
#apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.women,filename="Table10_APA.doc",table.number = 10)

apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)

#create a graph based on the data that corresponds to each table using psych::pairs.panels, need tidyverse
library(tidyverse)

#Create a correlation graph
#the following 2 give the same results 
psych::pairs.panels(analytic_data)

#psych::pairs.panels(as.data.frame(analytic_data),lm=TRUE)

