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
extraversion_items <- select (raw_data, Extraversion)
neuroticism_items <- select (raw_data, Neuroticism)

#check for out of range items (all adjectives from 0 to 3), extra/neuroto (0 to 24)

psych::describe(pa_affect_items)
psych::describe(na_affect_items)
psych::describe(extraversion_items)
psych::describe(neuroticism_items)

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

analytic_data <- cbind(categorical_variables,pos_affect,neg_affect,extraversion_items,neuroticism_items)

#save data 

save(analytic_data, file="practice.quiz.1.analytic_data.RData")


------
#create male and female subsets for analytic data

#to use select/filter commands, you need to download library (dplyr)

library(dplyr)

#this is how you use the filter function... filter (where is it saved under, whats the title, specifics)

filter(categorical_variables, sex == "Male")

#this does not continue onto the next line, the above line needs to be incorporated below somehow


#the following works to filter the data for sure, but not sure about selecting the data  

analytic_data_male <- filter(categorical_variables, sex == "Male")

                      select=c(pos_affect,neg_affect,extraversion_items,neuroticism_items)


#View(analytic_data)

#View(analytic_data_male)
         
       
#the following doesn't work                                               
analytic_data_male <- cbind(filter(categorical_variables, sex == "Male")
                            
                            select=c(pos_affect,neg_affect,extraversion_items,neuroticism_items)
            

#ideas from online: 
#analytic_data <- (categorical_variables,pos_affect,neg_affect,extraversion_items,neuroticism_items)

#filter(mammals, species == "Balaena mysticetus")



#create apa correlation tables for overall, females, males and i'm guessing all the variables
#needs to be a numerical value for it to work, can't be a factor? 

library(apaTables)
apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)

#lets try males first - filter the category 

analytic.data.sex.is.male <- filter(analytic_data, sex==male)
cor.test(x=analytic.data.sex.is.male$)

#calculate correlations for several subgroups

cor.test(x=)