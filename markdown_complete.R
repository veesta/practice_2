## Creating analytic data file

library(tidyverse)
library(apaTables)

#load data
raw_data <- read_csv(file="raw_data.csv")

str(raw_data)
# View(raw_data)


#use str(raw_data) to see if sex is already a categorical variable, it isn't, make it one 

raw_data$sex <- as.factor(raw_data$sex)
levels(raw_data$sex) <- list("Male"=1, "Female"=2)


#raw_data$sex <- as.factor(raw_data$sex)
#levels(raw_data$sex) <- list("Male"=1, "Female"=2)


#group the adjectives under their titles and then check for out of range values
#select raw data for ALL VARIABLES 

sex <- select(raw_data, sex)
neg_affect_items <- select(raw_data, afraid, angry, anxious, ashamed)
pos_affect_items <- select(raw_data, delighted, elated, enthusiastic, excited)
Neuroticism <- select(raw_data, Neuroticism)
Extraversion <- select(raw_data, Extraversion)


#check for out of range items (all adjectives from 0 to 3), extra/neuroto (0 to 24)

psych::describe(neg_affect_items)
is_bad_value <- neg_affect_items<0 | neg_affect_items>3
neg_affect_items[is_bad_value] <- NA

# View(neg_affect_items)

psych::describe(pos_affect_items)
psych::describe(Neuroticism)
psych::describe(Extraversion)

#if there were any reverse keyed items youd fix them here 


#obtain a scaled score - calc mean for each person based on items 
#this is where you relable to "this data file should have the following final variable names" 


## To obtain scale scores:
pos_affect <- psych::alpha(as.data.frame(pos_affect_items),check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(neg_affect_items),check.keys=FALSE)$scores

analytic_data <- cbind(sex,pos_affect,neg_affect,Neuroticism, Extraversion)

# View(analytic_data)

write_csv(analytic_data,path="analytic_data.csv")

str(analytic_data)

analytic_data

# View(analytic_data)


## Create 3 filtered data sets
#title where you want the filter to be saved 
#title again where you want the filtered data to be saved 
analytic_data.male <- filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data.male, pos_affect, neg_affect, Neuroticism, Extraversion)


analytic_data.female <- filter(analytic_data, sex=="Female")
analytic_data_female <- select(analytic_data.female, pos_affect,neg_affect, Neuroticism, Extraversion)

#view data to make sure there is no sex column and the total entries are less showing its sorted by male and female 
#View(analytic_data_male)
#View(analytic_data_female)

## Save data sets
write_csv(analytic_data_male,path="analytic_data_male.csv")
write_csv(analytic_data_female,path="analytic_data_female.csv")

#load apa tables
library(apaTables)

## Create 3 correlation tables
apa.cor.table(analytic_data,filename="Table_1_Overall.doc",table.number=1)
apa.cor.table(analytic_data_male,filename="Table_2_Male.doc",table.number=2)
apa.cor.table(analytic_data_female,filename="Table_3_Female.doc",table.number=3)

## Create 3 correlation figures
#not working... 
psych::pairs.panels(as.data.frame(analytic_data),lem=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_male),lem=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_female),lem=TRUE)

## Create Neuroticism Histogram
my.hist.female.neur <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist.female.neur <- my.hist.female.neur + geom_histogram(aes(y=..count..), binwidth = .30, fill="black",color="black")
my.hist.female.neur <- my.hist.female.neur + labs(title="Female Neuroticism Histogram",x="Neuroticism Rating",y="Frequency")
my.hist.female.neur <- my.hist.female.neur + coord_cartesian(xlim=c(0,25), ylim = c(0,175))
my.hist.female.neur <- my.hist.female.neur + theme_classic()
my.hist.female.neur <- my.hist.female.neur + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                     axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.female.neur <- my.hist.female.neur + scale_y_continuous(breaks = seq(0,175,by=25), expand = c(0,0))
print(my.hist.female.neur)
ggsave(filename="Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist.female.neur, width=6,height=6, units="in")

## Create Neg_affect Scatter Plot
my.hist.f.neg <- ggplot(analytic_data_female,aes(neg_affect))
my.hist.f.neg <- my.hist.f.neg + geom_histogram(aes(y=..count..), binwidth = .10, fill="black",color="black")
my.hist.f.neg <- my.hist.f.neg + labs(title="Female Negative Affect Histogram",x="Negative Affect Rating",y="Frequency")
my.hist.f.neg <- my.hist.f.neg + theme_classic()
my.hist.f.neg <- my.hist.f.neg + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                   axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#change where the bins begin and end (at 0 to 3.5) and how often the axes are width (.5)
my.hist.f.neg <- my.hist.f.neg + scale_x_continuous(breaks = seq(0,3.5,by=.5))

#get the graph to touch the X axis
my.hist.f.neg <- my.hist.f.neg + scale_y_continuous( expand = c(0,0))
print(my.hist.f.neg)

ggsave(filename="Figure_5_Neuroticism_Histogram_Female.tiff", plot=my.hist.f.neg, width=6,height=6, units="in")

## Create NA-Neur Scatter Plot
my.scatter <- qplot(x=neg_affect,y=Neuroticism,data=analytic_data_female)

#add a regression line, without the confidence interval se = false 
#correlations in r page 6 

my.scatter <- my.scatter + geom_smooth(method = "lm", se = FALSE, color='black')

my.scatter <- my.scatter + labs(title="Female Negative Affect vs. Neuroticism",x="Negative Affect Rating",y="Neuroticism Rating")
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

print(my.scatter)
ggsave(filename="Figure_6_NA_Neuroticism_Scatter.tiff", plot=my.scatter, width=6,height=6, units="in")

