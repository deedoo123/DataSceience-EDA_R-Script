" Project on “Census Income” Dataset In this project, you are going to work on the The
Census Income data set from the UCI Machine Learning Repository that contains the income information
for over 48,000 individuals taken from the 1994 US census. "

# Problem Statement: 
"In this project, initially you need to preprocess the data and then develop an understanding of different 
features of the data by performing exploratory analysis and creating visualizations"



#Reading a data set
read.csv("D:\\R\\FinalProject\\census-income_ _1_ (1).csv")-> census_ds
View(census_ds)

#1.Data Pre-processing
#1.a)Replacing all the missing values with NA
census_ds[census_ds == ' ?']<-NA

#1.b)Remove all the row that contains NA values
##creating function to count number of NA values
NA_Count <-function (x) sapply(x, function(y) sum(is.na(y)))
NA_Count(census_ds)

na.omit(census_ds)->census_ds
View(census_ds)

#1.c)Remove all the white spaces from column
##importing required libraries
library(dplyr)
library(stringr)

census_ds %>%
  mutate_if(is.character, str_trim) -> census_ds
View(census_ds)

#2.Data Manipulation##############------------------------------------

#a. Extract the education column
select(census_ds, 4) ->census_ed
View(census_ed)

#b Extract all the columns from age to relationship
select(census_ds, 1:8) ->census_seq
View(census_seq)

#c Extract column numbers 5,8,11
select(census, 5,8,11) ->census_col
View(census_col)

#d Extract all the male employee who work in state govt
census_ds$workclass == 'State-gov' & census_ds$sex == 'Male' ->male_gov
subset(census_ds, male_gov == T) ->male_gov
View(male_gov)

#E  Extract all the 39 year old who either have a bachelor's degree or who are native of United States and store the result in “census_us”. 
subset(census_ds, census_ds$age == 39 & (census_ds$education == "Bachelors" | census_ds$native.country == 'United-States'))->census_us 
View(census_us)

#F Extract 200 random rows from the “census” data frame and store it in “census_200”
sample_n(census_ds, 200) ->census_200
View(census_200)

#G  Get the count of different levels of the “workclass” column. 
table(census_ds$workclass)
census_ds %>% count(workclass)

#H Calculate the mean of “capital.gain” column grouped according to “workclass”
census_ds %>% group_by(workclass) %>% summarise(mean(capital.gain))


#3.Data Visualization#############--------------------------------------
#loading the required packages
library(ggplot2)
library(plotly)

#A.Bar plot
ggplot(data = census_ds ,aes(x=relationship,fill = relationship)) + geom_bar(position = "dodge")+ facet_grid(~sex) +
  ggtitle ("Distribution of relationship by sex")

#B.Histogram
ggplot(census_ds,aes(x=age, fill = X)) + geom_histogram(bins = 50) + labs(title = "Distribution of Age") +
  theme_bw()

#C.Scatter plot
ggplot(census_ds,aes(y=hours.per.week,x=capital.gain,col=X)) + geom_point(size=2, alpha=0.4 ) +
  ggtitle("Capital Gain VS Hours per week by income")

#D.BOX plot
ggplot(census_ds, aes(x=education,y=age,fill=sex)) + geom_boxplot() +
  ggtitle("BOX-plot of Age by Education and Sex")

