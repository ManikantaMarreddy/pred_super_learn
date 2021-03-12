

library(readr)

students_data <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
print("Data successfully imported")
head(students_data)  #it will print the first 6 rows of data

summary(students_data) # to fine mean,median,mode,std

is.na(students_data)  #to check the null values
is.null(students_data) #it will give only single statement (true/false) for the entire table


boxplot(students_data$Hours) #boxplot for hours
boxplot(students_data$Scores)#boxplot for scores
boxplot(students_data) #boxplot for entire table(data)

#ggplot is used for scatter plot b/w x and y
#geom_point to used to modify the size,color,shape,.. of the points
ggplot(data=students_data, mapping = aes(x = Hours,y=Scores)) + geom_point(color = "blue" , size =2 )

#students_data %>% ggplot(aes(x=Hours, y=Scores)) +geom_point(color = "blue", size = 2)


#The main point of using the seed is to be able to reproduce a particular sequence of 'random' numbers
set.seed(234) 

#data partition into training and testing data sets using ceratedatapartition
sample_index<- createDataPartition(students_data$Scores,p=0.80,list=FALSE)
sample_index
training_set <- students_data[sample_index,]
test_set <- students_data[-sample_index,]

training_set
test_set

#bulding a model(training the model)
model = lm(formula = Scores~Hours , data = training_set)
model

#scatter plot and regression line on training data set
training_set %>% ggplot(aes(x=Hours,y<-Scores)) + geom_point(color = "red",size = 2)+ 
  geom_abline(slope = model$coefficients[2],intercept = model$coefficients[1])


#prediction on test data set
prediction <- predict(model,test_set)
prediction

#scatter plot and regression line on testing data set
test_set %>% ggplot(aes(x=Hours,y<-Scores,colour ="green"))+
  geom_point(color = "blue",size = 2)+ 
  geom_line(aes(x=Hours,y=prediction),color ="red")

#comparing actual vs scores
data.frame(actual = test_set$Scores , predicted = prediction)

#finding accuracy percentage of the model
R2(prediction,test_set$Scores)


no_of_hours = 1.5
predicted_score = model$coefficients[1] + model$coefficients[2]*no_of_hours
paste("predicted score of student studying 9.25 hrs/day ="  , predicted_score)
