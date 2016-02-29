
library(dplyr)

setwd("C:/Users/0010083/Documents/R/clean")

#read files

#read master data tables
activity_labels<-read.table("activity_labels.txt")
features<-read.table("features.txt")

#read test tables
subject_test<-read.table("test/subject_test.txt")
x_test<-read.table("test/X_test.txt")
y_test<-read.table("test/y_test.txt")

#read train tables
subject_train<-read.table("train/subject_train.txt")
x_train<-read.table("train/X_train.txt")
y_train<-read.table("train/y_train.txt")


#1.	Merges the training and the test sets to create one data set.
x_merged<-rbind(x_test,x_train)

#2.	Extracts only the measurements on the mean and standard deviation for each measurement. 

features_mini_index<-grep("mean|std",features[,2])
x_named<-x_merged
names(x_named)<-features[,2]
x_mini<-x_named[,features_mini_index]

#3.	Uses descriptive activity names to name the activities in the data set
y_merged<-rbind(y_test,y_train)

y_label<-merge(y_merged,activity_labels,sort=FALSE)
#y_label<-merge(y_named,activity_labels,by.x="activity",by.y="V1",sort=FALSE)
y_label2<-select(y_label,V2)

y_named<-y_label2
names(y_named)<-"activity"


#4.	Appropriately labels the data set with descriptive variable names. 

subject_merged<-rbind(subject_test,subject_train)
subject_named<-subject_merged
names(subject_named)<-"subject"

names(x_mini)<-gsub("Acc", "Accelerometer", names(x_mini))
names(x_mini)<-gsub("Gyro", "Gyroscope", names(x_mini))
names(x_mini)<-gsub("BodyBody", "Body", names(x_mini))
names(x_mini)<-gsub("Mag", "Magnitude", names(x_mini))
names(x_mini)<-gsub("^t", "Time", names(x_mini))
names(x_mini)<-gsub("^f", "Frequency", names(x_mini))
names(x_mini)<-gsub("tBody", "TimeBody", names(x_mini))
names(x_mini)<-gsub("-mean()", "Mean", names(x_mini), ignore.case = TRUE)
names(x_mini)<-gsub("-std()", "STD", names(x_mini), ignore.case = TRUE)
names(x_mini)<-gsub("-freq()", "Frequency", names(x_mini), ignore.case = TRUE)
names(x_mini)<-gsub("angle", "Angle", names(x_mini))
names(x_mini)<-gsub("gravity", "Gravity", names(x_mini))

#5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

merged<-cbind(subject_named,y_merged,x_mini)

subjects=30
activities=6

sumary=matrix(nrow=subjects*activities,ncol=81)
mean=1

for(i in 1:subjects) {

 for (j in 1:activities) {

  level<-merged[merged[,1]==i & merged[,2]==j,]
  
  for(k in 1:81) {
  mean[k]<-mean(as.numeric(unlist(level[,k]))) 
  mean[1]=level[1,1]
  mean[2]=level[1,2]
    
  sumary[(i-1)*6+j,k]<-mean[k]
  }
 }  
}

final<-as.data.frame(sumary)
names(final)<-names(merged)
names(activity_labels)<-c("V1","activity")

final2<-merge(final,activity_labels,by.x="V1",by.y="V1",sort=FALSE)
final3<-select(final2,c(82,2,3:81))
final4<-arrange(final3,activity,subject)
write.table(final4, file = "./final.txt", row.name=FALSE)



