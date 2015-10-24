library(data.table)
library(plyr)
library(reshape2)

features = "UCI HAR Dataset\\features.txt"
test_data_file = "UCI HAR Dataset\\test\\X_test.txt"
test_label_file = "UCI HAR Dataset\\test\\Y_test.txt"
test_subject_file="UCI HAR Dataset\\test\\subject_test.txt"
train_data_file = "UCI HAR Dataset\\train\\X_train.txt"
train_label_file = "UCI HAR Dataset\\train\\Y_train.txt"
train_subject_file="UCI HAR Dataset\\train\\subject_train.txt"

train_data <-read.table(train_data_file);
train_label <- read.table(train_label_file);
train_subject<- read.table(train_subject_file)


train_label_frame<-cbind(train_subject,train_label)
train_frame<- data.frame(train_label_frame, train_data)
rm(train_data)
rm(train_label)
rm(train_subject)

test_subject<- read.table(test_subject_file)
test_data <-read.table(test_data_file);
test_label <- read.table(test_label_file);


#test_frame<- data.frame(test_subject,test_label, test_data)
test_label_frame<-cbind(test_subject,test_label)
test_frame<- data.frame(test_label_frame, test_data)

rm(test_data)
rm(test_label)
rm(test_subject)

#this is for join two bind two tables
bind_frame <- rbind(train_frame,test_frame)
#rm(train_frame)
#rm(test_frame)


conn=file(features,open="r")
linn=readLines(conn)
result_frame<-data.frame(bind_frame[[1]],bind_frame[[2]])
names(result_frame)[1]<-'SUBJECTS'
names(result_frame)[2]<-'ACTIVITY'
j=3
for (i in 1:length(linn)){
  names(bind_frame)[i+1]<-linn[i]
  #print(grepl("mean()",linn[i]))
  
  if((grepl("mean()",linn[i]) && (!grepl("meanFreq()",linn[i]))) || grepl("std()",linn[i]) ){
    name=paste(i," ",sep="");
    if(grepl("mean()",linn[i]) && (!grepl("meanFreq()",linn[i])))name=paste(name,"MEAN",sep="")
    if(grepl("std()",linn[i]))name=paste(name,"STANDAR DEVIATION",sep="")
    
    if(grepl("tBody",linn[i]))name=paste(name,"Time BODY",sep=" ")
    if(grepl("fBody",linn[i]))name=paste(name,"Frequency BODY",sep=" ")
    if(grepl("BodyBody",linn[i]))name=paste(name,"BODY",sep=" ")
    if(grepl("tGravity",linn[i]))name=paste(name,"Time GRAVITY",sep=" ")
    if(grepl("fGravity",linn[i]))name=paste(name,"Frequency GRAVITY",sep=" ")
    
    if(grepl("Acc",linn[i]))name=paste(name,"ACCELARATION",sep=" ")
    
    if(grepl("Gyro",linn[i]))name=paste(name,"GYROSCOPE",sep=" ")
    if(grepl("Jerk",linn[i]))name=paste(name,"JERK",sep=" ")
    if(grepl("Mag",linn[i]))name=paste(name,"MAGNITUDE",sep=" ")
    
    if(grepl("()-X",linn[i]))name=paste(name,"X",sep=" ")
    if(grepl("()-Y",linn[i]))name=paste(name,"Y",sep=" ")
    if(grepl("()-Z",linn[i]))name=paste(name,"Z",sep=" ")
    print(name)
    result_frame[[j]]<-bind_frame[[i+1]]
    #result_frame
    names(result_frame)[j]<-name
   
    j=j+1
    print(j)
  }
}
close(conn)

by_SUBJECTS<-result_frame %>% group_by(SUBJECTS,ACTIVITY) %>%  summarize_each(funs(mean))
 
write.table(by_SUBJECTS,"womwork_cleaning_data.txt",row.name=FALSE)

