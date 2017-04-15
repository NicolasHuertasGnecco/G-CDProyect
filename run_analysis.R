        ## 1. gives the file a name and getting libraries
        fname<-"Data proyect.zip"
        library(data.table)
        library(plyr)
        library(reshape2)
        ##2.looks for a file with the name  if not downloads the file 
        if (!file.exists(fname))
        {
                fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
                download.file(fileURL, fname, method="curl")
        } 
        ##3. if the file exists it needs to be unzipped after creating it
        if(file.exists(fname))
        {
                unzip(fname)   
        }
        ##4.Obtain the wanted elements from the features
        ##only does containing mean and std in their names
        ##4.1 first load all feautures
        feat<-read.table("./UCI HAR Dataset/features.txt")
        ##4.2 make name of features character
        feat[,2]<-as.character(feat[,2])
        ##4.5 get position of means and stds
        queridos<-grep(".*mean*.|.*std*.",feat[,2])
        ##5. getting data of test only for colums with means and stds
        testobs<-read.table("./UCI HAR Dataset/test/X_test.txt")[queridos]
        ##6. transforming the names
        nombresf<-feat[queridos,2]
        v1<-gsub("mean","Mean",nombresf)
        v2<-gsub("-","",v1)
        v3<-gsub("\\(\\)","",v2)
        setnames(testobs,old=names(testobs),new=v3)
        ##7. getting the subject id
        subjetcttestidtest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
        ##8 setting the name of the subject
        setnames(subjetcttestidtest,old=names(subjetcttestidtest),"Subject")
        ##9.Getting activities and their labels
        activitiestest<-read.table("./UCI HAR Dataset/test/y_test.txt")
        setnames(activitiestest,names(activitiestest),"Activity")
        testtotal<-cbind(subjetcttestidtest,activitiestest,testobs)
        etiquetas<-read.table("./UCI HAR Dataset/activity_labels.txt")
        ##9 including the labels in the table and setting subject id as a F
        testtotal$Activity<-factor(testtotal$Activity,levels= etiquetas[,1],labels=etiquetas[,2])
        testtotal$Subject<-as.factor(testtotal$Subject)
        ##10. Repeat proces for train
        
        
        
        ##10.5. getting data of train only for colums with means and stds
        trainobs<-read.table("./UCI HAR Dataset/train/X_train.txt")[queridos]
        ##10.6. transforming the names
        nombresf<-feat[queridos,2]
        v1<-gsub("mean","Mean",nombresf)
        v2<-gsub("-","",v1)
        v3<-gsub("\\(\\)","",v2)
        setnames(trainobs,old=names(trainobs),new=v3)
        ##10.7. getting the subject id
        subjetcttrainidtrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
        ##10.8 setting the name of the subject
        setnames(subjetcttrainidtrain,old=names(subjetcttrainidtrain),"Subject")
        ##10.9.Getting activities and their labels
        activitiestrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
        setnames(activitiestrain,names(activitiestrain),"Activity")
        traintotal<-cbind(subjetcttrainidtrain,activitiestrain,trainobs)
        etiquetas<-read.table("./UCI HAR Dataset/activity_labels.txt")
        ##10.10 including the labels in the table and setting subject id as a F
        traintotal$Activity<-factor(traintotal$Activity,levels= etiquetas[,1],labels=etiquetas[,2])
        traintotal$Subject<-as.factor(traintotal$Subject)
        
        ##11. bindig the data together
        grandtotal<-rbind(testtotal,traintotal)
        
        ##12.Compresingdata
        grandtotalmel<-melt(grandtotal,id=c("Subject","Activity"))
        grandtotalmean<-dcast(grandtotalmel,Subject + Activity ~ variable, mean)
        
        ##13.Writing the tidy table
        write.table(grandtotalmean, "tidy.txt", row.names = FALSE, quote = FALSE)
        








