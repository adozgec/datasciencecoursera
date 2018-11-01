  #In this code I'm reading files dynamically and then binding them together. there are to main files types.
  #1st one is Sets. 2nd one is features. Sets file contaions the Inertial Signals. Inertial signals files are
  #binded by row since group, place and axis informations are variables. Features data binded by row by
  #group information. 
  
  #group = test, train
  #places = body, total
  #sensors = gyro, acc
  #axis = x,y,z
  
# unique(sets$group)
  library(data.table)  
  # max(sets$measurement), min(sets$measurement), mean(sets$measurement)

  path = "./Samsung data/"
  
  #test / train variable
  group_names <- c("test","train")
  
  #Intertial Signals files variables
  place_sensor_names <- list(c(place = "body",sensor = "acc"),c(place = "body",sensor = "gyro"),c(place = "total",sensor = "acc"))
  axis_names <- c("x","y","z")
  
  #activity names
  activity_names <- read.table(paste0(path,"activity_labels.txt"),header = FALSE)
  names(activity_names) <- c("activity","activity_name")
  
  #features names
  feature_names <- data.table(read.table(paste0(path,"features.txt"),header = FALSE))
  names(feature_names) = c("feature","feature_name")
  
  #variables defined
  features <- NULL      #Features Master  
  sets <- NULL          #Sets Master
  temp_features <- NULL #temporary dataset to keep individual datasets before binding.
  temp_sets <- NULL     #temporary dataset to keep individual datasets before binding.

  
  
  #Loop in to folders and files and get each file bind it with its master dataset.
  #question 1
  for (group_index in 1:length(group_names)) {
    
    group_name <- group_names[group_index]
    
    subject_file_name = paste0(path, group_name, "/subject_", group_name, ".txt")
    activity_file_name = paste0(path, group_name, "/y_", group_name, ".txt")
    
    subject_list <- read.table(subject_file_name,header = FALSE)
    activity_list <- read.table(activity_file_name,header = FALSE)
    
    features_file_name <- paste0(path, group_name,"/X_",group_name,".txt")
    temp_features <- data.table(read.table(features_file_name,header = FALSE))
    temp_features[,"group"] <- group_name
    
    temp_features <- cbind(subject_list,activity_list,temp_features)
    
    if (is.null(features)) {
      features <- temp_features
    }
    else{
      features <- rbind(features,temp_features)
    }
    
    for (place_sensor_index in 1:length(place_sensor_names)) {
      for (axis_index in 1:length(axis_names)) {
        
        place_name <- as.character(place_sensor_names[[place_sensor_index]]["place"])
        sensor_name <- as.character(place_sensor_names[[place_sensor_index]]["sensor"])
        axis_name <- axis_names[axis_index]
        
        set_file_name <- paste0(path, group_name, "/Inertial Signals/", place_name , "_" , sensor_name , "_" , axis_name , "_", group_name,".txt")

        t_set <- data.table(read.table(set_file_name,header = FALSE))
        t_set[,"group"] <- group_name
        t_set[,"place"] <- place_name
        t_set[,"sensor"] <- sensor_name
        t_set[,"axis"] <- axis_name


        t_set <- cbind(subject_list,activity_list,t_set)

        if (is.null(sets)) {
          sets <- t_set
        }
        else
        {
          sets <- rbind(sets,t_set)
        }
      }
    }
  }
  
  sets <- data.table(sets)
  features <- data.table(features)
  
  #Appropriately labels the data set with descriptive variable names.
  #Question 4
  names(sets) <- c("subject","activity",c(1:128),"group","place","sensor","axis")
  names(features) <- c("subject","activity",c(1:561),"group")
  
  #Uses descriptive activity names to name the activities in the data set
  #Question 3  
  sets <- merge(x = sets,y = activity_names,by.x = c("activity"),by.y = c("activity"),no.dups = TRUE)
  features <- merge(x = features,y = activity_names,by.x = c("activity"),by.y = c("activity"),no.dups = TRUE)

  #Melting data set so that each mesurements are in seperate rows which allows operations easliy
  sets <- melt(sets,id=c("group","subject","activity","place","sensor","axis","activity_name"),measure.vars = as.character(c(1:128)),value.name = "measurement", variable.name = "window")
  features <- melt(features,id=c("group","subject","activity","activity_name"),measure.vars = as.character(c(1:561)),value.name = "value",variable.name = "feature")
  
  setkey(features,"feature")
  features <- merge(x = features,y = feature_names,by.x = c("feature"),by.y = c("feature"),no.dups = TRUE)
  
  #adding fature details to fetatures data table
  feature_groups = list(
      place = c("body","gravity","angle"),
      sensor = c("acc","gyro"),
      formula = c("mean","std","mad","max","min","sma","energy","iqr","entropy","arCoeff","correlation","maxInds","skewness","kurtosis")
  )
  
  feature_names = feature_names[,place := as.character(NA)]
  feature_names = feature_names[,sensor:= as.character(NA)]
  feature_names = feature_names[,formula := as.character(NA)]
  
  for (i in 1:3) {
    
    feature_grup_name = names(feature_groups[i])
    feature_group = feature_groups[[i]]
    
    for (feature_item in feature_group) {
        
      feature_names[grep(feature_item, feature_names$feature_name ,ignore.case = TRUE),feature_grup_name ] = feature_item
    }
  
  }

  #Extracts only the measurements on the mean and standard deviation for each measurement.
  #Question 2
  features_mean_std <- features["std" ==  formula | "mean" == formula,]
  
  
  #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  #Question 5
  sets_mean <-
    sets[,list(
      mean = mean(measurement)
    ),c("group","subject","activity","place","sensor","axis","activity_name")]  
  
  #write files
  write.table(sets, row.name=FALSE,file = "sets.txt")
  write.table(features, row.name=FALSE,file = "features.txt")
  write.table(sets_mean, row.name=FALSE,file = "sets_mean.txt")
  write.table(features_mean_std, row.name=FALSE,file = "features_mean_std.txt")
  
  View(sets[1:100,])
  View(features[1:100,])
  View(sets_mean[1:100,])
  View(features_mean_std[1:100,])
  dim(sets);dim(features);dim(sets_mean);dim(features_mean_std)
  
    