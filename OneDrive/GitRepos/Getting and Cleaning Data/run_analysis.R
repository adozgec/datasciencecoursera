run_alanysis <- function() {
  #In this function I'm reading files dynamically and then binding them together. there are to main files types.
  #1st one is Sets. 2nd one is features. Sets file contaions the Inertial Signals. Inertial signals files are
  #binded by row since group, place and axis informations are variables. Features data binded by row by
  #group information. 
  
  #group = test, train
  #places = body, total
  #sensors = gyro, acc
  #axis = x,y,z
  
  #final data would be like this. 
  # sets
  # subject, activity, widow001:window128,       group, place, sensor, axis
  #       1         1       [-1:1]               train  body    gyro    x
  #       2         2       [-1:1]               test   total   acc     y
  #       .         .            .               .      .       .       .
  #       .         .            .               .      .       .       .
  #      28         5       [-1:1]               train  body    gyro    z
  #      30         6       [-1:1]               test   total   acc     z
  # features
  # subject, activity, features001:features561,  group
  #       1         1       [-1:1]               train 
  #       2         2       [-1:1]               test
  #       .         .            .               .   
  #       .         .            .               .   
  #      28         5       [-1:1]               train
  #      30         6       [-1:1]               test
  
  path = "./AssignmentDataSet/"
  
  #test / train variable
  group_names <- c("test","train")
  
  #Intertial Signals files variables
  place_sensor_names <- list(c(place = "body",sensor = "acc"),c(place = "body",sensor = "gyro"),c(place = "total",sensor = "acc"))
  axis_names <- c("x","y","z")
  
  #activity names
  activity_names <- read.table(paste0(path,"activity_labels.txt"),header = FALSE)
  names(activity_names) <- c("activity","activity_name")
  
  #features names
  feature_names <- read.table(paste0(path,"features.txt"),header = FALSE)
  
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
    temp_features <- read.table(features_file_name,header = FALSE) 
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

        t_set <- read.table(set_file_name,header = FALSE)
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
  
  #Appropriately labels the data set with descriptive variable names.
  #Question 4
  names(sets) <- c("subject","activity",paste0("window",strnumber(1:128)),"group","place","sensor","axis")
  names(features) <- c("subject","activity",as.character(feature_names[[2]]),"group")
  
  #Uses descriptive activity names to name the activities in the data set
  #Question 3  
  sets <- merge(x = sets,y = activity_names,by.x = c("activity"),by.y = c("activity"),no.dups = TRUE)
  features <- merge(x = features,y = activity_names,by.x = c("activity"),by.y = c("activity"),no.dups = TRUE)
  
  
  #Extracts only the measurements on the mean and standard deviation for each measurement.
  #Question 2
  mean_std <- c(grep("mean",names(features),value = TRUE),grep("std",names(features),value = TRUE))
  features_mean_std <- features[1:10,c("group","subject","activity",mean_std)]
  
  #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  #Question 5
  sets <- data.table(sets)
  set_mean <- 
    sets[,list(
      "window001_mean" = mean(window001),
      "window002_mean" = mean(window002),
      "window003_mean" = mean(window003),
      "window004_mean" = mean(window004),
      "window005_mean" = mean(window005),
      "window006_mean" = mean(window006),
      "window007_mean" = mean(window007),
      "window008_mean" = mean(window008),
      "window009_mean" = mean(window009),
      "window010_mean" = mean(window010),
      "window011_mean" = mean(window011),
      "window012_mean" = mean(window012),
      "window013_mean" = mean(window013),
      "window014_mean" = mean(window014),
      "window015_mean" = mean(window015),
      "window016_mean" = mean(window016),
      "window017_mean" = mean(window017),
      "window018_mean" = mean(window018),
      "window019_mean" = mean(window019),
      "window020_mean" = mean(window020),
      "window021_mean" = mean(window021),
      "window022_mean" = mean(window022),
      "window023_mean" = mean(window023),
      "window024_mean" = mean(window024),
      "window025_mean" = mean(window025),
      "window026_mean" = mean(window026),
      "window027_mean" = mean(window027),
      "window028_mean" = mean(window028),
      "window029_mean" = mean(window029),
      "window030_mean" = mean(window030),
      "window031_mean" = mean(window031),
      "window032_mean" = mean(window032),
      "window033_mean" = mean(window033),
      "window034_mean" = mean(window034),
      "window035_mean" = mean(window035),
      "window036_mean" = mean(window036),
      "window037_mean" = mean(window037),
      "window038_mean" = mean(window038),
      "window039_mean" = mean(window039),
      "window040_mean" = mean(window040),
      "window041_mean" = mean(window041),
      "window042_mean" = mean(window042),
      "window043_mean" = mean(window043),
      "window044_mean" = mean(window044),
      "window045_mean" = mean(window045),
      "window046_mean" = mean(window046),
      "window047_mean" = mean(window047),
      "window048_mean" = mean(window048),
      "window049_mean" = mean(window049),
      "window050_mean" = mean(window050),
      "window051_mean" = mean(window051),
      "window052_mean" = mean(window052),
      "window053_mean" = mean(window053),
      "window054_mean" = mean(window054),
      "window055_mean" = mean(window055),
      "window056_mean" = mean(window056),
      "window057_mean" = mean(window057),
      "window058_mean" = mean(window058),
      "window059_mean" = mean(window059),
      "window060_mean" = mean(window060),
      "window061_mean" = mean(window061),
      "window062_mean" = mean(window062),
      "window063_mean" = mean(window063),
      "window064_mean" = mean(window064),
      "window065_mean" = mean(window065),
      "window066_mean" = mean(window066),
      "window067_mean" = mean(window067),
      "window068_mean" = mean(window068),
      "window069_mean" = mean(window069),
      "window070_mean" = mean(window070),
      "window071_mean" = mean(window071),
      "window072_mean" = mean(window072),
      "window073_mean" = mean(window073),
      "window074_mean" = mean(window074),
      "window075_mean" = mean(window075),
      "window076_mean" = mean(window076),
      "window077_mean" = mean(window077),
      "window078_mean" = mean(window078),
      "window079_mean" = mean(window079),
      "window080_mean" = mean(window080),
      "window081_mean" = mean(window081),
      "window082_mean" = mean(window082),
      "window083_mean" = mean(window083),
      "window084_mean" = mean(window084),
      "window085_mean" = mean(window085),
      "window086_mean" = mean(window086),
      "window087_mean" = mean(window087),
      "window088_mean" = mean(window088),
      "window089_mean" = mean(window089),
      "window090_mean" = mean(window090),
      "window091_mean" = mean(window091),
      "window092_mean" = mean(window092),
      "window093_mean" = mean(window093),
      "window094_mean" = mean(window094),
      "window095_mean" = mean(window095),
      "window096_mean" = mean(window096),
      "window097_mean" = mean(window097),
      "window098_mean" = mean(window098),
      "window099_mean" = mean(window099),
      "window100_mean" = mean(window100),
      "window101_mean" = mean(window101),
      "window102_mean" = mean(window102),
      "window103_mean" = mean(window103),
      "window104_mean" = mean(window104),
      "window105_mean" = mean(window105),
      "window106_mean" = mean(window106),
      "window107_mean" = mean(window107),
      "window108_mean" = mean(window108),
      "window109_mean" = mean(window109),
      "window110_mean" = mean(window110),
      "window111_mean" = mean(window111),
      "window112_mean" = mean(window112),
      "window113_mean" = mean(window113),
      "window114_mean" = mean(window114),
      "window115_mean" = mean(window115),
      "window116_mean" = mean(window116),
      "window117_mean" = mean(window117),
      "window118_mean" = mean(window118),
      "window119_mean" = mean(window119),
      "window120_mean" = mean(window120),
      "window121_mean" = mean(window121),
      "window122_mean" = mean(window122),
      "window123_mean" = mean(window123),
      "window124_mean" = mean(window124),
      "window125_mean" = mean(window125),
      "window126_mean" = mean(window126),
      "window127_mean" = mean(window127),
      "window128_mean" = mean(window128)
    ),c("subject","activity_name","group","place","sensor","axis")]
  
  
  
  list("sets" = sets,"features" = features, "set_mean" = set_mean, "features_mean_std" = features_mean_std  )
  
}

strnumber <- function (list){
  library(stringr)
  repeat_count <- str_length(max(list))
  for (i in 1:length(list)) {
    item <- list[[i]]
    list[[i]] <- paste0(c(rep(0,repeat_count - str_length(item)),item),collapse = "")
  }
  list
}

