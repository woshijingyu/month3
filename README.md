### How the script works
The script run_analysis.R does the following:
 1. load the train data set 
 2. add the name of each feature 
 3. load the activities and subjects
 4. combine train data set and labels and subjects
 5. do the same to test data as train data
 6. combine test data and train data
 7. replace all activity numbers to activity names
 8. extract only mean and standard deviation measurements
 9. generate a second, independent tidy data set group by activity and subject



### Codebook for variables:

The functions in the variable names are:
mean(): Mean value
std(): Standard deviation


All columns of the data set:

 [1] "label"     the activity label                     
 [2] "subject"     the subject identifier          

The rest of the variables all represent the average values of the original variables. For example, "tBodyAcc.mean...X" represents the average value of "tBodyAcc.mean...X" for each individual for each activity. 

 [3] "tBodyAcc.mean...X"            
 [4] "tBodyAcc.mean...Y"              
 [5] "tBodyAcc.mean...Z"              
 [6] "tBodyAcc.std...X"               
 [7] "tBodyAcc.std...Y"               
 [8] "tBodyAcc.std...Z"               
 [9] "tGravityAcc.mean...X"           
[10] "tGravityAcc.mean...Y"           
[11] "tGravityAcc.mean...Z"           
[12] "tGravityAcc.std...X"            
[13] "tGravityAcc.std...Y"            
[14] "tGravityAcc.std...Z"            
[15] "tBodyAccJerk.mean...X"          
[16] "tBodyAccJerk.mean...Y"          
[17] "tBodyAccJerk.mean...Z"          
[18] "tBodyAccJerk.std...X"           
[19] "tBodyAccJerk.std...Y"           
[20] "tBodyAccJerk.std...Z"           
[21] "tBodyGyro.mean...X"             
[22] "tBodyGyro.mean...Y"             
[23] "tBodyGyro.mean...Z"             
[24] "tBodyGyro.std...X"              
[25] "tBodyGyro.std...Y"              
[26] "tBodyGyro.std...Z"              
[27] "tBodyGyroJerk.mean...X"         
[28] "tBodyGyroJerk.mean...Y"         
[29] "tBodyGyroJerk.mean...Z"         
[30] "tBodyGyroJerk.std...X"          
[31] "tBodyGyroJerk.std...Y"          
[32] "tBodyGyroJerk.std...Z"          
[33] "tBodyAccMag.mean.."             
[34] "tBodyAccMag.std.."              
[35] "tGravityAccMag.mean.."          
[36] "tGravityAccMag.std.."           
[37] "tBodyAccJerkMag.mean.."         
[38] "tBodyAccJerkMag.std.."          
[39] "tBodyGyroMag.mean.."            
[40] "tBodyGyroMag.std.."             
[41] "tBodyGyroJerkMag.mean.."        
[42] "tBodyGyroJerkMag.std.."         
[43] "fBodyAcc.mean...X"              
[44] "fBodyAcc.mean...Y"              
[45] "fBodyAcc.mean...Z"              
[46] "fBodyAcc.std...X"               
[47] "fBodyAcc.std...Y"               
[48] "fBodyAcc.std...Z"               
[49] "fBodyAcc.meanFreq...X"          
[50] "fBodyAcc.meanFreq...Y"          
[51] "fBodyAcc.meanFreq...Z"          
[52] "fBodyAccJerk.mean...X"          
[53] "fBodyAccJerk.mean...Y"          
[54] "fBodyAccJerk.mean...Z"          
[55] "fBodyAccJerk.std...X"           
[56] "fBodyAccJerk.std...Y"           
[57] "fBodyAccJerk.std...Z"           
[58] "fBodyAccJerk.meanFreq...X"      
[59] "fBodyAccJerk.meanFreq...Y"      
[60] "fBodyAccJerk.meanFreq...Z"      
[61] "fBodyGyro.mean...X"             
[62] "fBodyGyro.mean...Y"             
[63] "fBodyGyro.mean...Z"             
[64] "fBodyGyro.std...X"              
[65] "fBodyGyro.std...Y"              
[66] "fBodyGyro.std...Z"              
[67] "fBodyGyro.meanFreq...X"         
[68] "fBodyGyro.meanFreq...Y"         
[69] "fBodyGyro.meanFreq...Z"         
[70] "fBodyAccMag.mean.."             
[71] "fBodyAccMag.std.."              
[72] "fBodyAccMag.meanFreq.."         
[73] "fBodyBodyAccJerkMag.mean.."     
[74] "fBodyBodyAccJerkMag.std.."      
[75] "fBodyBodyAccJerkMag.meanFreq.." 
[76] "fBodyBodyGyroMag.mean.."        
[77] "fBodyBodyGyroMag.std.."         
[78] "fBodyBodyGyroMag.meanFreq.."    
[79] "fBodyBodyGyroJerkMag.mean.."    
[80] "fBodyBodyGyroJerkMag.std.."     
[81] "fBodyBodyGyroJerkMag.meanFreq.."

For the information of the original variables, please refer to the original data set at 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 



