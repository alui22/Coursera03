Data Dictionary - Human Activity Recognition Using Smartphones

average: Average value grouped by subject, activity, and metric.
  
labels: Type of activity

  LAYING
  SITTING
  STANDING
  WALKING
  WALKING_DOWNSTAIRS
  WALKING_UPSTAIRS
  
  
subject: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

  1
  2
  3
  4
  5
  6
  7
  8
  9
  10
  11
  12
  13
  14
  15
  16
  17
  18
  19
  20
  21
  22
  23
  24
  25
  26
  27
  28
  29
  30
  
variable:
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

  1	fBodyAcc.mean...X
  2	fBodyAcc.mean...Y
  3	fBodyAcc.mean...Z
  4	fBodyAcc.meanFreq...X
  5	fBodyAcc.meanFreq...Y
  6	fBodyAcc.meanFreq...Z
  7	fBodyAcc.std...X
  8	fBodyAcc.std...Y
  9	fBodyAcc.std...Z
  10	fBodyAccJerk.mean...X
  11	fBodyAccJerk.mean...Y
  12	fBodyAccJerk.mean...Z
  13	fBodyAccJerk.meanFreq...X
  14	fBodyAccJerk.meanFreq...Y
  15	fBodyAccJerk.meanFreq...Z
  16	fBodyAccJerk.std...X
  17	fBodyAccJerk.std...Y
  18	fBodyAccJerk.std...Z
  19	fBodyAccMag.mean..
  20	fBodyAccMag.meanFreq..
  21	fBodyAccMag.std..
  22	fBodyBodyAccJerkMag.mean..
  23	fBodyBodyAccJerkMag.meanFreq..
  24	fBodyBodyAccJerkMag.std..
  25	fBodyBodyGyroJerkMag.mean..
  26	fBodyBodyGyroJerkMag.meanFreq..
  27	fBodyBodyGyroJerkMag.std..
  28	fBodyBodyGyroMag.mean..
  29	fBodyBodyGyroMag.meanFreq..
  30	fBodyBodyGyroMag.std..
  31	fBodyGyro.mean...X
  32	fBodyGyro.mean...Y
  33	fBodyGyro.mean...Z
  34	fBodyGyro.meanFreq...X
  35	fBodyGyro.meanFreq...Y
  36	fBodyGyro.meanFreq...Z
  37	fBodyGyro.std...X
  38	fBodyGyro.std...Y
  39	fBodyGyro.std...Z
  40	tBodyAcc.mean...X
  41	tBodyAcc.mean...Y
  42	tBodyAcc.mean...Z
  43	tBodyAcc.std...X
  44	tBodyAcc.std...Y
  45	tBodyAcc.std...Z
  46	tBodyAccJerk.mean...X
  47	tBodyAccJerk.mean...Y
  48	tBodyAccJerk.mean...Z
  49	tBodyAccJerk.std...X
  50	tBodyAccJerk.std...Y
  51	tBodyAccJerk.std...Z
  52	tBodyAccJerkMag.mean..
  53	tBodyAccJerkMag.std..
  54	tBodyAccMag.mean..
  55	tBodyAccMag.std..
  56	tBodyGyro.mean...X
  57	tBodyGyro.mean...Y
  58	tBodyGyro.mean...Z
  59	tBodyGyro.std...X
  60	tBodyGyro.std...Y
  61	tBodyGyro.std...Z
  62	tBodyGyroJerk.mean...X
  63	tBodyGyroJerk.mean...Y
  64	tBodyGyroJerk.mean...Z
  65	tBodyGyroJerk.std...X
  66	tBodyGyroJerk.std...Y
  67	tBodyGyroJerk.std...Z
  68	tBodyGyroJerkMag.mean..
  69	tBodyGyroJerkMag.std..
  70	tBodyGyroMag.mean..
  71	tBodyGyroMag.std..
  72	tGravityAcc.mean...X
  73	tGravityAcc.mean...Y
  74	tGravityAcc.mean...Z
  75	tGravityAcc.std...X
  76	tGravityAcc.std...Y
  77	tGravityAcc.std...Z
  78	tGravityAccMag.mean..
  79	tGravityAccMag.std..
