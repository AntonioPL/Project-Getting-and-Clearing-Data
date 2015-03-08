# Project-Getting-and-Clearing-Data

run_analysis.R script performs the following steps:
============================================================================  
0. Copy the files X_train.txt, X_test.txt, y_train.txt, y_test.txt, subject_train.txt, subject_test.txt, features.txt and activity_labels.txt in the same directory (working directory).
1. Read X_train.txt, X_test.txt, y_train.txt, y_test.txt, subject_train.txt and subject_test.txt from the working directory into the data.frames: X.Train, X.Test, Y.Train, Y.Test, S.Train and S.Test respectively
2. Create a new variable "Group" to clasify training and test data.
3. Concatenate the training and test data for each X,Y and S in the data.frames: X,Y and S.
4. Read features.txt into the data.frame features.
5. Select the variable relates with sd or mean.
6. Filter the X data.frame with the selected features.
7. Replace the name of the variable of X with another more clear.
8. Read activity_labels.txt into the data.frame activities.
9. Replace the value of the activities in the data.frame S with the name of the activities.
10. Join the X,Y and S data.frames in the data.frame Data.
11. Write the file "merged clean data.txt" with the data.frame Data.
12. For each Subject and each Activitie we calculate the mean of each variable and construct the data.frame Data2.
13. Write the file "features averages per subject and activiy.txt" with the data.frame Data2.
