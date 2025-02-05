### Data

-   **X\_train.txt**: Each row is a subject/activity combination. Each
    column is a feature (described below). Training data is 70% of the
    total dataset.
-   **X\_test.txt**: Each row is a subject/activity combination. Each
    column is a feature (described below). Training data is 30% of the
    total dataset.

### Variables

-   **features.txt**: Maps features to ID numbers 1-561. Features are
    variables like mean, std dev, etc taken for various measurements
    like BodyAcc, BodyGyroJerkMag, etc. There are 17 variables taken for
    each of 33 measurements. ID number corresponds to column number in
    X\_train.txt and X\_test.txt.
-   **activity\_labels.txt**: Maps activities (walking, standing, etc)
    to ID numbers 1-6.
-   **y\_train.txt**: Activity ID numbers for each row in X\_train.txt
-   **y\_test.txt**: Activity ID numbers for each row in X\_test.txt
-   **subject\_train.txt**: Subject ID numbers for each row in
    X\_train.txt
-   **subject\_test.txt**: Subject ID numbers for each row in
    X\_test.txt

### Transformations

-   Read in each of the .txt files above. For the data tables, read them
    in as fixed width files and define the widths of each column.
-   Append the test data to the training data.
-   We only want to keep columns from our dataset which correspond to
    the means and standard deviations of the measurements, so get these
    column indices, and drop off all other columns from the combined
    data.
-   Set the column names of the dataset according to features.txt.
-   Add columns to the dataset for subject ID and activiy name. To do
    this, first append the subject IDs and activity names of the test
    tables to the training tables. For the activity table, drop off the
    column of activity ID numbers.
-   Create a new table of averages for each subject/activity, by
    grouping the combined data bu subject and activity and then taking
    the mean of the remaining columns.
