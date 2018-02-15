
## Instructions
### Overview
Upload a csv file with data, indicate which columns are fulfilling which role (Date, Category, Endogenous Metric, etc), then click "Run Analysis." The app will assume every column in your dataset that isn't fulfilling some other role to be an exogenous metric. Once the app is done processing your data, it will generate a number of analytical outputs. Among these are correlations for the entire dataset, correlations by date, scatter plot, histogram, QQ plots, and more.

### File Upload

#### File Format
Uploaded data needs to be in .csv file format. 

#### Data Format
Your data should be in a "long form" format. This means your table of data should have columns for an endogenous metric, each exogenous metric, a date (optional), and categories (optional), and a row for each unique combination of date and category. Below is an example table:
<br/>

|    Date    |    Country    | Development |  X1  |  X2  |  Y   |
|------------|---------------|-------------|------|------|------|
| 12/31/2003 | United States |     DM      | 0.04 | 0.01 | 0.19 | 
| 12/31/2004 | United States |     DM      | 0.86 | 0.93 | 0.41 | 
| 12/31/2005 | United States |     DM      | 0.16 | 0.57 | 0.42 | 
| 12/31/2003 |    Brazil     |     EM      | 0.13 | 0.39 | 0.41 | 
| 12/31/2004 |    Brazil     |     EM      | 0.12 | 0.21 | 0.07 | 
| 12/31/2005 |    Brazil     |     EM      | 0.15 | 0.69 | 0.45 | 
| 12/31/2003 |    Germany    |     DM      | 0.99 | 0.82 | 0.81 | 
| 12/31/2004 |    Germany    |     DM      | 0.06 | 0.26 | 0.99 | 
| 12/31/2005 |    Germany    |     DM      | 0.57 | 0.38 | 0.37 | 

### Column Selection Options
#### Select Y column
Here you will select which column in your dataset is the endogenous factor. 

The selected column will be excluded from the list of exogenous factors.

#### Select Date column (optional)
Here you will select which column in your data is the date column. This metric will be used to slice up your data and analyze each section. For instance, if your data is annual, you will see correlations for each year that exists in your dataset.

The selected column will be excluded from the list of exogenous factors.

#### Format dates are in
If you designate a date column, you'll need to specify what format it's in. For example, if your dates look like this `12/31/2017`, the corresponding format would be `%m/%d/%Y`. You can find a more detailed explanation <a href="http://www.statmethods.net/input/dates.html" target="_blank">here</a>.

#### Select Category column (optional)
The column you select here will determine how to display some of the analysis on the output pages. For instance, the scatter plot of the data will be highlighted by category.

The selected column will be excluded from the list of exogenous factors.

#### Select Columns to Ignore (optional)
The columns you selected in the above sections will be automatically excluded from the list of exogenous factors. For instance, the app will know not to display a correlation between your date column and your y column. However, if there are some additional columns that you want to ignore, you can add them here.