

## Instructions
---

### Overview
<<<<<<< HEAD
Upload a csv file with data, input the appropriate columns to each field (Date, Category, Endogenous Metric, etc) in the , then click "Run Analysis." The app will assume every column in your dataset not included in at least on field to be exogenous metric. Once the app is done processing your data, it will generate a number of analytical outputs. Among these are correlations for the entire dataset, correlations by date (if applicable), scatter plot, histogram, QQ plots, and more.
=======
Upload a csv file with data, input the appropriate columns to each field (Date, Category, Endogenous Metric, etc), then click "Run Analysis." The app will assume every column in your dataset not included in at least on field to be exogenous metric. Once the app is done processing your data, it will generate a number of analytical outputs. Among these are correlations for the entire dataset, correlations by date (if applicable), scatter plot, histogram, QQ plots, and more.
>>>>>>> 2431528b477056caf2f56f9a41661f5958dcadda

### File Upload
---
#### File Format
Uploaded data needs to be in .csv file format. 

#### Data Format
Your data should be in a "long form" format as opposed to wide form. This means your table of data should have columns for an endogenous metric, each exogenous metric, a date (optional), and categories (optional), and a row for each unique combination of date and category. Below is an example table:
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
<br/>
An illlustration of long vs wide form can be found <a href="https://www.theanalysisfactor.com/wide-and-long-data/" target="_blank">here</a>. 

### Column Selection Options
---
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

### Filters
---
This section allows you to filter your data in various ways. You will select a column, then provide the criteria by which to filter the data. Note that this will be applied to all of the rows in your dataset. For example, if you filter out any dates before 1/1/2001, that will remove all rows where your date column is earlier than that date.

For an explanation of each type of filter (Value, Percentile, Date), see the Glossary.

### Metric Transformations
---
This section allows you to create transformations of columns in your dataset. For example if you have a column of prices that you want to be turned into performance, you can use the "% Change" transformation to do that. See the Glossary for an explanation of each transformation type.

<<<<<<< HEAD
Each transformation you make will ask you to fill out a number of fields. Please note that which fields you see will be dependent on which transformation you are creating. An explanation of each field can be found below:
=======
Each transformation require you to fill out a number of fields. An outline of each can be found below:
>>>>>>> 2431528b477056caf2f56f9a41661f5958dcadda

#### Column Suffix Name
When you create a metric transformation, an additional column will be appended to the end of your dataset. The *Column Suffix Name* field lets you designate a suffix for that new column. For example, if you have a column called `Price` and you want to turn it into performance, you might want to add the suffix "Pct_Chg." Doing so will result in a new column named `Price_Pct_Chg`.

#### Lag
Many of the transformations allow you to set the timeframe over which the transformation is applied. For example, if you want to transform a column by subtracting the rolling median, you are given the opportunity to define what the window is for the rolling median. Should the rolling median include the last 10 data points? The last 100? If your data is daily and you want to subtract out the rolling 30 day median, you should set the *Lag* field to 30. This field will be used in different ways for different types of transformations. Check the Glossary for how it will be applied specifically to the transformation you're interested in.

#### Select columns to transform
This is self explanatory. Select the columns that you are interested in transforming. Multiple column selections are allowed.

#### Select column to transform along (probably a date)
This is the column that will determine what order your data is in when the transformation is applied. This will typically be a date column. For instance, if you want to take a % Change of a given column, the app needs to know what order the data should be in when creating that transformation -- otherwise, this calculation will be performed in whatever order it happens to be in when imported.

#### Select category columns to group by (optional)
If you have a category column in your dataset (eg country, company, level of development, etc). Since the data is in long form, you might have multiple categories stacked on top of each other (see the example table above). If this is the case, the app needs to know how to slice up your data to apply these transformations. If you want to subtract out the rolling median price for a handful of companies, you wouldn't want the prices for one company to be included in the median price for another company.

<<<<<<< HEAD
#### Value to be flagged as 1
There are a couple transformations that allow you to convert a column with binary <a href="http://www.statmethods.net/input/dates.html" target="_blank">string</a> data (as opposed to numeric) into binary numeric data. If you want to flag a certain string as 1 (and the rest of the values in the column as 0), this field is where you could set that value.

#### Value, data points equal to or above to be flagged as 1
=======

### Value to be flagged as 1
There are a couple transformations that allow you to convert a column with binary <a href="http://www.statmethods.net/input/dates.html" target="_blank">string</a> data (as opposed to numeric) into binary numeric data. If you want to flag a certain string as 1 (and the rest of the values in the column as 0), this field is where you could set that value.

### Value, data points equal to or above to be flagged as 1
>>>>>>> 2431528b477056caf2f56f9a41661f5958dcadda
Similar to above, except instead of a string to flag, you designate a minimum value above which all values will be flagged as 1.

#### Select x columns & Select y column
These are used when creating a residual column. You are asked to give x columns (one or more) and a y column. The resulting column will be the difference between a given y value and the corresponding expected value from a linear regression.

### Offsets
---
This section allows you to shift a column up or down by a given amount. If you provide category column(s), the app will take that into account and it will shift the data up/down for each category in the category columns. So if you have price data that you've turned into performance using the % change metric transformation, you can use an offset to shift those performance numbers forward so they become subsequent performance.

For an explanation of each type of transformation along with their requisite inputs, see the Glossary.
