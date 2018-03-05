

## Glossary
---

### Date Aggregation Functions
---
Below you'll find a description for each aggregation function. This is how your granular data will be combined into a condensed value.

#### Sum
A simple sum of the individual data points to be aggregated.

#### Average
Arithmetic mean of the individual data points to be aggregated. The sum of the data points divided by the number of data points.

#### Earliest
The earliest number available in the data to be aggregated.

#### Latest
The latest number available in the data to be aggregated.

#### Lowest Number
The lowest number in the data to be aggregated.
 
#### Highest Number
The higher number in the data to be aggregated.

### Filters
---
#### Value
You provide a column, a minimum and a maximum value. The app will then filter your dataset to only include the rows where the value in the given column is within those bounds, inclusive.

#### Percentile
You provide a column and a percentile range. The app will then filter your dataset to only include the rows where the value in the given column is within that range of percentiles, inclusive. For example, if you want to weed out rows with the highest 5% of values in that column, you could set the range to be 0 - 0.95.

#### Date
You provide a column (date), minimum and maximum dates, and a date format (see <a href="http://www.statmethods.net/input/dates.html" target="_blank">here</a> for how to interpret date formats). The app will then filter your dataset to only include the rows where the date in the given date column is within that range of dates, inclusive.

### Metric Transformations
---
#### Difference
The difference between data points in a given column, lagged by a given amount, ordered by a given date column.

#### Rolling Sum
The rolling sum of a given column, ordered by a given date, where the size of the rolling window is based on the given lag value.

#### Subtract Rolling Median
Each data point in a given column will have a rolling median subtracted from it. The column will first be ordered by a given date column and sliced up based on a given category column, then the window for the rolling median is defined by the given lag value.

#### Subtract Historical Median
Similar to `Subtract Rolling Median` except the median being subtracted from each data point is the median for the entire history up until that data point.

#### Subtract Cross Sectional Median
Each data point in a given column will have the cross sectional median subtracted from it. The cross sectional median is defined as the median value in the column, sliced up by an optional category column. For example, if you want to subtract out the median price of all countries for each date in your dataset, you would select your `Price` column in the *Select columns to transform* field, then select your `Date` column in the *Select category columns to group by (optional)* field. 

#### % Change
Similar in structure to the *Difference* transformation, but it's a % change, instead of a simple difference. The later data point divided by the earlier data point, minus 1.

#### % Change from Median
Similar in structure to `% Change` transformation, but instead of the base number being the earlier data point, it is the historical median up until that data point.

#### % Change from Std
Similar to `% Change from Median` except instead of the base number being the historical median up until that data point, it's the standard deviation of the history up until that data point.

#### Z-Score Cross Sectional
Each value in the given column (sliced up by the values in a given category column, if provided) minus the mean divided by the standard deviation.

#### Z-Score Longitudinal
Each value in the given column (ordered by a given date column and sliced up by the values in a given category column, if provided) minus the historical mean up until that data point and divided by the standard deviation of the history up until that data point.

#### Binary (String)
A given column will be transformed into 0s and 1s. Values that match the user-provided string will be 1, all others will be 0.

#### Binary (Value)
A given column will be transformed into 0s and 1s. Values equal to or greater than the user-provided value will be 1, all others will be 0.

#### Linear Residual
The residual values of a univariate linear regression, where the user provides an x column and a y column. It is the y value minus the expected y given the linear model.

#### Offset Forward
Data will simply be shifted forward by a specified number of data points.

#### Offset Backward
DAta will simply be shifted backward by a specific number of data points.