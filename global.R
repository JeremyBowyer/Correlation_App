transformationList = list("Difference" = "diff",
                          "Rolling Sum" = "rollingsum",
                          "Subtract Rolling Median" = "submedian",
                          "Subtract Historical Median" = "subhistmedian",
                          "Subtract Cross Sectional Median" = "crossmedian",
                          "% Change" = "perchg",
                          "% Change from Median" = "perchgmedian",
                          "% Change from Std" = "perchgstd",
                          "Z-Score Cross Sectional" = "zscorecross",
                          "Z-Score Longitudinal" = "zscorelong",
                          "Binary (String)" = "binarystring",
                          "Binary (Value)" = "binaryvalue",
                          "Linear Residual" = "residual",
                          "Offset Forward" = "offsetfwd",
                          "Offset Backward" = "offsetbwd")

filterList = list("Value Filter" = "valueFilter",
                  "Percentile Filter" = "percentileFilter",
                  "Date Filter" = "dateFilter")

aggregationLevelList = list("Day" = "day",
                            "Month" = "month",
                            "Year" = "year")

aggregationFuncList = list("Sum" = "sum",
                           "Average" = "average",
                           "Earliest" = "earliest",
                           "Latest" = "latest",
                           "Lowest Number" = "lowest",
                           "Highest Number" = "highest")