Divvy Bike Trips Data Analysis (2020)

This project focuses on the analysis of Divvy bike trip data for the year 2020, aiming to identify patterns in rider behavior, such as ride length, station usage, and differences between members and casual riders. 
Data cleaning was performed to ensure the dataset was prepared for analysis. This involved converting columns like start_station_id and end_station_id from character to integer, handling missing values using the drop_na() function, and removing duplicate entries. 
Additionally, trips with negative ride lengths were filtered out to maintain data quality.

Several new columns were created to aid analysis, including date, month, day, year, and day_of_week using the lubridate package, allowing for time-based trend analysis. The cleaned dataset was then used for statistical analysis, where key metrics like mean, median, minimum, and maximum ride length were computed for both rider types. 
The dataset was also grouped by rider type (member vs. casual) to calculate average ride length, standard deviation, and other summary statistics. A custom function was used to calculate the mode for categorical data like day_of_week and start_station_name.

In terms of statistical analysis, the data was grouped by rider type and day of the week to determine which days were most popular among members and casual riders. Additionally, seasonal trends were analyzed by grouping rides by month to assess the variation in ride frequency and ride duration over the year. 
The statistical summaries included measures of central tendency (mean, median) and dispersion (standard deviation) to understand the typical ride duration and its variability across different rider groups.

Visualizations were created using ggplot2 to further explore the data. These included bar charts to compare the number of rides between members and casual riders by weekday, ride duration distributions, and monthly ride patterns. This analysis provided insights into seasonal and weekly trends, identifying key periods of activity for both rider types. 
The project concluded with an analysis of station popularity, identifying the most frequently used start stations for members and casual riders separately.

