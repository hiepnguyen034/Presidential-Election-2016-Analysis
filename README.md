# Presidential-Election-2016-Analysis
Analyzing the presidential election in 2016 using the data sets from http://classic.fec.gov/pindex.shtml

The collected data sets can be downloaded from https://www.dropbox.com/s/t8n4s3lugyep44y/Campaign.zip?dl=0 (140 mb)

# This piece of R code does the following:

1)Joined the data with the appropriate headers and column names from http://www.fec.gov/finance/disclosure/metadata/DataDictionaryContributionsbyIndividuals.shtml
and cleaned the dataset, which includes dropping unnecessary columns and removed NA rows 

2)Created a dataframe "popular" that aggregates the data by candidate and added a column for the total number of donations and total amount of donations
made to each candidate

3)Created two dataframes "weekly_d" and "weekly_r"  that show the number of donations and amount of donations of each candidate every week, 
with 'weekly_d' showing candidates in Democratic and weekly_r showing candidates in Republican

4)Filtered top 9 candidates of both parties and added other candidates into a group called "Others" and visualized the amount of donations by week

5)Created a datafame "by_state" that shows the amount raised by each candidate in each state, joined it with primary_outcome.csv, and created
a column for both the money raised and votes received such that state/party combination equals 100

6)Chose 5 variables from county_fact.csv that might affect the amount of money raised and created decision tree and random forest for those variables
