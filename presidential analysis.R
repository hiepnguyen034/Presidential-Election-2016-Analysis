load(file='camp.Rdata')
options(stringsAsFactors = FALSE)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(stringr)
library(data.table)
library(magrittr)
library(rpart)
library(rpart.plot)
library(ranger)
library(caret)
cn<-fread('cn.txt', sep = '|', header = F, na.strings = '')
add_names <- function(df, file) {
        names(df) <- names(read.csv(file))
        df
}
read_fec <- function(file) {
        read.csv(file, sep = '|', skipNul = T, na.strings = '',
                 header = F, quote = "")
}
#Joining data set
'ccl.txt' %>%
        read_fec %>%
        add_names('ccl_header_file.csv')-> link
'cn.txt' %>%
        read_fec %>%
        add_names('cn_header_file.csv') ->cand
cand %>%
        inner_join(link, by = c("CAND_ID", "CAND_ELECTION_YR")) -> df
df %>%
        inner_join(camp, by = "CMTE_ID") -> df
df %>%
        select(-CAND_ID, -CMTE_ID,-OTHER_ID,-CAND_ST2) -> df
df %>%
        filter(!is.na(TRANSACTION_DT)) %>%
        mutate(date = as.Date(mdy(TRANSACTION_DT)))->df
df %>%
        filter(!is.na(TRANSACTION_AMT))->df
df %>%
        filter(!is.na(CAND_NAME))->df
#I removed Candidate ID, CMTE ID since we already have the names of the cadidates and the ID's 
#do not seem to be necessarry. In addition, OTHER_ID and CAND_ST2 are removed because the columns
#do not contain much useful information . Also, I removed all transactions dates and amounts that do not
#contain any information
#The camp table has 7845344 rows and 15 columns
#The cand table has 7644 rows and 15 columns
#The cn table has 7644 rows and 15 columns
#The link column(ccl.txt) has 6426 rows and 7 columns.
#After combining the name and dropping some variables, the table has 3440497 rows and 31 variables


df %>%
        group_by(CAND_NAME) %>%
        summarize(total_amount=sum(TRANSACTION_AMT),total_donation=n()) -> popular
#This is a data frame that include all candidates' names and the number of donations  made to a candidate as well
#as total money of donations that this candidate has

#Cleaning data, dividing data sets into weeks, focusing on the statistics of top 9 candidates of each party and plotting.
df_d<-filter(df,CAND_PTY_AFFILIATION=="DEM")
df_d$week<-floor(as.numeric(df_d$date-min(df_d$date))/7)+1
df_d<-filter(df_d,week>740) #Clean some data from 2010, which is trivial but will make the graph not readable
df_d$week<-df_d$week-700#Restart the count of week
df_d<-filter(df_d,week<220) #Clean some data from 2018, which is probably noise
df_d<-select(df_d,CAND_NAME,TRANSACTION_AMT,week)
df_d %>%
        group_by(CAND_NAME,week)%>%
        summarize(Donations=sum(TRANSACTION_AMT))->df_d
weekly_d<-df_d
#Rank candidates
weekly_d%>%
        group_by(CAND_NAME)%>%
        summarize(Dollars=sum(Donations))->weekly_d_rank #Sum the amount of dollars of all candidates
weekly_d_rank<-arrange(weekly_d_rank,desc(Dollars))
weekly_d_rank%>%
        top_n(9,Dollars)->weekly_d_rank9#Showing top 9 after calculating the total money
#From the table, we can see that the top 9 candidates of Democratic are Clinton, Sanders, Van Hollan, Schumer,
#Murphy, Feingold, Bennet, Kander, and Wyden.
weekly_d%>%
        inner_join(weekly_d_rank,by="CAND_NAME")->weekly_d_rank
weekly_d_rank%>%
        filter(Dollars<4208096)->weekly_d_rank_non9
#Filter out non-top 9
weekly_d_rank_non9%>%
        group_by(week)%>%
        summarize(Donations=sum(Donations))->weekly_d_rank_non_9
#Consider candidates in non-top 9 by week and donations
weekly_d_rank_non_9[["CAND_NAME"]]<-rep('Others',105)
weekly_d_rank_non_9<-weekly_d_rank_non_9[c(3,1,2)]
#Change the candidate in non-top 9 to others
#The  above codes create a new dataframe consisting only non-top9 and their donations
#by week
weekly_d_rank%>%
        filter(Dollars>=4208096)->weekly_d_rank_9
arrange(weekly_d_rank_9,desc(Dollars))->weekly_d_rank_9
weekly_d_rank_9[["Dollars"]]<-NULL

#Create a new dataframe containing only top 9 and their donations by week
rbind(as.data.frame(weekly_d_rank_9),as.data.frame(weekly_d_rank_non_9))->plot_d
#Combine top 9 data frame and Others data frame to plot
ggplot(plot_d, aes(week,Donations,color=CAND_NAME)) + geom_line(bins = 45)

df_r<-filter(df,CAND_PTY_AFFILIATION=="REP")
df_r$week<-floor(as.numeric(df_r$date-min(df_r$date,na.rm=TRUE))/7)+1
df_r<-filter(df_r,week>46976) #Clean some data from 1010, which are just noises and errors
df_r$week<-df_r$week-52089#Restart the count of week
df_r<-filter(df_r,week<155) #Clean some data that is not in 2016 or back, which is probably noise
df_r<-select(df_r,CAND_NAME,TRANSACTION_AMT,week)
df_r %>%
        group_by(CAND_NAME,week)%>%
        summarize(Donations=sum(TRANSACTION_AMT))->df_r
weekly_r<-df_r

weekly_r%>%
        group_by(CAND_NAME)%>%
        summarize(Dollars=sum(Donations))->weekly_r_rank #Sum the amount of dollars of all candidates
weekly_r_rank<-arrange(weekly_r_rank,desc(Dollars))
weekly_r_rank%>%
        top_n(9,Dollars)->weekly_r_rank9
#From the table we can see that the top 9 of Republican are Ryan, Cruz, Rubio, Bush, Trump, Kasich,
#Portman, and Boehner
weekly_r%>%
        inner_join(weekly_r_rank,by="CAND_NAME")->weekly_r_rank
weekly_r_rank%>%
        filter(Dollars<11383161)->weekly_r_rank_non9
#Filter out non-top 9
weekly_r_rank_non9%>%
        group_by(week)%>%
        summarize(Donations=sum(Donations))->weekly_r_rank_non_9
#Consider candidates in non-top 9 by week and donations
weekly_r_rank_non_9[["CAND_NAME"]]<-rep('Others',114)
weekly_r_rank_non_9<-weekly_r_rank_non_9[c(3,1,2)]
weekly_r_rank_non_9<-filter(weekly_r_rank_non_9,Donations>0)#Clean value of donations < 600, which is probably just an error in data
#Change the candidates' names in non-top 9 to others
#The  above codes create a new dataframe consisting only non-top9 and their donations
#by week

weekly_r_rank%>%
        filter(Dollars>=11383161)->weekly_r_rank_9
arrange(weekly_r_rank_9,desc(Dollars))->weekly_r_rank_9
weekly_r_rank_9[["Dollars"]]<-NULL

#Create a new dataframe containing only top 9 and their donations by week
rbind(as.data.frame(weekly_r_rank_9),as.data.frame(weekly_r_rank_non_9))->plot_r
#Combine top-9 data frame and Others dataframe to plot
ggplot(plot_r, aes(week,Donations,color=CAND_NAME)) + geom_line(bins = 45)


df%>%
        group_by(STATE,CAND_NAME)%>%
        summarize(Donations=sum(TRANSACTION_AMT),Number_of_donations=n())->by_state#Aggregate the total number of donations and amount raised by candiates each state
df_4<-by_state
read.csv("primary_results.csv")->primary_result
primary_result%>%
        select(-state)->primary_result
colnames(primary_result)[1]<-"STATE"#Change the column name for consitency
primary_result%>%
        group_by(STATE,candidate,party)%>%
        summarize(state_votes=sum(votes))->primary_result#Calculate votes by states, candidates and parties
primary_result$family <- primary_result$candidate %>% sapply(function(x){strsplit(x,split=' ')[[1]][2]})#Create a last name column to join two data sets that have different names in the column
df_4$family<-df_4$CAND_NAME %>% sapply(function(x){strsplit(x,split=', ')[[1]][1]})#Create a last name column to join two data sets
df_4$family<-tolower(df_4$family)#Change to lower case to match two data sets
primary_result$family<-tolower(primary_result$family)
df_4%>%
        inner_join(primary_result,by=c("family","STATE"))->df_4#Join two data sets by the new family column
df_4%>%
        group_by(party,STATE,candidate)%>%
        summarize(donations=sum(Donations),votes=sum(state_votes))->df_4
df_4%>%
        group_by(party,STATE)%>%
        summarize(donations=sum(donations),votes=sum(votes))->df_4_1
df_4_1%>%
        inner_join(df_4, by=c("party","STATE"))->df_4
df_4%>%
        mutate(ratio_donation=donations.y/donations.x)->df_4#Divide the candidate vote to the number of votes to get the ratio 
df_4%>%
        mutate(ratio_votes=votes.y/votes.x)->df_4
colnames(df_4)<-c("party","STATE","candidate donation","candidate vote","total donations","total votes","ratio_donation","ratio_votes")
#Change the name to clean the data


#Analyzing factors that affect the results of elections
read.csv("county_facts.csv")->county_facts
county_facts%>%
        select(area_name,PST045214,SEX255214,HSG010214,EDU685213,VET605213)->county_facts
####
#The five variables I think that might impact the money raised and primary outcome is
#population, number of female,number of housing units, number of bachelor's degrees
#and number of veterans
###
read.csv("primary_results.csv")->primary_result_5
df%>%
        group_by(CITY)%>%
        summarize(donation=sum(TRANSACTION_AMT))->COUNTY_DONATION
COUNTY_DONATION$CITY<-tolower(COUNTY_DONATION$CITY)
primary_result_5$county<-tolower(primary_result_5$county)
colnames(COUNTY_DONATION)[1]<-'county'
primary_result_5%>%
        inner_join(COUNTY_DONATION,by='county')->COUNTY_DONATION
county_facts$area_name <- county_facts$area_name %>% sapply(function(x){strsplit(x,split=' County')[[1]][1]})
colnames(county_facts)[1]<-"county"
#The above codes change the names of columns so that it will be possible to merge data for analysis purposes
county_facts$county<-tolower(county_facts$county)
COUNTY_DONATION%>%
        inner_join(county_facts,by="county")->df_5_donation#Create a data frame that has information about donation along with the 5 chosen variables
primary_result_5%>%
        group_by(county,party)%>%
        summarize(votes=sum(votes))->df_5_vote
primary_result_5%>%
        group_by(county)%>%
        summarize(votes=sum(votes))->votes
votes%>%
        inner_join(county_facts,by="county")->df_5_vote#Create a data frame that has information about votes along with the 5 chosen variables
df_5_vote%>%
        select(-county)->ds_5_vote_no_county#data cleaning
df_5_donation%>%
        select(-county)->ds_5_donation_no_county
ds_5_donation_no_county%>%
        select(-state,-state_abbreviation,-fips,-party,-candidate,-votes,-fraction_votes)->ds_5_donation_no_county#data cleaning
summary(ds_5_vote_no_county) #The data model of votes and 5 chosen factors
summary(ds_5_donation_no_county) #The data model of donations and 5 chosen factors
decision_tree_donation<-rpart(donation~.,data=ds_5_donation_no_county)#The decision tree for donation as the dependent variable of 5 chosen variables   
decision_tree_vote<-rpart(votes~.,data=ds_5_vote_no_county)#The decision tree for the vote as the dependent variable of 5 chosen variables
rpart.plot(decision_tree_vote)#The plot for the decision tree
#From the plot of the decision tree of votes, we can see that counties having more veterans have more 
#people who vote. In counties that have more housing units, the number of people who vote is also significantly higher
#than counties with lower housing units
random_forest_donation <- ranger(donation~., data = ds_5_donation_no_county,importance = 'permutation')
#This is the random forest with the donation being the dependent variable of 5 chosen variables
random_forest_donation$variable.importance
#After running the random forest donation, we can see that the factor that influnces the donations the most
#is the population, followed by number of veterans, housing units, level of education and number of females
random_forest_vote<-ranger(votes~.,data=ds_5_vote_no_county,importance='permutation')
random_forest_vote$variable.importance
#After running the random forest, we can see that the most important variable that influences the number
#of votes in each county is number of housing units, followed by number of veterans, population, level of education and the least
#influential factor is number of females
#The result indicates that number of veterans appears to affect the number of donations and number of 
# votes significantly. The least important factor that affects the data appears to be the number of female.This
# may imply that gender difference does not remarkably affect the candidates' donations and the votes in general.
#What may be wrong with this model is that the 5 chosen variables may not necessarily be important and
# significantly affect the data. For instance, the number of votes and donations may increase if there are more veterans and houses in the county, 
#which mean that this is just the effect of having more people instead of the fact that people are veterans or people have high level of education
#What may be wrong with the model is that the donation graph appears uninformative, which may imply that
#the value is not distributed properly to each county and lead to too much combination of data.
