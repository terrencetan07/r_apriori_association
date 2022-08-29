############################
####### Section 1 Q2 #######
############################
# section 1 Q2
setwd(q2_directory)
q2_data<-read.csv("records-of-hdb-flat-resale-transactions-closed-by-salespersons.csv",stringsAsFactors = FALSE)
setwd(working_directory)

#################################################################################################################
# task1
# Based on the dataset, how many sales would you expect an agent to close each year? How much variation is there among agents?
# data manipulation
q2_data<-q2_data%>%
  mutate("year"=year(mdy(complete_date_txt)))

q2_summary_data<-q2_data%>%
  group_by(year,salesperson_reg_no,salesperson_name)%>%
  summarise(n=n())

q2_summary_output<-q2_summary_data%>%
  group_by(year)%>%
  summarise(mean=mean(n),
            sd=sd(n),
            n_agents=n())
print(q2_summary_output)
#################################################################################################################
# task2
# Examine the distribution for number of sales closed by an agent in a year & suggest a probability distribution
# that may be suitable for modelling this set of values. 
ggplot(q2_summary_data[q2_summary_data$year==2018,],aes(x=n,fill=year))+
  geom_histogram(binwidth = 2)+
  labs(title="2018 Histogram of Sales Transaction")+
  xlim(c(0,50))
# choose 2018
q2_2018<-q2_summary_data%>%
  filter(year==2018)
#run distribution test
gpd.fit(q2_2018$n,"combined")#this is answer
gpd.test(q2_2018$n)
#################################################################################################################
# task3
# Given a property agent who has closed sales in Sembawang and Yishun during a given year, which other town is he/she
# most likely to be active in that year? 
table(q2_data$year)
q2_data_2018<-q2_data%>%
  dplyr::select(town_txt,salesperson_reg_no,year)%>%
  filter(year==2018)

txn_sorted <- q2_data_2018[order(q2_data_2018$salesperson_reg_no),]
txn_itemList <- plyr::ddply(q2_data_2018,c("salesperson_reg_no"), 
                      function(df1)paste(df1$town_txt, 
                                         collapse = ","))
txn_itemList$salesperson_reg_no <- NULL
colnames(txn_itemList) <- c("town")

#write csv
setwd(q2_directory)
write.csv(txn_itemList,"txt_itemList.csv", quote = FALSE, row.names = TRUE)

#read csv to txn type
txn = read.transactions(file="txt_itemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
setwd(working_directory)

#run apriori
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
rules <- apriori (data=txn, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="rhs",lhs=c("SEMBAWANG","YISHUN")), control = list (verbose=F))
arules::inspect(head(rules))
