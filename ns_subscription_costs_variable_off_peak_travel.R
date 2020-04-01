##################
# Load libraries #
##################
library(tidyverse)

#############
# Variables #
#############

# Number of days a month you will travel the specified route. Plus nu number of days you expect to go traveling during off-peak hours
days.i.travel = 15
travel.off.peak = 8 # specify the number of days to travel off-peak

# Check for days entered and calculate days travelling during peak hours
if(travel.off.peak > days.i.travel){
  message("\n ERROR: Number of days entered do not match")
} else travel.peak = days.i.travel - travel.off.peak

# Price of a single journey as indicated in ns.nl + calculate a return ticket price (= 2 times price of a single journey)
price.single.journey = 3.80
traject.costs = 2 * price.single.journey

traject.fixed  = 92 # Traveller gets 100% discount on a fixed traject


###################
# Fixed variables #
###################

# List of subscriptions plus their standard monthly costs (without travelling)

no.subscrition = 0 # You anly pay the price for the fair
free.anytime = 351 # Traveller can travel throughout the Netherlands withoud additional costs
fourty.pct = (53/12) # Traveller gets 40% discount (off-peak hours) - costs of the subscription calculated per month
twenty.fourty.pct = 23 # Traveller gets 20%discounts during peak hours & 40% discount duing off-peak hours



# Days to calculate a price for (one month has 30 days on average)
days = c(1:days.i.travel)
days.off.peak = c(1:travel.off.peak)
days.peak = c(1:(days.i.travel-travel.off.peak))


#############
# Functions #
#############

# No subscription 
 no.sub = function(x) {
   monthly.costs = x * traject.costs + no.subscrition
   return(monthly.costs)
 }

# Fixed 
fixed.sub = function(x) {
  monthly.costs = x * (traject.costs*0) + traject.fixed
  return(monthly.costs)
}

# Free anytime
free.anytime.sub = function(x) {
  monthly.costs = x * (traject.costs*0) + free.anytime
  return(monthly.costs)
}
 
# 40% off 
 fourty.pct.sub = function(y) {
   monthly.costs = (y * traject.costs*0.6) + no.sub(travel.peak) + fourty.pct
   return(monthly.costs)
 }
 
 # 20% off
 twenty.sub = function(z) {
   monthly.costs = (z * traject.costs*0.8) +twenty.fourty.pct
   return(monthly.costs)
 }
 
 # 40% off - no subscriptioncosts  
 fourty.pct2.sub = function(y) {
   monthly.costs = (y * traject.costs*0.6) + twenty.sub(travel.peak)
   return(monthly.costs)
 }
 
 ##########################
 # Monthly costs and plot #
 ##########################
 
 # Functions into one dataframe for one month of travel
 df.costs = data.frame(cbind(as.vector(days), # Number of days in a month
                       as.vector(no.sub(days)),
                       as.vector(fixed.sub(days)),
                       c(as.vector(no.sub(days.peak)),as.vector(fourty.pct.sub(days.off.peak))),
                       c(as.vector(twenty.sub(days.peak)),as.vector(fourty.pct2.sub(days.off.peak))),
                       as.vector(free.anytime.sub(days))
                       )
 )

 # Create column names of the dataframe  
colnames(df.costs) = c("day_number", 
                 "no_subscription",
                 "traject_vrij",
                 "40_pct_off",
                 "20_pct_40_pct_off",
                 "free_anytime")

#First filter if the super expensive "traject_vrij" is worth to show
if(free.anytime > 1.25*traject.fixed){
  df.costs = df.costs %>% select(-free_anytime)
}


# Datafrome in long format
df.costs.long = gather(df.costs,
                       key = "subscription",
                       value = "euro",
                       -day_number)


# Plot the costs per #days you travel for each type of subscription 


  
# Pass filtered data to ggplot
ggplot(df.costs.long)+
  geom_line(aes(x = day_number, y = euro, colour = subscription))+
  geom_vline(xintercept= days.i.travel, colour="grey") +
  geom_text(aes(x=days.i.travel, label="days_you_travel", y = 5*traject.costs), colour="black", angle=90, size = 3)+
  geom_vline(xintercept= travel.peak, colour="grey") +
  geom_text(aes(x=travel.peak, label="days_during_peak", y = 5*traject.costs), colour="black", angle=90, size = 3)

#############################
# Calculate cheapest option #
#############################

cost.my.days = as.data.frame(df.costs.long %>% filter(day_number == days.i.travel))

message("\n Costs for the number of days you travel");cost.my.days

# cheaperst option
ordered.cheap.to.expensive = cost.my.days[order(cost.my.days$euro),]

# Print the cheapest option
message("\n This is your cheapest option:");ordered.cheap.to.expensive[1,2:3]

message("\n This is your MOST EXPENSIVE option:");ordered.cheap.to.expensive[nrow(ordered.cheap.to.expensive),2:3]
