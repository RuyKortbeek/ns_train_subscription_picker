#############
# Variables #
#############

# Number of days a month you will travel the specified route
days.i.travel = 20

# Price of a single journey as indicated in ns.nl 
price.single.journey = 5 

# Price for one day of travel (= 2 times price of a single journey)
traject.costs = 2 * price.single.journey

# Days to calculate a price for (one month has 30 days on average)
days = c(1:30)

# Montly costs for the subscription (without travelling)
traject.fixed  = 92 # Traveller gets 100% discount on a fixed traject
fourty.pct = (53/12) # Traveller gets 40% discount (off-peak hours) 

#############
# Functions #
#############

# No subscription 
 no.sub = function(days) {
   monthly.costs = days * traject.costs
   return(monthly.costs)
 }

# Fixed 
fixed.sub = function(days) {
  monthly.costs = days * (traject.costs*0) + traject.fixed
  return(monthly.costs)
}
 
# 40% off 
 fourty.pct.sub = function(days) {
   monthly.costs = days * (traject.costs*0.6) + fourty.pct
   return(monthly.costs)
 }
 
 ##########################
 # Monthly costs and plot #
 ##########################
 
 # Functions into one dataframe for one month of travel
 df.costs = data.frame(cbind(as.vector(1:30), # Number of days in a month
                       as.vector(no.sub(days)),
                       as.vector(fixed.sub(days)),
                       as.vector(fourty.pct.sub(days))
                       )
 )

 # Create column names of the dataframe  
colnames(df.costs) = c("day_number", 
                 "no_subscription",
                 "fixed",
                 "40_pct_off")

# Datafrome in long format
df.costs.long = gather(df.costs,
                       key = "subscription",
                       value = "euro",
                       -day_number)

# Plot the costs per #days you travel for each type of subscription 
ggplot(df.costs.long)+
  geom_line(aes(x = day_number, y = euro, colour = subscription))+
  geom_vline(xintercept= days.i.travel, colour="grey") +
  geom_text(aes(x=days.i.travel, label="days_you_travel", y = price.single.journey), colour="black", angle=0, size = 3)

#############################
# Calculate cheapest option #
#############################

cost.my.days = as.data.frame(df.costs.long %>% filter(day_number == days.i.travel))

message("\n Costs for the number of days you travel");cost.my.days

# cheaperst option
ordered.cheap.to.expensive = cost.my.days[order(cost.my.days$euro),]

# Print the cheapest option
message("\n This is your cheapest option:");ordered.cheap.to.expensive[1,2:3]


      