#############
# Variables #
#############

# Number of days a month you will travel the specified route
days.i.travel = 16

# Price of a single journey as indicated in ns.nl 
price.single.journey = 9.90

# Montly costs for the subscription (without travelling)
traject.fixed  = 254.8 # Traveller gets 100% discount on a fixed traject

no.subscrition = 0 # You anly pay the price for the fair
free.anytime = 351 # Traveller can travel throughout the Netherlands withoud additional costs
fourty.pct = (53/12) # Traveller gets 40% discount (off-peak hours) - costs of the subscription calculated per month
twenty.fourty.pct = 23 # Traveller gets 20%discounts during peak hours & 40% discount duing off-peak hours

# Price for one day of travel (= 2 times price of a single journey)
traject.costs = 2 * price.single.journey

# Days to calculate a price for (one month has 30 days on average)
days = c(1:30)


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
 fourty.pct.sub = function(x) {
   monthly.costs = x * (traject.costs*0.6) + fourty.pct
   return(monthly.costs)
 }
 
 # 20%-40% off. Assumed is that you travel half the time during peak hours, other half off-peak hours
 twenty.fourty.sub = function(x) {
   x = days
   monthly.costs = (x * 0.5 * (traject.costs*0.6) + x * 0.5 * (traject.costs*0.8)+ twenty.fourty.pct)
   return(monthly.costs)
 }
 
 ##########################
 # Monthly costs and plot #
 ##########################
 
 # Functions into one dataframe for one month of travel
 df.costs = data.frame(cbind(as.vector(1:30), # Number of days in a month
                       as.vector(no.sub(days)),
                       as.vector(fixed.sub(days)),
                       as.vector(fourty.pct.sub(days)),
                       as.vector(twenty.fourty.sub(days)),
                       as.vector(free.anytime.sub(days))
                       )
 )

 # Create column names of the dataframe  
colnames(df.costs) = c("day_number", 
                 "no_subscription",
                 "fixed",
                 "40_pct_off",
                 "20_pct_40_pct_off",
                 "free_anytime")

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

message("\n This is your MOST EXPENSIVE option:");ordered.cheap.to.expensive[nrow(ordered.cheap.to.expensive),2:3]



      