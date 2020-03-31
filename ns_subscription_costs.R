#############
# Variables #
#############

# Price of a single journey as indicated in ns.nl 
price.single.journey = 5 

# Price for one day of travel (= 2 times price of a single journey)
traject.costs = 2 * price.single.journey

# Days to calculate a price for (one month has 30 days on average)
days = c(1:30)

# Number of days a month you will travel the specified route
days.i.travel = 20

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
 

 # plot both functions
 ggplot(x = days)+
   geom_line(aes(x=days, y=no.sub(days)), colour = "blue")+
   geom_line(aes(x = days, y = fourty.pct.sub(days)), colour = "red")+
   geom_line(aes(x = days, y = fixed.sub(days)), colour = "green")

 # Functions into dataframe of one month of travel
 df = data.frame(cbind(as.vector(1:30), # Number of days in a month
                       as.vector(no.sub(days)),
                       as.vector(fixed.sub(days)),
                       as.vector(fourty.pct.sub(days))
                       )
 )
 
colnames(df) = c("Day_number", 
                 "no_subscription",
                 "fixed",
                 "40_pct_off")



      