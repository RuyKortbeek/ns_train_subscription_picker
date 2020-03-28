#Variables 
price.single.journey = 5 
days = c(1:30)
traject.costs = 2 * price.single.journey
traject.fixed  = 92
fourty.pct = (53/12) #Monthyly cost to get 40% discoutn

#############
# Functions #
#############

# No subscription 
 no.sub = function(days) {
   monthly.costs = days * traject.costs
   return(monthly.costs)
 }
 
# 40% off 
 fourty.pct.sub = function(days) {
   monthly.costs = days * (traject.costs*0.6) + fourty.pct
   return(monthly.costs)
 }
 
 # Fixed 
 fixed.sub = function(days) {
   monthly.costs = days * (traject.costs*0) + traject.fixed
   return(monthly.costs)
 }

 # plot both functions
 ggplot(x = days)+
   geom_line(aes(x=days, y=no.sub(days)), colour = "blue")+
   geom_line(aes(x = days, y = fourty.pct.sub(days)), colour = "red")+
   geom_line(aes(x = days, y = fixed.sub(days)), colour = "green")

 # Functions into dataframes
 