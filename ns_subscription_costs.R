#Variables 
price.single.journey = 5 
days = c(1:30)
traject.costs = 2 * price.single.journey
traject.fixed  = 92

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
   monthly.costs = days * (traject.costs*0.6)
   return(monthly.costs)
 }
 
 # Fixed 
 fixed.sub = function(days) {
   monthly.costs = days * (traject.costs*0) + traject.fixed
   return(monthly.costs)
 }

 # plot both functions
 ggplot(x = days, y = no.sub(days))+
   geom_line(aes(x=days, y=no.sub(days)), colour = "blue")+
   geom_line(aes(x = days, y = fourty.pct.sub(days)), colour = "red")+
   geom_line(aes(x = days, y = fixed.sub(days)), colour = "green")
 