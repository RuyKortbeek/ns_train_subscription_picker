library(httr)

#####################################
# Input data for http PRICE request #
#####################################

# Assign from-and to stations to variables "A" and "B"  


A = "Alkmaar"
B = "Zaandam"

# Url to retrive data from
ns.url = as.character(paste("https://gateway.apiportal.ns.nl/public-prijsinformatie/prices?fromStation=",A,"&toStation=",B, sep = ""))

# Ocp-Apim-Subscription-Key -> should be in header pirmaire sleutel
sub.key = as.character("92fd805f312b4907840fa436a2af87df")

# Host: gateway.apiportal.ns.nl
host = as.character("gateway.apiportal.ns.nl")

################
# HTTP Resuest #
################

# Create a header to send with the request
ns.header = add_headers(.headers = 
                          c(Host = host,
                            "Ocp-Apim-Subscription-Key" = sub.key))

request.result = content( # conent() is a JSON parser - it will create a list() object
  GET(ns.url,
    ns.header)
)

##########################################################
# Get data you want (price single fare without discount) #
##########################################################

single.fare = (request.result$priceOptions[[2]]$totalPrices[[1]]$price)/100
traject.vrij = single.fare = (request.result$priceOptions[[2]]$totalPrices[[15]]$price)/100
print(single.fare)



