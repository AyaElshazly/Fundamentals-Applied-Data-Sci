fact = read.csv("customer_churn.csv")

revenue_cube <- 
  tapply(fact$Total.Revenue, 
         fact[,c("Contract", "Offer", "Internet.Type","Customer.Status")], 
         FUN=function(x){return(sum(x))})


sub_data =apply(revenue_cube, c("Contract", "Offer","Internet.Type"),
                FUN=function(x) {return(sum(x, na.rm=TRUE))}) 

sub_data["Two Year" , , ]


total = sum(revenue_cube[,"Offer B",,],na.rm = TRUE)
churned = revenue_cube["Month-to-Month","Offer B","Cable","Churned"]
churned/total *100