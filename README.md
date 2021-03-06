---
title: "Create a Gephi Ready Twitter Social Media Network File"
author: "Dre Dyson"
date: "4/26/2021"
output: html_document
---

This vignette covers how to use this Gephi Ready Twitter Social Media Network package. We start with creating a Twitter Developer Account. Next we provide our Twitter Developer credentials, queries, and number of tweets to the function titled 'build_gephi_adjacency'. Finally, we write this function's output as a CSV, which Gephi understands. 

In this package, we are using Twitter Users as Nodes, and Retweets as edges. 

GEPHI is a Free and Open Source network visualization tool commonly used in Academia. GEPHI allows you to explore, manipulate, and represent various techniques in Network Analysis. 


# Step 1:
To use this package, you will need to obtain a Twitter API key. 
see more here: 

<iframe width='100%' height='1000px' src='https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html' >
  <p>Your browser does not support iframes</p>
</iframe>


# Step 2:
Excute 'build_gephi_adjacency' to create a Gephi Ready Twitter Social Media Network. You will need to provide the function with the following:
 
1. Search Query
    + any character string
2. Number of Tweets
    + number of tweets to return
3. Twitter API Key
    + this will come from your Twitter Dev Account
4. Twitter Secret
    + this will come from your Twitter Dev Account
```{r example, eval=FALSE}
library(GephiReadyFile)

adjacency_list <- build_gephi_adjacency(query = "#Gamestop", numOfTweets = 1000, 
                                        twitter_api_key = "XXXXXXXX", 
                                        twitter_secret =  "XXXXXXX", app_name = "XXXXXX")
write.csv(adjacency_list, "adjacency_list.csv")

```

# Step 3:
Create a new project in Gephi, then import 'adjacency_list.csv' as a spreadsheet using the 'Data Laboratory' tab. See me here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/9UaHPniudAc" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
