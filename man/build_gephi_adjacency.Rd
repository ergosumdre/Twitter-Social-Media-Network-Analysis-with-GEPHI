% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GephiReadyFile.R
\name{build_gephi_adjacency}
\alias{build_gephi_adjacency}
\title{Create Gephi Ready File for Twitter Social Media Network Analysis}
\usage{
build_gephi_adjacency(
  query,
  numOfTweets,
  twitter_api_key,
  twitter_secret,
  app_name
)
}
\arguments{
\item{query}{character string}

\item{numOfTweets}{integer}

\item{twitter_api_key}{character string}

\item{twitter_secret}{character string}

\item{app_name}{character string}
}
\value{
dataframe
}
\description{
Create Gephi Ready File for Twitter Social Media Network Analysis.
This package will help users generate a Gephi ready csv file. We will ultize Twitter's API to search for tweets, given a user's search query. Then once we receive these tweets, this package will create a list of Usernames and Retweeter's usernames. From this list, we build an adjacency list, which a Twitter User is a node, and a Retweet is an edge. This package then writes the adjacency list as a CSV that GEPHI recognizes and accepts as an input. GEPHI is a Free and Open Source network visualization tool commonly used in Academia. GEPHI allows you to explore, manipulate, and represent various techniques in Network Analysis.
}
