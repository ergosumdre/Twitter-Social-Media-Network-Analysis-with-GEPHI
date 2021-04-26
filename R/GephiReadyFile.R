library("dplyr")
library("readr")
library("rtweet")

#' Create Gephi Ready File for Twitter Social Media Network Analysis
#'
#' @param query character string
#' @param numOfTweets integer
#' @param twitter_api_key character string
#' @param twitter_secret character string
#' @param app_name character string
#'
#' @return dataframe
#' @export
#'

build_gephi_adjacency <- function(query,
                                  numOfTweets,
                                  twitter_api_key,
                                  twitter_secret, app_name){

  ## store api keys (these are fake example values; replace with your own keys)
  api_key <- twitter_api_key
  api_secret_key <- twitter_secret

  ## authenticate via web browser
  twitter_token <- create_token(
    app = app_name,
    consumer_key = api_key,
    consumer_secret = api_secret_key)

  get_tweets <- rtweet::search_tweets(q = query,
                                      n = numOfTweets,
                                      token = twitter_token)

  adjacency_list <- get_tweets %>% dplyr::filter(lang == "en")
  adjacency_list <- adjacency_list %>% dplyr::select(screen_name, retweet_screen_name)
  adjacency_list <- adjacency_list %>% dplyr::filter(!is.na(retweet_screen_name))
  adjacency_list <- adjacency_list %>% dplyr::group_by(screen_name, retweet_screen_name) %>% dplyr::tally()
  adjacency_list <- adjacency_list %>% dplyr::rename(Target = "screen_name",
                                                     Source = "retweet_screen_name",
                                                     Weight = "n")
  adjacency_list$Type <- rep("Directed", nrow(adjacency_list))
  return(adjacency_list)
}
