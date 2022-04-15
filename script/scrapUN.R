library(rtweet)
library(stringr)
library(here)

# Create a token containing the Twitter keys
token <- create_token(
  app = "CarloTwitter",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

data <- search_tweets("from:UNHumanRightsUA",
                      include_rts = FALSE,
                      token = token)

n <- nrow(data) #numero tweet da processare

data$text <- gsub("\\,", "", data$text) #rimuove le virgole

res <- data.frame(matrix(NA, nrow = n, ncol = 7))
colnames(res) <- c("Date", "Civilian", "KilledTotal", "KilledChildren", "InjuredTotal", "InjuredChildren", "Link")

for (i in 1:n) {
  res$Date[i]   <- substr(as.character(data$created_at[i]), 1, 10)
  res$Civilian[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*civilian)"))
  res$KilledTotal[i]  <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*killed)"))
  res$KilledChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children;)"))
  res$InjuredTotal[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*injured)"))
  res$InjuredChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children )"))
  res$Link[i] <- unlist(data$urls_expanded_url[i])
}

res$Date <- as.character(as.Date(res$Data))


if (file.exists(here("data", "outputUN.csv"))) {
  hist_res <- read.csv(here("data", "outputUN.csv"))
  res <- unique(rbind(res, hist_res))
}

write.csv(
  res,
  file = here("data", "outputUN.csv"),
  row.names = F,
  fileEncoding = "UTF-8"
)
