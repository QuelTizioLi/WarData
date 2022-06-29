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

data <- search_tweets("from:UNHumanRightsUA Latest update on civilian casualties",
                      include_rts = FALSE)

# n <- nrow(data) #numero tweet da processare
n <- 1 #numero tweet da processare

data$text <- gsub("\\,", "", data$text) #rimuove le virgole

res <- data.frame(matrix(NA, nrow = n, ncol = 7))
colnames(res) <- c("Date", "Civilian", "KilledTotal", "KilledChildren", "InjuredTotal", "InjuredChildren", "Link")

for (i in 1:n) {
  res$Civilian[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*civilian)"))
  res$KilledTotal[i]  <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*killed)"))
  res$KilledChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children;)"))
  res$InjuredTotal[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*injured)"))
  res$InjuredChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children )"))
  res$Link[i] <- unlist(data$urls_expanded_url[i])
  
  tempDate <- unlist(str_split(res$Link[i], '-'))
  tempDate <- paste(tail(tempDate, 3), collapse = ' ')
  res$Date[i] <- as.character(as.Date(tempDate, format = '%d %b %Y'))
  
  # res$Date[i]   <- substr(as.character(data$created_at[i]), 1, 10)
  
  if(is.na(res$Civilian[i])){
    res$Civilian[i] <- res$KilledTotal[i] + res$InjuredTotal[i] 
  }
  
  if(is.na(res$Date[i])){
    tempDate <- substr(as.character(data$created_at[i]), 1, 10)
    tempDate <- as.Date(tempDate, format = '%Y-%m-%d') - 1
    res$Date[i] <- as.character(tempDate)
  }
}

res$Date <- as.character(as.Date(res$Date))

if (file.exists(here("data", "outputUN.csv"))) {
  hist_res <- read.csv(here("data", "outputUN.csv"))
  res <- unique(rbind(res, hist_res))
}

res <- na.omit(res)
res <- res[order(res$Date, decreasing = T),]
res <- unique(res)

write.csv(
  res,
  file = here("data", "outputUN.csv"),
  row.names = F,
  fileEncoding = "UTF-8"
)
