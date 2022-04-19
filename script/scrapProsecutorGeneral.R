library(rtweet)
library(tesseract)
library(stringr)
library(here)
library(magick)

eng <- tesseract("eng")

# Create a token containing the Twitter keys
token <- create_token(
  app = "CarloTwitter",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

data <- search_tweets("from:GP_Ukraine #RussianWarCrimes",
                      include_rts = FALSE)
data <- data[startsWith(data$text, "#"),]

n <- nrow(data) #numero tweet da processare

res <- data.frame(matrix(NA, nrow = n, ncol = 4))
colnames(res) <- c("Date", "KilledChildren", "InjuredChildren",  "Link")

for (i in 1:n) {
  
  media <- unlist(data$media_url[i])
  
  img <- image_read(media) %>% 
    image_convert(type = 'Grayscale') %>% 
    # image_crop("450x300+600+480") %>% 
    image_negate()
  
  text <- tesseract::ocr(img, engine = eng)
  text <- gsub("\\,", "", text) #rimuove le virgole
  
  res$Date[i]   <- substr(as.character(data$created_at[i]), 1, 10)
  
  res$KilledChildren[i] <- as.numeric(str_extract(text, "(?i)(?<=KILLED:)\\d+"))
  if(is.na(res$KilledChildren[i])){
    res$KilledChildren[i] <- as.numeric(str_extract(text, "(?i)(?<=KILLED: )\\d+"))
  }
  res$InjuredChildren[i] <- as.numeric(str_extract(text, "(?i)(?<=INJURED: )\\d+"))
  if(is.na(res$InjuredChildren[i])){
    res$InjuredChildren[i] <- as.numeric(str_extract(text, "(?i)(?<=INJURED:)\\d+"))
  }
 
  res$Link[i] <- media
}

res$Date <- as.character(as.Date(res$Date))

if (file.exists(here("data", "crimeChildren.csv"))) {
  hist_res <- read.csv(here("data", "crimeChildren.csv"))
  res <- unique(rbind(res, hist_res))
}

res <- na.omit(res)
res <- res[order(res$Date, decreasing = T),]

write.csv(
  res,
  file = here("data", "crimeChildren.csv"),
  row.names = F,
  fileEncoding = "UTF-8"
)

