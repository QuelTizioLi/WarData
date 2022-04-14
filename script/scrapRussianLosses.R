library(rtweet)
library(tesseract)
library(stringr)

eng <- tesseract("eng")

# Create a token containing the Twitter keys
token <- create_token(
  app = "CarloTwitter",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

data <- search_tweets("from:KyivIndependent indicative estimates of Russia's combat losses",
                      include_rts = FALSE,
                      token = token)

n <- nrow(data) #numero tweet da processare


res <- data.frame(matrix(NA, nrow = n, ncol = 16))
colnames(res) <- c("Data", "Troops", "MLRS", "Planes", "Boats", "Helicopters", "Vehicles", "Tanks",
                   "FuelTanks", "ArtilleryPieces", "UAV", "ArmoredPersonnelCarriers",
                   "AntiAircraftWarfare", "MobileSRBMSystems", "SpecialEquipment",  "Link")

for (i in 1:n) {
  
  media <- unlist(data$media_url[i])
  text <- tesseract::ocr(media, engine = eng)
  text <- gsub("\\,", "", text) #rimuove le virgole
  
  res$Data[i]   <- substr(as.character(data$created_at[i]), 1, 10)
  
  res$Troops[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*troops)"))
  res$MLRS[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*MLRS)"))
  res$Planes[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*planes)"))
  res$Boats[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*boats)"))
  res$Helicopters[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*helicopters)"))
  res$Vehicles[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*vehicles)"))
  res$Tanks[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*tanks)"))
  res$FuelTanks[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*fuel)"))
  res$ArtilleryPieces[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*artillery)"))
  res$UAV[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*UAV)"))
  res$ArmoredPersonnelCarriers[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*armored)"))
  res$AntiAircraftWarfare[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*anti)"))
  res$MobileSRBMSystems[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*mobile)"))
  res$SpecialEquipment[i] <- as.numeric(str_extract(text, "[0-9]+(?=\\s*special)"))
 
  res$Link[i] <- media
}

res$Data <- as.character(as.Date(res$Data))


if (file.exists(here("data", "russianLosses.csv"))) {
  hist_res <- read.csv(here("data", "russianLosses.csv"))
  res <- unique(rbind(res, hist_res))
}

write.csv(
  res,
  file = here("data", "russianLosses.csv"),
  row.names = F,
  fileEncoding = "UTF-8"
)

