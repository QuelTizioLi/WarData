library(rtweet)
library(stringr)

data <- search_tweets("from:UNHumanRightsUA",
                      include_rts = FALSE)

n <- nrow(data) #numero tweet da processare

data$text <- gsub("\\,", "", data$text) #rimuove le virgole

res <- data.frame(matrix(NA, nrow = n, ncol = 7))
colnames(res) <- c("Data", "Civilian", "KilledTotal", "KilledChildren", "InjuredTotal", "InjuredChildren", "Link")

for (i in 1:n) {
  res$Data[i]   <- substr(as.character(data$created_at[i]), 1, 10)
  res$Civilian[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*civilian)"))
  res$KilledTotal[i]  <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*killed)"))
  res$KilledChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children;)"))
  res$InjuredTotal[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*injured)"))
  res$InjuredChildren[i] <- as.numeric(str_extract(data$text[i], "[0-9]+(?=\\s*children )"))
  res$Link[i] <- unlist(data$urls_expanded_url[i])
}

res$Data <- as.character(as.Date(res$Data))

if (file.exists("output.csv")) {
  hist_res <- read.csv("output.csv")
  res <- unique(rbind(res, hist_res))
}

write.csv(
  res,
  file = "output.csv",
  row.names = F,
  fileEncoding = "UTF-8"
)
