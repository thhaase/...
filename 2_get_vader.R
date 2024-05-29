library(tidyverse)
library(vader)


time_vader <- system.time({
  vader_scores <- vader_df(data$abstract)
})

write.csv(vader_scores,"data/vader.csv")
