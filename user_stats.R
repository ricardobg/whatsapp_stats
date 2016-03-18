library(dplyr)

# Custom statistics 
users.count_ocurrencies <- function (messages, regex) {
  ocurrencies <- sapply(gregexpr(regex, messages$MESSAGE), function (el) { if (el[1] == -1) 0 else length(el) })
  temp <- tbl_df(data.frame(USER=messages$USER,COUNT=ocurrencies))
  summarize(group_by(temp, USER), COUNT=sum(COUNT))
}


# Messages per user
users.count_messages <- function (messages) {
  count(messages, USER)
}

# Words per user
users.count_words <- function (messages) {
  
}

# Characters per user
users.count_characters <- function (messages, letters.only = TRUE) {
  
}

