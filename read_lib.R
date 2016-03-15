library(dplyr)

## Message object has the 

# Message types


## Function to get Message vector
## Date of the message uses POSIXlt
get_messages <- function(filename) {

	lines <- tolower(readLines(filename, warn=FALSE))
	# Remove empty entries
	lines <- lines[which(lines != '' & !is.na(lines))]
	entry_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .+'

	# Merge messages until no more messages without date/time
	messages_without_time <- grep(entry_regex, lines, ignore.case=TRUE, invert=TRUE)
	# Last index analysed
	index_last <- -1
	# Destination
	dest_last <- 0
	for (i in messages_without_time) {
		if (index_last != i - 1) {
			dest_last <- i - 1
		}
		lines[dest_last] <- paste(lines[dest_last], lines[i])
		index_last <- i
	}
	lines <- lines[-messages_without_time]

	#Now we have all lines with date
	messages_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .+[:].+'
	temp_messages <- lines[grep(messages_regex, lines)]

	date_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} '
	user_regex <- '^[^:]+[:]'
	msg_regex  <- '.+'

	dates <- regmatches(temp_messages, regexpr(date_regex, temp_messages)) 
	temp_messages <- sub(date_regex, '', temp_messages)
	users <- sub(":", "", regmatches(temp_messages, regexpr(user_regex, temp_messages)))
	msgs <- sub(user_regex, '', temp_messages)

	res <- data.frame(DATE=as.POSIXlt(dates, "%d/%m/%y %H:%M:%S: "), USER=users, MESSAGE=msgs)
	tbl_df(res)
}