
## Message object has the 

# Message types
message.type.MESSAGE <- 1
message.type.USER_JOIN <- 2
message.type.USER_LEFT <- 3
message.type.USER_NUMBER_CHANGE <- 4
message.type.ICON_CHANGE <- 5
message.type.NAME_CHANGE <- 6

## Function to get Message vector
## Date of the message uses POSIXlt
get_messages <- function(filename) {

	lines <- readLines(input_file, warn=FALSE)
	# Remove empty entries
	lines <- lines[which(lines != '' & !is.na(lines))]
	message_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .+'

	# Merge messages until no more messages without date/time
	messages_without_time <- grep(message_regex, lines, ignore.case=TRUE, invert=TRUE)
	

	repeat {
		
		temp_length <- length(lines)
		no_time_indices <- grep(message_regex, lines, ignore.case=TRUE, invert=TRUE)
		lines[no_time_indices] <- paste(lines[no_time_indices - 1], lines[no_time_indices])

		if ()
	}
}