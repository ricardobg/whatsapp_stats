library(dplyr)

## Function to load Message vector
## Date of the message uses POSIXlt
load_messages <- function(filename, model = 'automatic') {
  # Table with config (regex and others)
  config <- data.frame(date_regex=character(2), date_message_separator=character(2), row.names = c("android", "ios"), stringsAsFactors = FALSE)
  
  # Setting android config
  config['android', 'date_regex'] <- '([0-9][0-9]?[/]){2}[0-9]{2}([0-9]{2})?[,] ([0-9]{2}[:][0-9]{2})'
  config['android', 'date_message_separator'] <- ' -'
  
  # Setting ios config
  config['ios', 'date_regex'] <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){2}[0-9]{2}'
  config['ios', 'date_message_separator'] <- ':'
  
  #Put ^ as start
  config[, 'date_regex'] = paste('^', config[, 'date_regex'] ,sep='')  
	
	
	# Load lines
	lines <- tolower(readLines(filename, warn=FALSE, encoding='utf-8'))
	# Remove empty entries
	lines <- lines[which(lines != '' & !is.na(lines))]
	
	
	# Automatic
	if (model == 'automatic') {
	  for (m in row.names(config)) {
	    test_regex <- paste(config[m, 'date_regex'], config[m, 'date_message_separator'], ' .+$', sep = '')
	    test_messages <- grep(test_regex, lines[1])
	    # if this is the one, breaks
	    if (length(test_messages) > 0) {
	      model <- m
	      break
	    }
	  }
	}
	
	##((?:(?:[^[:punct:][:space:]])|(?:[\-]))+)\W* 
	##Para brocoli
	
	# Make regex to recognize a line
	entry_regex <- paste(config[model, 'date_regex'], config[model, 'date_message_separator'], ' .+$', sep = '')
	# Make regex to recognize MESSAGES
	message_regex <- paste(config[model, 'date_regex'], config[model, 'date_message_separator'], ' [^:]+[:] .+$', sep = '')
	
	
	# Merge messages until no more messages without date/time
	messages_without_time <- grep(entry_regex, lines, ignore.case = TRUE, invert = TRUE)
	# Last index analysed
	index_last <- -1
	# Destination
	dest_last <- 0
	if(length(messages_without_time)!=0 ){
  	for (i in messages_without_time) {
  		if (index_last != i - 1) {
  			dest_last <- i - 1
  		}
  		lines[dest_last] <- paste(lines[dest_last], lines[i])
  		index_last <- i
  	}
  	lines <- lines[-messages_without_time]
  }
	# Now we have all lines with date

	##Remove entries with events (such as create group) instead of messages
	##TODO: Create list of events BROCOLLESSSSSSS
	messages <- lines[grep(message_regex, lines)]
	events <- lines[grep(message_regex, lines, invert = TRUE)]

	# Set column date
	column_date <- regmatches(messages, regexpr(config[model, 'date_regex'] , messages))
	temp_messages <- sub(paste('^', config[model, 'date_regex'], config[model, 'date_message_separator'], ' ' ,sep=''), '', messages)
	column_users <- sub('[:] $', '', regmatches(temp_messages, regexpr('^[^:]+[:] ', temp_messages)))
	temp_messages <- sub(paste('^[^:]+[:] ' ,sep=''), '', temp_messages)
	column_messages <- temp_messages

	tbl_df(data.frame(DATE=column_date, USER=column_users, MESSAGE=column_messages))

}