library(dplyr)

## Message object has the 

# Message types


## Function to get Message vector
## Date of the message uses POSIXlt
get_messages <- function(filename,model) {
  
	lines <- tolower(readLines(filename, warn=FALSE))
	# Remove empty entries
	lines <- lines[which(lines != '' & !is.na(lines))]
	
	if(model=="IOS"){
	entry_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .+'
	}else if (model=="ANDROID")	{
	 
	entry_regex <- "([0-9][0-9]?[/])([0-9][0-9]?[/])[0-9]{2}[,] ([0-9]{2}[:][0-9]{2}) [-].+"
	  
	}
	
	
	# Merge messages until no more messages without date/time
	messages_without_time <- grep(entry_regex, lines, ignore.case=TRUE, invert=TRUE)
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
	#Now we have all lines with date
	
	if(model=="IOS"){
	messages_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .+[:].+'
	}else if(model=="ANDROID"){
	messages_regex <- "([0-9][0-9]?[/])([0-9][0-9]?[/])[0-9]{2}[,] ([0-9]{2}[:][0-9]{2}) [-].+"
	
	}
	
	temp_messages <- lines[grep(messages_regex, lines)]
	
	
	
	
	
	
	if(model=="IOS"){
	date_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} '
	user_regex <- '^[^:]+[:] '
	user_regex2 <- '[^:]+[:] '
	msg_regex  <- '.+'
	}else if (model=="ANDROID"){
	  date_regex <- "([0-9][0-9]?[/])([0-9][0-9]?[/])[0-9]{2}[,] ([0-9]{2}[:][0-9]{2})"
	  user_regex <- '^[^-].+[:] '
	  user_regex<-'^ [-] ([a-z][ ]?)+[:]'
	  user_regex2<-' [-] ([a-z][ ]?)+[:]'
	  msg_regex  <- '.+'
	}
	
	##Remove entries with events (such as create group) instead of messages
	##TODO: Create list of events BROCOLLESSSSSSS
	##user_regex2 eh a regex do user sem determinar o comeco de string
	temp_messages <- temp_messages[grep(user_regex2,temp_messages)]

	
	
	if(model=="IOS"){
	dates <- regmatches(temp_messages, regexpr(date_regex, temp_messages)) 
	temp_messages <- sub(date_regex, '', temp_messages)
	users <- sub(": ", "", regmatches(temp_messages, regexpr(user_regex, temp_messages)), fixed=TRUE)
	msgs <- sub(user_regex, '', temp_messages)

	}else if(model=="ANDROID"){
	  dates <- regmatches(temp_messages, regexpr(date_regex, temp_messages)) 
	  temp_messages <- sub(date_regex, '', temp_messages)
	  
	  users <- sub(": ", "", regmatches(temp_messages, regexpr(user_regex, temp_messages)), fixed=TRUE)
	 
	  users <- sub(" - ","",users)
	 
	  msgs <- sub(user_regex, '', temp_messages)
	  
	}
	
	if(model=="IOS"){
	
	res <- data.frame(DATE=as.POSIXct(dates, "%d/%m/%y %H:%M:%S ",tz=Sys.timezone()), USER=users, MESSAGE=msgs, MESSAGE_LENGTH=sapply(msgs, 
	               function(m) { if (m == '<\U200Eimage omitted>') 0 else nchar(m) }))
	
	} else if (model== "ANDROID" ){
	  res <- data.frame(DATE=as.POSIXct(dates, "%m/%d/%y, %H:%M",tz=Sys.timezone()), USER=users, MESSAGE=msgs, MESSAGE_LENGTH=sapply(msgs, 
	                                                                                                              function(m) { if (m == '<\U200Eimage omitted>') 0 else nchar(m) }))
	}
	
	
	tbl_df(res)
}