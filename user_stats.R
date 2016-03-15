
# TODO: list files in chats/* and ask user which files to analyse (or all)

# Read file(s)
input_file <- "chats/group_assin.txt"
cat(sprintf("Reading file %s ...\n", input_file))
lines <- readLines(input_file, warn=FALSE)

# Remove empty entries
lines <- lines[which(lines != '' & !is.na(lines))]
cat(sprintf("%d messages read! \n", length(lines)))

# Treat messages
message_regex <- '([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .*(: .*)?'
messages_without_time <- grep(message_regex, lines, ignore.case=TRUE, invert=TRUE)

paste(lin[grep(, lin, ignore.case=TRUE, invert=TRUE) - 1], lin[grep('([0-9]{2}[/]){2}[0-9]{2} ([0-9]{2}[:]){3} .*: .*', lin, ignore.case=TRUE, invert=TRUE)])

# Messages per user

# Words per user

# Characters per user

# Custom statistics 