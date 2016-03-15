## File

file <- "kenzo.txt"



## Create connection
con <- file(description=file, open="r")

## Loop over a file connection

tmp <- readLines(file)


##nome <- vector("list",length(tmp))

##mensagem <- vector("list",length(tmp))
nome <- c()
mensagem <- c()
for (i in 1:length(tmp)){
  
  nomeEmensagem <- strsplit(tmp[[i]],"-")[[1]][2]  ## Elimina data e hora do resto da mensagem
  nome[[i]] <- strsplit(nomeEmensagem,":")[[1]][1] ## Separa o nome do corpo da mensagem
  mensagem[[i]] <- strsplit(nomeEmensagem,":")[[1]][2] ## Separa o nome do corpo da mensagem
  
}


tabelaWhats <- data.frame(NOMES = nome, MENSAGENS = mensagem)
tabelaWhats2 <- tbl_df(tabelaWhats)
##Oloko <- filter(tabelaWhats2,grepl("(o|O)loo*(c|k)o",MENSAGENS)
##count(Oloko,NOMES)