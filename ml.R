# Machine learning

learn.learn_from_messages <- function (messages) {
    #TODO: learn from messages and return structure using logistic regression
}

lear.predict <- function (learn_structure, message) {
    #TODO: Predict who sent the message
}


costFunction <- function(theta,X,y ){
  
  
  resultados <- sigmoid(X%*%theta);
  m <- size(y)[1]
  
  
  J <- (1/m) *  (-y*log(resultados)-(1.-y)*log(1.-resultados));
  
  return (sum(J))##retornar custo
  
}



