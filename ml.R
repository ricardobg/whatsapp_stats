# Machine learning

learn.learn_from_messages <- function (messages) {
    #TODO: learn from messages and return structure using logistic regression
}

lear.predict <- function (learn_structure, message) {
    #TODO: Predict who sent the message
}


costFunction <- function(theta,X,y,lambda ){
  
  
  resultados <- sigmoid(X%*%theta);
  m <- size(y)[1];
  
  
  J <- (1/m) *  (-y*log(resultados)-(1.-y)*log(1.-resultados));
  
  return (sum(J))##retornar custo
  
}

grad <- function(theta, X, y, lambda){ ##nao sei ainda se funciona!
  resultados <- sigmoid(X%*%theta);
  m <- size(y)[1]
 
  
  grad <- (1/m) *  t(X)%*%(resultados-y);
  
  return (grad)
}



