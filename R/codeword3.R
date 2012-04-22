codeword3 <- function(x,...) {if(x[3]<x[1]){if(x[2]<x[1]){if(x[3]<x[2]){return(1)}else{return(2)}}else{return(3)}}else{if(x[2]<x[1]){return(4)}else{if(x[3]<x[2]){return(5)}else{return(6)}}}}
