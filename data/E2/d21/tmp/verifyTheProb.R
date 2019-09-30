x <- read.table('blockRandomised.txt')
head(x)
tLength <- nrow(x)
tmp <- gsub(pattern='[A-Z]', replacement='1', x[,5])
upperL <- sum( ifelse(tmp =='1', 1, 0) )
tmp <- gsub(pattern='w[a-z]', replacement='1', x[,5])
lowerL <- sum( ifelse(tmp =='1', 1, 0) )
c(upperL,lowerL) / tLength
        

        
        