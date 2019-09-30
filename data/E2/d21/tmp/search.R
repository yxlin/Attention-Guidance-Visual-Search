rm(list=ls())
# -----------------------   
# DEFINE some variables
#************************
angle <- seq(from = -pi/2 + pi/12.5, to = 3*pi/2, by = pi/12.5)   # Calculate the angles
positiontable <- cbind( cos(angle), sin(angle) )
source('searchTabFun.R')

iLength <- 101
setsizeSet <- c(3, 6, 12, 18)
bRSize <- length(setsizeSet)*2  

minsetsize = 5
maxsetsize = 20

# -----------------
# START
#******************
for ( idx in 1:5 ) {
  filename <- paste('./tables/ranTab', idx, sep='')
  for ( i in 1:iLength ) # for each repetition of this condition
  { # countervalue is used to randomise trial within a block
    previousSetpoint <- countervalue     
    for (setsize in setsizeSet) 
    {
    
      # target present trials:
     
      stimulipos = sample( c(1:12,14:25) , setsize )  
      #take the first one as the target, the rest as distractors
      stimuli=c("target",sample( c("distractor1","distractor2"),replace=T,(setsize-1)))
      cat("  ",counter(),"1",setsize," ", file="./tables/nonRanTab", append=T)
      printstimuli( stimuli , stimulipos )
      printemptystimuli( maxsetsize - setsize )
      cat("\n", file="./tables/nonRanTab", append=T)

      # target absent trials:
 
      stimulipos = sample( c(1:12,14:25) , setsize )  
      # all distractors
      stimuli=sample( c("distractor1","distractor2"),replace=T,(setsize))
      cat("  ",counter(),"7",setsize," ", file="./tables/nonRanTab", append=T)
      printstimuli( stimuli , stimulipos )
      printemptystimuli( maxsetsize - setsize )
      cat("\n", file="./tables/nonRanTab", append=T)
    }
    # Randomize the trail sequence. You may want to change this to a function
    # 'size' here refers to the size of one block.
    # Put these codes inside the repetition loop to ensure its 'within-block' randomization
    tabTmp <- read.table("./tables/nonRanTab", colClasses = rep('character', 5), header=F)
  
    #-------------------------------------------------------------------------
    # change the replace=TRUE to FALSE, if you do not to have replication    
    #--------------------------------------------------------------------------
    blockRandomise <- tabTmp[sample(previousSetpoint:nrow(tabTmp), replace=F, size=bRSize, 
                                  prob=c(rep(c(.50, .50), bRSize/2))), ]
    write.table(blockRandomise, file=filename, col.names=F, row.names=F, quote=F, append=T)
  }
  system("rm ./tables/nonRanTab")  
  countervalue <- 1
}
     
