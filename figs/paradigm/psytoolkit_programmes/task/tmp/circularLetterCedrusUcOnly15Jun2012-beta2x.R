# --------------------------------------------------------------------------------------
# This programme is modified from Dr. Gijsbert Stoet's visual search task 
# The drafting of first beta version started on 10th of Dec and was finished on 19th of December, 
# 2011. Author: Yi-Shin Lin
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# The first column of "positiontablex" is x coordinate, and the second is the y 
# coordinate. 
# Below lines present the search set in a circular fashion 30 May, 2012
# --------------------------------------------------------------------------------------
rm(list=ls())

angle <- seq(from = -pi/2 + pi/10, to = 3*pi/2, by = pi/10)   # Calculate the angles
positiontable <- cbind( cos(angle), sin(angle) )
source('searchTabFun.R')

# The maxsetsize should set at 20 because the number of presented images were manually coded in psy file
minsetsize <- 3
maxsetsize <- 20

iLength <- 66                        # numbers of repetition
isiSet <- c(50, 400)                 # c(10, 30, 50)
setsizeSet <- c(5, 9)
targetPosSet <- c(5, 6)              # 5-left; 6-right
cueSet <- c(1)                       # 1-upper; 2-lower
congrSet <- c(1)                     # congrSet = c(1, 2); 1-cong; 2-incong
competingSet <- c(0)                 # compete == 1; no competition == 0.

bRSize <- length(isiSet) * length(setsizeSet) * length(targetPosSet) * length(cueSet) *
  length(competingSet) * length(congrSet)

sameTarget <- FALSE #TRUE 
whichLetter <- 'G'
#---------------------------------------------------------------------------------------

idx=1
# -----------------
# START
#******************
for ( idx in 1:2 ) {
  filename <- paste('./tables/ranTab', idx, sep='')
  for ( i in 1:iLength )   
  {  # i = 1
    previousSetpoint <- countervalue          
    for (isi in isiSet) 
    {  # isi = 50 
      for (setsize in setsizeSet)             
      {  # setsize = 3
      for (targetPos in targetPosSet)       # 5-left; 6-right
        { # targetPos = 5
        for (cue in cueSet)                 # 1-uppercase; 2-lowercase 
          { # cue = 1
          for (congr in congrSet)
            { # congr = 1
       
          samplePosPoolR <- c(1:9) 
          samplePosPoolL <- c(11:19)
          
          #---------------------------------------------------
          # decide target is on the right or left side 
          #---------------------------------------------------
          if (targetPos==5)  {                       # if target is on the left side.
             targetPosCode    <- sample(samplePosPoolL, 1)
               
             distractor.samplePosPoolL <- samplePosPoolL[!(samplePosPoolL == targetPosCode)]
             distractor.samplePosPoolR <- samplePosPoolR
               
          } else {                                   # if target is on the right side.
             targetPosCode    <- sample( samplePosPoolR, 1)
             
             distractor.samplePosPoolR <- samplePosPoolR[!(samplePosPoolR == targetPosCode)]
             distractor.samplePosPoolL <- samplePosPoolL
          }
          distractorPosPoolRest <- c(distractor.samplePosPoolR, distractor.samplePosPoolL) 
          
          # counter() is used later to randomize the trial sequence.
          # 13 letters; one for target and cue; others as distractors
          # "A" "B" "D" "E" "F"   "G" "H" "J"    "M"  "N"   "Q"  "R"   "T"
          # "1" "2" "4" "5" "6"   "7" "8" "10"   "13" "14"  "17" "18"  "20"
          letterPool <- LETTERS[c(1, 2, 4, 5, 6, 7, 8, 10, 13, 14, 17, 18, 20) ]
          target <- targetUpper <- ifelse(sameTarget == T, whichLetter, sample(letterPool, 1))
          distractorPool <- letterPool[!(letterPool == target)]

          # (1)----- if congruent. cue uc - target uc; cue lc - target lc.
          if (congr == 1) {        

            #--------------------------------              
            # Upperrcase cue
            #--------------------------------              
            if (cue  == 1) {         #(1-1)--- uc-cue matches uc-target and uc-distractors
              cueImg <- targetUpper

            #--------------------------------              
            # Lowercase cue
            #--------------------------------              
            } else {                 #(1-2)--- cue == 2; lc-cue matches lc-target and lc-distractors 
              cueImg <- target <- paste('w', tolower(targetUpper), sep='')
              distractorPool <- paste('w', tolower(distractorPool), sep='')    # convert all distractors into lowercase
            }                        # end of 1-2

        #--------------------------------              
        # Incongruent
        #--------------------------------
          } else {          #(2)--- incongruent.  cue uc - target lc; cue lc - target uc.

            #--------------------------------              
            # Upperrcase cue
            #--------------------------------              
            if (cue == 1) {          #(2-1)--- uc-cue matches lc-target and lc-distractors, 
              target <- paste('w', tolower(targetUpper), sep='')
              cueImg <- targetUpper
              distractorPool <- paste('w', tolower(distractorPool), sep='')    # convert all distractors into lowercase
              
            } else {                #(2-2)--- lc-cue matches uc-target and uc-distractors,
              
            #--------------------------------              
            # lowercase cue
            #--------------------------------              
              target <- targetUpper
              cueImg <- paste('w', tolower(target), sep='')
            }         
          }                         

          # @1 is counter(); @2 indicates target position (left-1, right-2); @3 is set size
          # @4 records the duration ISI in milliseconds; @5 is the target letter; @6 is letter case. 
          # @37 is target letter. @38 and @39 are its coordinate. 
          cat("  ",counter(), targetPos, setsize, isi, cueImg, cue, congr, " ", file="./tables/nonRanTab", append=T)
          
          #--------------- Printing------------------------------
          # remove target and competing from the setsize
          stimulipos <- sample(distractorPosPoolRest, setsize - 2)    

          if(previewSize == 1) {
                 previewDistractorPos <- sample(distractorPosPoolRest, 1)       # sample one distractor position for the preview distractor
                 printstimuli( previewSet, previewDistractorPos )
                 stimuliposExcludePreview <- distractorPosPoolRest[!(distractorPosPoolRest == previewDistractorPos)]
          } else {
                 # sample as many distractors as needed, defined by the length of preview size
                 previewDistractorPos <- sample(distractorPosPoolRest, previewSize)  
                 printstimuli( previewSet, previewDistractorPos ) 
                 exclude  <- numeric(length(distractorPosPoolRest))
                 for (j in 1:previewSize) { exclude <- exclude | distractorPosPoolRest == previewDistractorPos[j] }
                 stimuliposExcludePreview <- distractorPosPoolRest[!exclude]
          }
          printemptystimuli( maxsetsize/2 - previewSize )
          #--------------- Printing------------------------------
 
          # Filter out those letters that have been used in preview set
          exclude  <- numeric(length(distractorPool))   
          for (j in 1:previewSize) { exclude <- exclude | distractorPool == previewSet[j] }
          distractorPool.unused <- distractorPool[!exclude]   # you can check previewSet to see if this is accurate
  
          targetSetSize <- setsize - previewSize
          
          #-------------------------------------------------------------------------
          # start to print the target set 
          #-------------------------------------------------------------------------
          if (setsize <= 3) {          # setsize 3 is the minimual
             #-----------------------------
             # show competing distractor
             #-----------------------------
             if(competing == 1) {           

               if(congr == 1) {
                  competingDistractor <- ifelse(cue == 1, paste('w', tolower(target), sep=''),  toupper(substr(target, 2,2)))
               } else {                     # incongruency
                  competingDistractor <- ifelse(cue == 1, toupper(substr(target,2,2)), paste('w', tolower(target), sep=''))
               }
               #--------------- Printing------------------------------
               printstimuli( target, targetPosCode )
               printstimuli( competingDistractor, competingPosCode)
               printemptystimuli( maxsetsize/2 - targetSetSize )
               #--------------- Printing------------------------------
             
             #-----------------------------
             # no competing distractor
             #-----------------------------
             } else {                      
               targetSet <- c(sample( distractorPool.unused , replace=F, 1))
               #--------------- Printing------------------------------
               printstimuli( target, targetPosCode )
               printstimuli( targetSet, competingPosCode )
               printemptystimuli( maxsetsize/2 - targetSetSize )
               #--------------- Printing------------------------------
             }
          }
          
          #----------------------------
          # set size is larger than 3
          #----------------------------
          if (setsize > 3) {

            stimuliposExcludePreviewL <- stimuliposExcludePreview[stimuliposExcludePreview > 9]
            stimuliposExcludePreviewR <- stimuliposExcludePreview[stimuliposExcludePreview < 9]
            #----------------------------
            # set size is even number
            #----------------------------
                if (setsize %% 2 == 0) {
                      if(competing == 1) {              # show competing distractor
                          
                          if(congr == 1) {
                            competingDistractor <- ifelse(cue == 1, paste('w', tolower(target), sep=''),  toupper(substr(target, 2, 2)))
                          } else {                     # incongruency
                            competingDistractor <- ifelse(cue == 1, toupper(substr(target,2,2)), paste('w', tolower(target), sep=''))
                          }
                          printstimuli( target, targetPosCode )
                          printstimuli( competingDistractor, competingPosCode)

                          if (setsize == 4) {
                            printemptystimuli( maxsetsize/2 - targetSetSize )
                          }
                  
                          #-----------------------------------------
                          # Handle the setsize == 4 and setsize >4
                          #-----------------------------------------
                          if (setsize > 4) {
                            restOftargetsetDistractors <- targetSetSize - 2

                            if (restOftargetsetDistractors == 1) {
                              targetSet <- sample( distractorPool.unused, replace=F, 1 )
                              if (sample(c('R', 'L') , 1) == 'R') {
                                restOfPositions <- sample(stimuliposExcludePreviewR, 1)
                              } else {
                                restOfPositions <- sample(stimuliposExcludePreviewL, 1)
                              }
                              printstimuli( targetSet, restOfPositions )
                              printemptystimuli( maxsetsize/2 - targetSetSize )
                            } else {
                              targetSet <- sample( distractorPool.unused, replace=F, restOftargetsetDistractors )
                              if(targetPos == 5) {
                                restOfPositionsR <- sample(stimuliposExcludePreviewR, ceiling(restOftargetsetDistractors/2))
                                restOfPositionsL <- sample(stimuliposExcludePreviewL, floor(restOftargetsetDistractors/2))
                              } else {   #targetPos == 6, namely right
                                restOfPositionsR <- sample(stimuliposExcludePreviewR, floor(restOftargetsetDistractors/2))
                                restOfPositionsL <- sample(stimuliposExcludePreviewL, ceiling(restOftargetsetDistractors/2))
                              }
                              restOfPositions <- c(restOfPositionsR, restOfPositionsL)
                              printstimuli( targetSet, restOfPositions )
                              printemptystimuli( maxsetsize/2 - targetSetSize )
                            }
                          }
                         
                          
                      } else {                   # no competing distractor
                          printstimuli( target, targetPosCode )
                          restOftargetsetDistractors <- targetSetSize - 1
                          targetSet <- sample( distractorPool.unused, replace=F, restOftargetsetDistractors )
                          
                          restOfPositionsR <- sample(stimuliposExcludePreviewR, ceiling(restOftargetsetDistractors/2))
                          restOfPositionsL <- sample(stimuliposExcludePreviewL, floor(restOftargetsetDistractors/2))
                          restOfPositions <- c(restOfPositionsR, restOfPositionsL)
                          
                          printstimuli( targetSet, restOfPositions )
                          printemptystimuli( maxsetsize/2 - targetSetSize )
                      }
            #----------------------------
            # set size is odd number
            #----------------------------
                } else {   # setsize is odd number and larger than 3
                      if (competing == 1) {

                        if(congr == 1) {
                          competingDistractor <- ifelse(cue == 1, paste('w', tolower(target), sep=''),  toupper(substr(target, 2, 2)))
                        } else {                     # incongruency
                          competingDistractor <- ifelse(cue == 1, toupper(substr(target,2,2)), paste('w', tolower(target), sep=''))
                        }
                        
                          printstimuli( target, targetPosCode )
                          printstimuli( competingDistractor, competingPosCode)
                        
                          restOftargetsetDistractors <- targetSetSize - 2

                            if (restOftargetsetDistractors == 1) {
                                  targetSet <- sample( distractorPool.unused, replace=F, 1 )
                                  if (sample(c('R', 'L') , 1) == 'R') {
                                    restOfPositions <- sample(stimuliposExcludePreviewR, 1)
                                  } else {
                                    restOfPositions <- sample(stimuliposExcludePreviewL, 1)
                                  }
                                  printstimuli( targetSet, restOfPositions )
                                  printemptystimuli( maxsetsize/2 - targetSetSize )
                            } else {
                                targetSet <- sample( distractorPool.unused, replace=F, restOftargetsetDistractors )
                                if(targetPos == 5) {
                                    restOfPositionsR <- sample(stimuliposExcludePreviewR, ceiling(restOftargetsetDistractors/2))
                                    restOfPositionsL <- sample(stimuliposExcludePreviewL, floor(restOftargetsetDistractors/2))
                                } else {   #targetPos == 6, namely right
                                    restOfPositionsR <- sample(stimuliposExcludePreviewR, floor(restOftargetsetDistractors/2))
                                    restOfPositionsL <- sample(stimuliposExcludePreviewL, ceiling(restOftargetsetDistractors/2))
                                }
                                restOfPositions <- c(restOfPositionsR, restOfPositionsL)
                                printstimuli( targetSet, restOfPositions )
                                printemptystimuli( maxsetsize/2 - targetSetSize )
                            }
                      } else {   # else not compete 
                        printstimuli( target, targetPosCode )
                        restOftargetsetDistractors <- targetSetSize - 1
                        targetSet <- sample( distractorPool.unused, replace=F, restOftargetsetDistractors )
                        
                        restOfPositionsR <- sample(stimuliposExcludePreviewR, ceiling(restOftargetsetDistractors/2))
                        restOfPositionsL <- sample(stimuliposExcludePreviewL, floor(restOftargetsetDistractors/2))
                        restOfPositions <- c(restOfPositionsR, restOfPositionsL)
                        
                        printstimuli( targetSet, restOfPositions )
                        printemptystimuli( maxsetsize/2 - targetSetSize )
                      }
                }
          }

          # previewOrNot will be at @67. We add it in the blockRandomise.
          cat("\n", file="./tables/nonRanTab", append=T)
          #--------------------------------------------------------
            } # end of congr loop
          } # end of letter case loop
        } # end of target position loop
      } # end of set size loop
    } # end of ISI loop

    # Randomize the trail sequence. You may want to change this to a function
    # 'size' here refers to the size of one block.
    # Put these codes inside the repetition loop to ensure its 'within-block' randomization
    tabTmp <- read.table("./tables/nonRanTab", colClasses = rep('character', 5), header=F)

    #-------------------------------------------------------------------------
    # change the replace=TRUE to FALSE, if you do not to have replication    
    #--------------------------------------------------------------------------
    blockRandomise <- tabTmp[sample(previousSetpoint:nrow(tabTmp), replace=F, size=bRSize, 
                             prob=c(rep(c(.50, .50), bRSize/2))), ]
    #blockRandomise <- cbind( blockRandomise, rep(previewOrNot, nrow(blockRandomise)) )
    write.table(blockRandomise, file=filename, col.names=F, row.names=F, quote=F, append=T)
 } # end of repetition for loop    
  system("rm ./tables/nonRanTab")  
  countervalue <- 1
}


  
