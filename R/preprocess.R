######################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.4
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 01 Oct, 2018 -- Draft one
## License: GPL 2
## Description: 
## 1. Exp 1 used uppercase letter only and 50-ms ISI. 
## 2. whichKey recorded the key (5 or 6) (see rb830_dim_map.png in figs folder) 
##    entered by the participants.
## 3. keyStatus 1=correct; 2=incorrect; 3=too early; 4=too late
rm(list = ls())
## Linux and Windows wk
setwd("~/Documents/Attention-Guidance-Visual-Search/")
## setwd("C:/Users/user/Documents/Attention-Guidance-Visual-Search/")
## Load E1 data ------------------------------------------------------
load('data/E1/E1.RData')
x0 <- x0[,-c(2, 6:9, 20, 21, 23 )]
x0$S <- factor( ifelse(x0$targetLR == 5, 'L', 
                ifelse(x0$targetLR == 6, 'R', NA)) )
x0$R <- factor( ifelse(x0$whichKey == 5, 'L', 
                      ifelse(x0$whichKey == 6, 'R', NA)) )
x0$Q <- factor( ifelse(x0$fixedOrVariedTgt == 'fixed', 'F',
               ifelse(x0$fixedOrVariedTgt == 'varied', 'V', NA)) )

## **keyStatus** indicates the accuracy (correct=1; incorrect=2) responses.
x0$C <- ifelse(x0$keyStatus == 1, TRUE,
        ifelse(x0$keyStatus == 2, FALSE, NA))
x0$ss <- x0$setSize
x0$SS <- factor( x0$setSize )
x0$RT <- x0$rt / 1000
x0 <- x0[, -c(1:3,6,8,12:15)]
x0$ISI <- factor(x0$isi)
x0$s <- factor(paste0("E1-", x0$subj))
x0$trial <- factor(x0$trialsq)
x0_wide <- x0[, -c(1:4, 6, 16)]
x0 <- x0[, -c(1:6, 16)]
names(x0) <- c("S","R","Q","C", "ss", "SS","RT","I","s")
names(x0_wide) <- c("TR", "S","R","Q","C", "ss", "SS","RT","I","s")
tibble::as_tibble(x0)

## Trimmed off is 0.21% 
censored      <- subset(x0, RT >= .2 & RT <= 2)
censored_wide <- subset(x0_wide, RT >= .2 & RT <= 2)
sum(is.na(x0$C))  ## no too slow or too fast responses
signif( 1-nrow(censored)/nrow(x0), 2)

e1 <- censored[, c("s", "I", "Q", "ss", "SS", "C", "R", "S", "RT")]
e1_wide <- censored_wide[, c("TR", "s", "I", "Q", "ss", "SS", "C", "R", "S", "RT")]

## Load E2 data --------------------------------------------------------
filename <- read.table('data/E2/E2_nameList.txt', header=FALSE)
## total 38 files; 19 valid particpants. Each took part in the two cue
## conditions, separately.
## d1 and d2 have only varied cue task (consistent cue missing).
## d6 withdrew from the study after finishing the varied cue, 
## so no consistent cues.
## trim off s1, s2 and s6
x <- data.frame(filename = filename[-c(1,2,6),]); x 
#-------------------------------------------------------------%

## An old way to load many data file; it is quick to use DLapply, 
## of data.table 
x0 <- NULL
for(i in 1:nrow(x)) {
  tmp1 <- read.table( as.character(x$filename[i]), 
                      col.names=c('B', 'counter',  'S', 'Ns', 
                                  "I", "cueImg", "uclc", "congr", "competing",
                                  "preview", 'whichKey', 'C', 'rt'))
  tmpdf1 <- tmp1[, -c(2, 7:10)]
  
  tmpdf1$S <- ifelse(tmpdf1$S == "1", "R", 
              ifelse(tmpdf1$S == "7", "L", "otherkeys"))
  tmpdf1$R <- ifelse(tmpdf1$whichKey == "1", "R", 
              ifelse(tmpdf1$whichKey == "7", "L", "unknown"))
  tmpdf1$C <- ifelse(tmpdf1$C == "1", TRUE, 
              ifelse(tmpdf1$C == "2", FALSE, NA))
  
  # 15 stands for "H". Psytoolkit translates the image code as 15.
  # varied cueing have changing cueImg codes.
  tmpdf1$Q <- ifelse(all(tmpdf1$cueImg == 15), "F", "V")
  # match one or more number coming after the letter d
  tmpdf1$s <-regmatches(x$filename[i],regexpr("(?<=d)[0-9]+", x$filename[i], perl=TRUE) )
  tmpdf1$TR <- i
  x0 <- rbind(x0, tmpdf1)
}

x0 <- x0[x0$B != "prc", -c(5, 6)]
x0$RT <- x0$rt/1000
x0$S  <- factor(x0$S)
x0$ss <- x0$Ns
x0$SS <- factor(x0$Ns)
x0$I <- factor(x0$I)
x0$Q <- factor(x0$Q)
x0$s <- factor(paste0("E2-", x0$s))
x0$R <- factor(x0$R)
x0$TR <- factor(x0$TR)

x0 <- x0[, -c(1, 3, 6)]
tibble::as_tibble(x0)

# Summarise across trials and target position 
censored <- subset(x0, RT >= .2 & RT <= 2 & R != "unknown")
censored_wide <- subset(x0, RT >= .2 & RT <= 2 & R != "unknown")

# Percentage that trimmed off is 0.456% (with error trials)
signif( 1-nrow(censored)/nrow(x0), 3)

e2 <- censored[, c("s", "I", "Q","ss", "SS", "C", "R", "S", "RT")]
e2_wide <- censored[, c("TR", "s", "I", "Q","ss", "SS", "C", "R", "S", "RT")]
sapply(e2, levels)  ## there were too slow and too fast responses
e2$R <- droplevels(e2$R)
e2_wide$R <- droplevels(e2_wide$R)
unique(e2$R)
unique(e2$C)
unique(e2_wide$R)
unique(e2_wide$C)

## Check if e1 and e2 are in the same formated.
dplyr::tbl_df(e1)
dplyr::tbl_df(e2)

## E2 bg info ---------------
bg <- as.data.frame(matrix(c(
  # 24, 'female', 18, 'r',
  26, 'female', 27, 'r',
  27, 'male',   19, 'r',
  30, 'female', 19, 'r',
  31, 'female', 22, 'r',
  32, 'female', 19, 'r',
  33, 'male',   26, 'r',
  34, 'female', 19, 'r',
  36, 'female', 19, 'r',
  37, 'male',   21, 'r',
  38, 'female', 18, 'r',
  39, 'male',   19, 'r',
  40, 'female', 19, 'r',
  41, 'female', 20, 'r',
  42, 'female', 21, 'r',
  43, 'female', 19, 'r',
  # 44, 'female', 21, 'r',
  45, 'female', 20, 'r',
  46, 'female', 20, 'r',
  47, 'male',   18, 'r',
  49, 'female', 23, 'r'), ncol = 4, byrow = TRUE))
names(bg) <- c("s", "S", "Yr", "H")
summary(bg$S)
summary(bg$H)
## 14 females (5 males) 
## All (19) were right handedness. 
psych::describe(as.numeric(as.character(bg$Yr)))
## vars  n  mean  sd median trimmed  mad min max range skew kurtosis   se
## X1    1 19 20.42 2.5     19   20.18 1.48  18  27     9 1.44     1.02 0.57

## Load E3 data -----------------------------------------------------
filepaths <- paste0("data/E3/",  # valid data (no drop out)
                    c("d24", "d26", "d27", "d30", "d31", # d24 tested a smaller  
                      "d32", "d33", "d34", "d36", "d37", # block size.
                      "d38", "d39", "d40", "d41", "d42",
                      "d43", "d45", "d46", "d47", "d49"))
x0 <- NULL
for(i in 1:length(filepaths)) {
  filenames <- list.files(path=filepaths[i], pattern=".data$")
  file1 <- paste(filepaths[i], filenames[1], sep="/")
  file2 <- paste(filepaths[i], filenames[2], sep="/")
  tmp1 <- read.table(file1, col.names=c(
    "block", "counter", "S", "Ns", 
    "I", "cueImg", "uclc", "congr", "competing",
    "preview", "whichKey", "C", "rt"))
  tmp2 <- read.table(file2, col.names=c(
    "block", "counter", "S", "Ns", 
    "I", "cueImg", "uclc", "congr", "competing",
    "preview", "whichKey", "C", "rt"))
  
  tmpdf1 <- tmp1[tmp1$bloc!="prc",-c(2,7:10)]
  tmpdf1$S <- ifelse(tmpdf1$S == "1", "R", 
                     ifelse(tmpdf1$S == "7", "L", "otherkeys"))
  tmpdf1$whichKey <- ifelse(tmpdf1$whichKey == "1", "R", 
                            ifelse(tmpdf1$whichKey == "7", "L", tmpdf1$whichKey))
  tmpdf1$C <- ifelse(tmpdf1$C == 1, TRUE, 
              ifelse(tmpdf1$C == 2, FALSE, NA))
  tmpdf1$Q <- ifelse(all(tmpdf1$cueImg == 15), "F", "V")
  tmpdf1$s <-regmatches(file1,regexpr("(?<=d)[0-9]+", file1, perl=TRUE)) 
  tmpdf1$TR <- i
  
  tmpdf2 <- tmp2[tmp2$block!="prc",-c(2,7:10)]
  tmpdf2$S <- ifelse(tmpdf2$S == "1", "R", 
                     ifelse(tmpdf2$S == "7", "L", "otherkeys"))
  tmpdf2$whichKey <- ifelse(tmpdf2$whichKey == "1", "R", 
                            ifelse(tmpdf2$whichKey == "7", "L", tmpdf2$whichKey))
  tmpdf2$C <- ifelse(tmpdf2$C == 1, TRUE, 
              ifelse(tmpdf2$C == 2, FALSE, NA))
  tmpdf2$Q <- ifelse(all(tmpdf2$cueImg == 15), "F", "V")
  tmpdf2$s <-regmatches(file2,regexpr("(?<=d)[0-9]+", file2, perl=TRUE)) 
  tmpdf2$TR <- i
  
  tmpdf <- rbind(tmpdf1,tmpdf2)
  x0 <- rbind(x0, tmpdf)
}


# Relabel
x0$RT <- x0$rt/1000 
x0$TR <- factor(c(1:2040, rep(1:2400, 19)))
x0$S <- factor(x0$S)
x0$I <- factor(x0$I)
x0$ss <- x0$Ns
x0$SS <- factor(x0$Ns)
x0$R <- factor(x0$whichKey)

x0$Q <- factor(x0$Q)
x0$s <- factor(paste0("E3-", x0$s))

dplyr::tbl_df(x0)
x0_wide <- x0[, -c(1, 3, 5, 6, 8)]
x0 <- x0[, -c(1, 3, 5, 6, 8, 11)]

dplyr::tbl_df(x0_wide)
dplyr::tbl_df(x0)

# Summarise across trials and target position 
censored <- subset(x0, RT >= .2 & RT <= 2 & R != 2 & R != 3)
censored_wide <- subset(x0_wide, RT >= .2 & RT <= 2 & R != 2 & R != 3)
# Percentage that trimmed off is 0.206% 
signif( 1-nrow(censored)/nrow(x0), 3)
## The levels of slow and fast responses needed to be removed
unique(censored$R)

e3 <- censored[, c("s", "I", "Q","ss", "SS", "C", "R", "S", "RT")]
e3_wide <- censored_wide[, c("TR", "s", "I", "Q","ss", "SS", "C", "R", "S", "RT")]
e3$R <- droplevels(e3$R)
e3_wide$R <- droplevels(e3_wide$R)
unique(e3$R)

e1$s <- droplevels(e1$s)
e2$s <- droplevels(e2$s)
e3$s <- droplevels(e3$s)

sapply(e1, levels)
sapply(e2, levels)
sapply(e3, levels)
dplyr::tbl_df(e1)
dplyr::tbl_df(e2)
dplyr::tbl_df(e3)

## Finally bind all 3 exps together
e1$E <- factor("E1")
e2$E <- factor("E2")
e3$E <- factor("E3")
allexp <- rbind(e1, e2, e3)

e1_wide$E <- factor("E1")
e2_wide$E <- factor("E2")
e3_wide$E <- factor("E3")
allexp_wide <- rbind(e1_wide, e2_wide, e3_wide)
sapply(allexp_wide[, -9], table)

## save(e1, e2, e3, allexp, file = "data/visual_search.RData)
## Next averaging.R