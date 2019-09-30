rm(list=ls())

# 04 January, 2012. 
# 14 Jan, 2012. Modified to fit simple search 
x0 = read.table( "/dev/shm/data" )
# x0 = read.table( "search04Jan2012-t1.psy.2012-01-04-1533.data" )
#----------------------------------#
#-- Dump data into to a save keep--#
#----------------------------------#
colnames(x0) <- c('blockName', 'counter', 'targetLR', 'setSize', 'isi', 'cueUcOrLc', 'congr', 'competing', 'preivewOrNot', 'keyPressed', 'keyStatus', 'rt')
write.table(x0, file='dataDump.txt', row.names=F, col.names=T, quote=F)
#----------------------------------#