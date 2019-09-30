printstimuli=function( stimuli,positions)
{
  diameter <- 145
  angle <- seq(from = -pi/2 + pi/10, to = 3*pi/2, by = pi/10)   # Calculate the angles
  positiontable <- cbind( cos(angle), sin(angle) )
  positiontablex = positiontable * diameter
  l = length(stimuli)
  for(i in 1:l )
  {
    cat( stimuli[i] , positiontablex[ positions[i] , 1 ] , positiontablex[ positions[i] , 2 ] , " ", file="./tables/nonRanTab", append=T)
  }
}





printemptystimuli=function(n)
{
  if ( n > 0 )
  {
    for ( i in 1:n )
      cat( " empty 250 250 ", file="./tables/nonRanTab", append=T)
  }
}

countervalue <<- 1 # global variable for condition
counter=function()
{
  z = countervalue
  countervalue <<- countervalue+1
  return ( z )
}
