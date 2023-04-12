This directory contains the scripts that were developed in the early days of testing out this idea to use Hector to solve for FFI pathways when we have some sort of target. I've learned a lot like just using MSE is not the way to go need something with more information like a ts of residuals aka usig FME. 

Now I the attempt 3 was successful and is what going to continue with but here are some things I have learned/am concerned about 

1) The target TS needs to be smooth from the historical period a discontinuous jump is not good! 
2) There is some instability in the ts, like odd jumps would not expect could this be addressed by adding more constraints on the inputs being suggested? If we used soemthing like RF as the target to fit to? Or if we did every 5 years as points to look at and then interpolated in between? 
