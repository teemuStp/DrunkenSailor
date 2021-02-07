# DrunkenSailor
Fortran 95 program


Simulation for solving the well known drunken sailor problem. Sailor wakes up drunk at midnight 10 blocks away from shore.
The ship leaves at 10 AM and each block is 100 meters long. 
The sailor walks at constant speed of 100m/min and in each crossing turns to a completely random direction. 
The next time the sailor has a plan and leaves bottles to the direction which she came from and then avoids those paths (SAW movement).
This has potential to lead to dead end in which case the sailor gives up and goes to sleep. 
The simulation calculates fraction for times the sailor made to the ship in time,
died of old age wandering (50 years) and walked herself in a dead end (SAW only).

To compile this program I used "gfortran". A seconf "read me" file is in the folder.
