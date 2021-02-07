module SAW
use mtmod
!mtmod is copied as example good rng
implicit none
!----------------------------------	README	--------------------------------------------------
!	--	--	--	This module contains methods:	--	--	--
!		type(location) function moveSAW(start,whereSAW)    r
!		subroutine wasIHere(from,whereSAW,n)
!		integer function direction(check)
!		integer function find(array,what)
!		subroutine walktrackSAW(saving,track)

!		matrix whereSAW contains the information of has sailor been in index whereSAW(i,j)
!		If element is zero, sailor hasn't been there, else it is 1 (one)	
!----------------------------------------------------------------------------------------------------

type :: location			!This derived type will be used to save the location of sailor
 integer :: x,y,t			!And the elapsed time
end type location


contains

type(location) function moveSAW(start,whereSAW)
type(location),intent(in) :: start
integer(16),dimension(610,1200),intent(in) :: whereSAW 
integer ::xi,yi,ti,canGo(4),way


call wasIHere(start,whereSAW,canGo)	!Gives values for canGO

!Here we use function direction() which returns random integer 1,2,3 or 4, depending which way we are allowed to move. 
way =  direction(canGO)

xi = start%x
yi = start%y
ti = start%t


if(way == 0) then		  !Checks if sailor has already been n all directions

else if(way == 1) then 		  !up?
	yi = yi+1
	ti = ti +1 

else if(way == 2) then		  !down?
	yi = yi -1
	ti = ti +1 
else if(way ==3) then 		  !right?
	xi = xi +1
	ti = ti +1 
else   				  ! Only option left to move is left
	xi = xi-1
	ti = ti +1 
end if

!Checking if we didnt move

moveSAW = location(xi,yi,ti)

end function moveSAW

!Function for checking if sailor has been in that spot.
 subroutine wasIHere(from,whereSAW,n)
	type(location),intent(in) :: from
	integer(16),dimension(610,1200),intent(in) :: whereSAW 
	integer,dimension(4),intent(out) :: n 
	n = 0
	! n represents information has the sailor been there (up,down,rigth, left) values are 0 or 1) 
	! elemnt of n = 1 means sailor has already been there, 0 means that sailor hasn't been there
	n(1) = whereSAW(from%x,from%y+1)
	n(2) = whereSAW(from%x,from%y-1)
	n(3) = whereSAW(from%x +1 ,from%y)
	n(4) = whereSAW(from%x -1, from%y)
	
end subroutine wasIHere

!This function minimizes computing done for selecting a path
integer function direction(check)
	integer,dimension(4),intent(in) :: check
	integer :: ways(4),a,b

	ways  = check
!	--	--	--	--	--	
	if(sum(check) == 4) then		!checks if sailor has been in all direcions
		direction = 0

	else if(sum(check) == 3) then		!Checks if one direction is free 
		direction =  find(check,0)
!	--	--	--	--	--
	else if(sum(check) == 2) then		!Check if two directions are free
		a = find(check,0)
		ways(a) = 1
		b = find(ways,0)
		if(igrnd(0,1) == 1) then 	!Calls a random integer 0 or 1
			direction = a
			else
				direction = b
			end if
!	--	--	--	--	--
	else 					!Else three directions are free 
	    a = igrnd(1,4)
		b = find(check,1)
	    if(a /= b) then
			direction = a
			else if(a -1 /= b .and. a > 1) then   !Array check doesn have index 0, so we
				direction = a-1			!have to determine if a = 1
					else
						direction = 4   ! can only return 4
		end if
	end if
!	--	--	--	--	--
end function direction
			
integer function find(array,what)
		integer,dimension(4),intent(in) ::  array
		integer,intent(in) :: what
		integer :: i
		do i = 1,4
		if(array(i) == what) then
			find = i
		    exit
		end if		
	
		end do
		 
end function find
		

subroutine walktrackSAW(saving,track)
	integer,intent(in) :: saving	
	type(location),intent(out) :: track           !Track will be returned as final location of 
	integer(16),dimension(610,1200) :: whereSAW   !		of the sailor
	integer :: t,check(4)
	
	whereSAW = 0  
	whereSAW(10,600) = 1				 !starting position
	track = moveSAW(location(10,600,0),whereSAW)
	whereSAW(track%x,track%y) = 1  !Saving information of sailors presence 
					!in given location to matrix 

	!-------------Loop for saving the trajectory to a file-------------	
	if(saving == 1) then
		
		open(unit = 1,file='../run/output.dat',form='formatted',status='old')
		write(1,'(i0,A,i0)') 10,' ',0				
		write(1,'(i0,A,i0)') track%x,' ',track%y-600
		
		do t = 2, 600
		!If x coordinate is zero, the sailor made it to the shore			
			if(track%x  == 0) exit
			call wasIHere(track,whereSAW,check)
			if(sum(check)==4) exit !if sum = 4, we have no direcitons left to go
			track = moveSAW(track,whereSAW)
			!If we dont move, track== place sailor is in dead end
			whereSAW(track%x,track%y) = 1 
			write(1,'(i0,A,i0)') track%x,' ',track%y-600
		end do
	!-------------------------------------------------------------------

	
	!--------------Loop for not saving to a file------------------------
	else				
		do t = 2, 600
			if(track%x  == 0) exit
			call wasIHere(track,whereSAW,check)			
			if(sum(check)==4) exit 
			track = moveSAW(track,whereSAW)			
			whereSAW(track%x,track%y) = 1 						
		end do
	end if
	!-------------------------------------------------------------------- 		
end subroutine walktrackSAW
end module SAW
