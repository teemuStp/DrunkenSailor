module sailormover
use SAW
implicit none
!-------------------	README	-------------------------------------------------------------
! 	This module is for moving sailor normally, and contains methods	
!	type(location) function move(start)  moves the sailor
!	subroutine walktrack(saving,track)	moves sailor until boundary condition is met 
!	subroutine returns her final location (track)
!---------------------------------------------------------------------------------------------
contains

type(location) function move(start)
type(location),intent(in) :: start
integer :: way1, way2,xi,yi,ti

xi = start%x
yi = start%y
ti = start%t

way1 = igrnd(0,1)		! igrnd() is located in mtfort90.f95 
way2 = igrnd(0,1)

!if random variable way1 is 0, the movement happens in x direction. If way1 = 1, movement happens in y direction.
!Random integer way2 determines if the movement happens in positive or negative direction of the selected y or x axis

if(way1 == 0) then		
    if(way2 == 1) then
          xi = xi+1		!Sailor moves to the right
          else
              xi = xi -1	!Sailor moves to the left
              end if
else
    if(way2 == 0) then
           yi = yi +1		!Sailor moves up
    else
            yi = yi - 1		!Sailor moves down
    end if
end if
ti = ti +1 !adds one minute

move = location(xi,yi,ti)

end function move

!This function changes value of location as long as sailor hasn't found the shore, ore died of old age
!SAW walking is defined in other function
!The function will return an location  an save the trajectory in to a file

subroutine walktrack(saving,track)
	integer,intent(in) :: saving	
	type(location),intent(out) :: track	
	integer :: t
	
	!loop for saving the trajectory to file	
	if(saving == 1) then
		open(unit = 1,file='../run/output.dat',form='formatted',status='old')
		!Starting value can be set here,for it is every time the same
		write(1,'(i0,A,i0)') 10,' ',0
		track = move(location(10,0,0))	
		write(1,'(i0,A,i0)') track%x,' ',track%y
		do t = 2, 26280000
			!If x coordinate is zero, the sailor made it to the shore			
			if(track%x == 0) exit

			track = move(track)
			write(1,'(i0,A,i0)') track%x,' ',track%y
		end do
	!loop for not saving to file
	else
		track = move(location(10,0,0))	
		do t = 2, 50*365*24*60
			if(track%x == 0) exit
			track = move(track)
		end do
	end if 	

	
end subroutine walktrack

end module sailormover
