module averages
use sailormover

implicit none

!-----------------------------	README	-----------------------------------------------
!This module uses sailormover and sailormoverSAW modules to calculate averages
!This module has methods for calculating averages, smallest, largest values and fractions of ending results
!	Method timebreaker() will split time given in minutes in more easily understandable units
!	
!---------------------------------------------------------------------------------------
contains

!Functions for comparing integers

integer function smaller(old,new)
	integer,intent(in):: old,new

	if(old >= new) then
	smaller = new
	else
	smaller = old
	end if
end function smaller

integer function bigger(old,new)
	integer,intent(in):: old,new

	if(old <= new) then
	bigger = new
	else
	bigger = old
	end if
end function bigger

subroutine average(saving,n,values)	
 	integer,intent(in) :: saving,n
	character(len = 80),dimension(8),intent(out) :: values
	integer :: i,minutes,mintime,maxtime,mins,gotship,died
	real :: atime,minlength,maxlength,ameters
	type(location) :: place
	
	! "saving" determines if the data will be saved to a file, "n" > 0 is number of iterations	
	!Variables for average calculations and to obtain min max values
	mintime = 26280000
	maxtime = 0
	minutes = 0
	mins = 0
	gotship = 0 
	died = 0
	!used time x 100m  gives us walked distance
	
	do i = 1,n
	!for this we need call walktrack from module sailormover
		
		call walktrack(saving,place)
		minutes  = minutes + place%t		
		
		mintime = smaller(mintime,place%t)
		maxtime = bigger(maxtime,place%t)
		
		!Solving how many got to ship and how many died
		if(600 >= place%t) then
			gotship = gotship + 1  
		else if(place%t == 26280000) then
			died = died +1
		end if

	end do

	!calculating the averages
	atime = (minutes*1.0) / (n*1.0) 
	values(1) = timebreaker(nint(atime))
	values(2) = timebreaker(mintime)
	values(3) = timebreaker(maxtime)
	
	ameters = (atime*0.1) !Turns 100m to 0.1km	
	minlength = (mintime*0.1)
	maxlength = (maxtime*0.1) 
	
	write(values(4),'(f0.1,a)') ameters,' km'
	write(values(5),'(f0.1,a)') minlength,' km'
	write(values(6),'(f0.1,a)') maxlength,' km'
	write(values(7),'(f0.1)') ((gotship*1.0)/n*1.0)*100

	!died value might usually be zero
	if(died == 0)	then 
	write(values(8),'(i0)') died
	else
	write(values(8),'(f0.1)') ((died*1.0)/n*1.0)*100
	end if
		
end subroutine average


!Averages for SAW 
subroutine averageSAW(saving,n,values)	
 	integer,intent(in) :: saving,n
	character(len = 80),dimension(8),intent(out) :: values
	integer :: i,minutes,mintime,maxtime,deadend,gotship
	real :: atime,minlength,maxlength,ameters
	type(location) :: place
	
	
	! "saving" determines if the data will be saved to a file, n > 0 is number of iterations	
	!Variables for average calculations and to obtain min max values
	mintime = 600
	maxtime = 0
	minutes = 0
	gotship = 0 
	deadend = 0
	!used time x 100m  gives us walked distance
	
	do i = 1,n
	!for this we need call walktrack from module sailormover
		
		call walktrackSAW(saving,place)
		minutes  = minutes + place%t		
		mintime = smaller(mintime,place%t)
		maxtime = bigger(maxtime,place%t)
		
		!Solving how many got to ship and how many died
		if(0 == place%x) then
			gotship = gotship + 1  
		else
			deadend = deadend +1
		end if

	end do

	!calculating the averages
	atime = (minutes*1.0) / (n*1.0) 
	values(1) = timebreaker(nint(atime))
	values(2) = timebreaker(mintime)
	values(3) = timebreaker(maxtime)
	
	ameters = (atime*0.1) !Turns 100m to 0.1km	
	minlength = (mintime*0.1)
	maxlength = (maxtime*0.1) 
	
	write(values(4),'(f0.1,a)') ameters,' km'
	write(values(5),'(f0.1,a)') minlength,' km'
	write(values(6),'(f0.1,a)') maxlength,' km'
	write(values(7),'(f0.1)') ((gotship*1.0)/n*1.0)*100

	!died value might usually be zero
	if(deadend == 0)	then 
	write(values(8),'(i0)') deadend
	else
	write(values(8),'(f0.1)') ((deadend*1.0)/n*1.0)*100
	end if
	

end subroutine averageSAW

!Function for turning minutes into better form
character(len = 80) function timebreaker(minutes)

	character(len = 80) :: yhdm
	integer,intent(in) :: minutes
	integer,dimension(4) :: time
	!Function works only for integer input parameters
	!time array will be in form time(mins,hours,days,years)
	time = 0

	if(minutes < 60) then !smaller than hour
		time(1) = minutes
		write(yhdm,'(i0,a)') time(1),' minutes'
	else if( minutes < 1440) then !smaller than day

		time(1) =  mod(minutes,60)
		time(2) = minutes / 60
		if(time(1) /= 0) then
		write(yhdm,'(i0,a,i0,a)') time(2),' hours, ',time(1),' minutes'
		else
			write(yhdm,'(i0,a)') time(2),' hours'
		end if

	else if(minutes < 525600) then !smaller than year
		time(1) = mod(minutes,60)
		time(3) = minutes /  1440
		time(2) = (minutes - time(1) - time(3)*1440)/60
		if(time(1) /= 0 .and. time(2) /= 0) then
		write(yhdm,'(i0,a,i0,a,i0,a)') time(3),' days, ',time(2),' hours, ',time(1),' minutes'
		else if(time(1) /= 0) then
		write(yhdm,'(i0,a,i0,a)') time(3),' days, ',time(1),' minutes'			
		else if(time(2) /= 0)	then
		write(yhdm,'(i0,a,i0,a)') time(3),' days, ',time(2),' hours'
		else 
		write(yhdm,'(i0,a)') time(3),' days'
		end if
	else !passed time exeeds years, leap year is not taken in account
		time(1) = mod(minutes,60)
		time(4) = minutes/525600
		time(3) = (minutes - time(4)*525600)/1440
		time(2) = (minutes - time(1) - time(3)*1440 -time(4)*525600) /60
	!If tree to solve if some elements are zero and not writing them
		if(time(1) == 0 .and. time(2)==0 .and. time(3) == 0 )then
		write(yhdm,'(i0,a)') time(4),' years'
		else if(time(1) == 0 .and. time(2)==0) then
		write(yhdm,'(i0,a,i0,a)') time(4),' years, ',time(3),' days'
		else if(time(1) == 0) then
		write(yhdm,'(i0,a,i0,a,i0,a)') time(4),' years, ',time(3),' days, ',time(2),' hours'
		else if(time(2)==0 .and. time(3) == 0 ) then
		write(yhdm,'(i0,a,i0,a)') time(4),' years, ',time(1),' minutes'
		else if(time(2) == 0)then
		write(yhdm,'(i0,a,i0,a,i0,a)') time(4),' years, ',time(3),' days, ',time(1),' minutes'
		else if(time(1) == 0 .and. time(3) == 0) then
		write(yhdm,'(i0,a,i0,a)') time(4),' years, ',time(2),' hours'	
		else if(time(3) == 0) then
		write(yhdm,'(i0,a,i0,a,i0,a)') time(4),' years, ',time(2),' hours, ',time(1),' minutes'
		else 
		write(yhdm,'(i0,a,i0,a,i0,a,i0,a)') time(4),' years, ',time(3),' days, ',time(2),' hours',time(1),' minutes'
		end if
	end if
	timebreaker = yhdm
end function timebreaker

end module averages
