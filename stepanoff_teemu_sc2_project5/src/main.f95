program drunkensailor
use averages
implicit none

character(len = 80) :: results(8)
integer :: i,n,saving,SAWalk,iostat,io
character(len = 80) :: iterate, wanttosave,wantSAW



!Program requires input from command line ./a.out iteration yes/*  yes/*

call get_command_argument(1,iterate)   !Argument for number of iterations

		          
	write(*,*) trim(iterate)
	read(iterate,'(i100)') n !maksium value for n is 9 999 999 999
       
	
	if(n < 0) then
	n = -n
	end if


call get_command_argument(2,wanttosave)
	if(trim(wanttosave).eq.'yes') then
		saving = 1   !saves trajectories to a file
		print'(a)','Trajectories are saved to a file'
	else
		saving = 0   !doesn't save anything to a file
		print'(a)','Trajectories are not saved to a file'
	end if

call get_command_argument(3,wantSAW)	

	if(trim(wantSAW).eq.'yes') then
		SAWalk = 1   !SAW simulation
		print'(a)','Self avoiding walk'
	else
		SAWalk = 0   !Ordinary walk simulation. No argument given leads here also
		print'(a)','Ordinary walk'
	end if
print'(a,i0,/)','Number of simulations: ',n

!The prints for results
if(n == 0) then
	print*,'Number of simulations has to be greater than zero!'

else if(SAWalk == 0) then 

call average(saving,n,results)

print'(a,a)','Average time: ',results(1)
print'(a,a)','Minium time spent walking: ',results(2)
print'(a,a)','Maxium time spent: ',results(3)
print'(a,a)','Average distance walked: ',results(4)
print'(a,a)','Minium distance: ',results(5)
print'(a,a)','Maxium distance: ',results(6)
print'(a,a,a)','Sailor made it to Ship in time ',results(7),'percent of the times'
print'(a,a,a)','Sailor died of old age ',results(8),'percent of the times'

else

call averageSAW(saving,n,results)
print'(a,a)','Average time: ',results(1)
print'(a,a)','Minium time spent walking: ',results(2)
print'(a,a)','Maxium time spent: ',results(3)
print'(a,a)','Average distance walked: ',results(4)
print'(a,a)','Minium distance: ',results(5)
print'(a,a)','Maxium distance: ',results(6)
print'(a,a,a)','Sailor made it to Ship in time ',results(7),'percent of the times'
print'(a,a,a)','Sailor walked in a dead end ',results(8),'percent of the times'

end if

end program drunkensailor
