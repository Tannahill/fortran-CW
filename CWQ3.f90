PROGRAM CALCULATOR
! solves hyperbolic cotangent function arcoth(x) for any input x and n term
 IMPLICIT NONE

!########################### ASSIGN VARIABLES ##############################################
                         INTEGER :: K, N, ERROR	
                           REAL  :: ARCOTH, X
 REAL, DIMENSION(:), ALLOCATABLE :: E				!Undefined array size determined by input
!###########################################################################################

!###################### REQUEST INTEGER INPUT WITH CONDITIONS ##############################
 DO
   WRITE (*,*) 'Input n, must be positive integer'	!Diplay request for positive integer
   READ (*,'(I7)',IOSTAT=ERROR) N					!Read integer of upto 7 places and set
     IF (ERROR==0 .AND. N.GE.0) THEN				!IOSTAT error code to variable ERROR and
       ALLOCATE(E(0:N))								!assign input variable N, if N is +ve or
       EXIT											!ERROR is 0 (read successfully according)
     ELSE											!to the read formatting) allocate N to 
       PRINT *, 'Error! Input not valid, try again'	!the array dimension size and end loop								
     END IF											!Or else display error message 	
 END DO	
												
!###########################################################################################


!########################### REQUEST X VALUE WITH CONDITIONS ###############################
 DO
   WRITE (*,*) 'Input x, any real number |x|>1'			!Display request for x value |x|>1
   READ (*,*) X											!Read and assign to variable X
     IF (X.LE.1.AND.X.GE.-1) THEN						!If |x|<1 display error message and
       PRINT *, 'Error! Input not valid, try again'		!repeat request
     ELSE
       EXIT
     END IF
 END DO
!###########################################################################################


!########################## SET VALUES FOR EACH SUM TO ARRAY ###############################
 DO K=0,N									!For zero to inputted integer
   E(K) = 1./((2*K+1)*X**(2*K+1))		!Calculate series values and assign to array E
 END DO												
!###########################################################################################


!############################## FIND VALUE FOR ARCOTH ###################################### 
 ARCOTH = SUM(E)					!Sum the series from array E and set to variable ARCOTH
 ! There seems to be a problem with larger n values, when over 4 or 5 decimal places theres
 ! an error message which I think is down to memory. I think I'm suppose to use KIND 
 ! parameters but I'm not sure...
!###########################################################################################

 
!###########################################################################################
 WRITE(*,*) 'The approximated result of arcoth(x) is'		!Display answer string and 
 WRITE(*,*) ARCOTH											!result for ARCOTH
!###########################################################################################

END PROGRAM

!===========================================================================================

