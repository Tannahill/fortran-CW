PROGRAM WAVE_HEIGHT
! A program to solve wave height using Euler's method.
IMPLICIT NONE

!########################### ASSIGN VARIABLES ##############################################
!length of array NUM_Z, this is the weakest part of my code as it needs changing if the step 
!size is decrease. Ideally I would use an ALLOCATABLE array but I need to fill the Z array  
!first to establish the dimension needed for the other arrays 
              INTEGER, PARAMETER :: NUM_Z=100 !inital array size for z values upto 5.0 
                         INTEGER :: M, N      !Counters
        REAL, DIMENSION(0:NUM_Z) :: Z         !Z equally spaced z positions
 REAL, DIMENSION(:), ALLOCATABLE :: U 		  !wave height array
                            REAL :: H, C      !step size and speed of wave
!###########################################################################################


!###################################### CONSTANTS ##########################################
     H = 0.05   !H is the step-length in space DZ 
     C = 0.45   !speed of the wave
     M = 0    	!Set initial value for the counter
!###########################################################################################       
      
 
!#################### CALL FORWARD THE SUBROUTINE FOR Z-VALUES #############################
  CALL Z_STEP(H,Z,M,NUM_Z)
!###########################################################################################


!################ ALLOCATED THE UNDEFINED ARRAYS AND INITAL CONDITION ######################
  ALLOCATE(U(0:M))		!Defines the size of the allocatable array using the counted value
   						!from the subroutine output
  U(0) = 0.2			!Set inital condition for element zero of array U
!###########################################################################################


!############## CALCULATING THE WAVE HEIGHT FOR CORRESPONDING Z DIRECTION  #################
  DO N=0,M-1 
    U(N+1) = -U(N) * (H*(C-2*U(N))**0.5 -1)	!Finds the next value for U
  ENDDO
!###########################################################################################

 
!######################## CALL HEADER SUBROUTINE TO DISPLAY HEADERS ########################
  CALL HEADERS
!###########################################################################################


!################################## RESULTS OUTPUT SECTION #################################
  DO N=0,M
    WRITE(*,*) Z(N),U(N)	!Display the value of z and u upto the coastline 5.0
  ENDDO
!###########################################################################################

END PROGRAM

!===========================================================================================
!################# SUBROUTINE TO SET-UP OF Z VALUE ACCORDING TO THE STEP SIZE ##############
SUBROUTINE Z_STEP(STEP,Z_VALUES,COUNTER, NUM_Z)
 IMPLICIT NONE
 
    REAL :: STEP, Z_VALUES(0:NUM_Z)
 INTEGER :: COUNTER, NUM_Z
 
   DO 
    Z_VALUES(COUNTER) = COUNTER*STEP   !The equally spaced z values
     IF (Z_VALUES(COUNTER).GE.5) THEN  !Stop loop when z element is greater or equal to 5   
       EXIT                            !(i.e. at the coastline)
     ELSE 							   
       COUNTER=COUNTER+1               !Next loop count
     END IF
  ENDDO

END SUBROUTINE
!###########################################################################################

!===========================================================================================
!############################ SUBROUTINE TO SET HEADER OUTPUT TEXT #########################
SUBROUTINE HEADERS
 
!Output results 
 WRITE(*,*) 'Results of wave amplitude upto coastline'
!Align header text: 4 blank spaces, 12 characters, 4 blank spaces and 16 characters
 WRITE(*,'(4x,a12,4x,a16)') 'Distance (z)','Wave height u(z)'

END SUBROUTINE
!###########################################################################################

  

