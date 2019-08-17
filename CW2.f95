PROGRAM ADVECTION
! A program to solve the 1D advection equation using finite differences.
IMPLICIT NONE

INTEGER, PARAMETER :: NUM_Z=100 
!NUM_Z+1: The number of grid/computation points in the z direction


INTEGER :: M ! iteration counters, M is used for counting in x direction

REAL :: U(0:NUM_Z), C, Z(0:NUM_Z), U_PRED(0:NUM_Z), H
! Array U is the wave amplitude
! C is the speed of the wave
! The array Z contains equally spaced z positions, 5.0 is the coastline position 

! Assign numerical and physical constants their values
  H = 0.05  !H is the step-length in space DZ 
  C = 0.45  !speed of the wave
  U(0) = 0.2
  U_PRED(0) = 0.2

!#### GEOMETRY SET-UP SECTION ##########
 DO M=0,NUM_Z
  Z(M) = M*H !The equally spaced x location values from 0 to 5, inclusive
 ENDDO
!#######################################

!#### INITIAL CONDITIONS SET-UP AND MODIFIED EULER  (values of U at time t=0) ####
 DO M=0,NUM_Z-1
  U_PRED(M+1) = -U(M)*(H*(C-2*U(M))**0.5 -1)

  U(M+1) = U(M) - (H/2)*((U(M)*(C-2*U(M))**0.5)+(U_PRED(M+1)*(C-2*U_PRED(M+1))**0.5))
 ENDDO
 
!############ RESULTS OUTPUT SECTION ################################
!Output results at first and last time iteration
WRITE(*,*) 'Results of wave amplitude upto coastline'
!This strange write command formats our output
!(not important for this course)
WRITE(*,'(6x,a2,10x,a6)') 'z','u(z)'
 DO M=0,NUM_Z
   WRITE(*,*) Z(M),U(M)
 ENDDO

!WRITE(*,*) 'Results at NUM_T (last time step)'
!WRITE(*,'(6x,a2,10x,a6)') 'x','c(x,t)'
! DO M=0,NUM_X
!   WRITE(*,*) X(M),C(M,NUM_T)
! ENDDO
!####################################################################


END PROGRAM
