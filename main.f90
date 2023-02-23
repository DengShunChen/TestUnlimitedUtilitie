
program main
 use fy_UnlimitedUtilities
 use fy_ArrayWrapper
 use gFTL_UnlimitedVector
 logical :: test_logical=.false.
 real :: test_real=0.0
 type(ArrayWrapper) :: test_wrapper
 type(UnlimitedVector) :: test_unlimitedvector

!case 1 : logical
 write(6,*) '--> test_logical'
 if (is_logical(test_logical)) then
   write(6,*) 'is logical'
 else
   write(6,*) 'is NOT logical'
 endif

!case 2 : real
 write(6,*) '--> test_real'
 if (is_logical(test_real)) then
   write(6,*) 'is logical'
 else
   write(6,*) 'is NOT logical'
 endif

! case 3 : test_wrapper
 write(6,*) '--> test_wrapper'
 test_wrapper%elements = test_real
 if (allocated(test_wrapper%elements)) then
   print*,'allocated'
 else
   print*,'NOT allocated'
 endif
 
 if (is_logical(test_wrapper)) then
   write(6,*) 'is logical'
 else
   write(6,*) 'is NOT logical'
 endif

  
!if (is_logical_scalar(test_unlimitedvector)) then
!  write(6,*) 'is logical'
!else
!  write(6,*) 'is NOT logical'
!endif
    
end program main
