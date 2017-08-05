program test_lagrangecoeff
use types, only: dp
use utils, only: assert
use interpolation

implicit none
integer,parameter :: degree = 3
real(dp) :: x(degree), y(degree), coeff(degree)
real(dp) :: eps = 1e-9
x(1) = 1
x(2) = 2
x(3) = 0
y(1) = 6
y(2) = 12
y(3) = 2
!the results are give from smaller to bigger

call lagrangecoeff(x,y,degree,coeff)
call assert(abs(coeff(2) - 3_dp) < eps)
call assert(abs(coeff(1) - 2_dp) < eps)
call assert(abs(coeff(3) - 1_dp) < eps)

end program
