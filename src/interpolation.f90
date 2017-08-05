module interpolation
use types
implicit none

private 
public  lagrangecoeff

contains

! Given two 1-D arrays x and w, returns the Lagrange 
! interpolating polynomial through the points (x, w).

subroutine lagrangecoeff(x,y, deg, coeff)
    implicit none
    integer,intent(in) :: deg
    real(dp) :: x(deg), y(deg)
    real(dp),intent(out) :: coeff(deg)

    integer,parameter :: MAX_DEG = 16
    integer :: i,j,k

    real(dp) :: s(MAX_DEG), phi,ff,b
    coeff = 0
    s = 0
    s(deg)=- x(1)

    do i =2, deg
        do j = deg + 1 - i, deg - 1
            s(j) = s(j) - x(i) * s(j + 1)
        enddo
        s(deg) = s(deg) - x(i)
    enddo

    do j = 1,deg
        phi = deg
        do k = deg-1,1,-1
            phi = k*s(k+1) + x(j)*phi
        enddo
        ff= y(j)/phi
        b = 1.0_dp
        do k=deg,1,-1
            coeff(k) = coeff(k) + b * ff
            b = s(k) + x(j)*b
        enddo
    enddo
    
end subroutine lagrangecoeff

end module interpolation
