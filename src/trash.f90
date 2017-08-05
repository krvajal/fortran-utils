function polyfit(x,y,deg) result(coeff)
    implicit none
    integer  :: deg ! degree of the polynomial
    real(dp) :: x(:), y(:)
    real(dp) :: coeff(deg)

    integer :: sizex, sizey
    integer :: i
   !perform checkinks
   sizex =  ubound(x,1)
   sizey =  ubound(y,1)

   if (sizex /= sizey) stop "polyfit error: input data size does not match"
   if( sizex < deg) stop "polyfit error: the degree is bigger than the data size"


end function polyfit