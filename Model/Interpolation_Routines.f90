!#######################################################################
! Module Interpolation
! References:
! 1) The linear and bilinear interpolation routines draw on the code accompanying
!    "Monetary Policy According to HANK", AER 2018, by Greg Kaplan, Benjamin Moll, Giovanni L. Violante
!#######################################################################

module Interpolation_Routines

    implicit none
    
contains

    subroutine LinInterp1_Extrap(n,x,y,xi,yi)
    ! this does linear interpolation of (x,y) at points only point,xi
    ! requires x to be sorted in ascending order
    ! extrapolates out of range
    ! does interpolation at just one node
    
        integer, intent(in) :: n
        real*8, intent(in) :: x(:),y(:),xi
        real*8, intent(out) :: yi
        real*8 :: xL,xH,yL,yH
        integer :: locL
    
        LocL = MAXLOC(x,1,MASK=xi>x)

        if (xi<=x(1)) then
            LocL = 1
        end if

        if (LocL>=n) then 
            LocL = n-1
        end if

        xL  = x(locL)
        xH  = x(locL +1)
        yL  = y(locL)
        yH  = y(locL +1)

        if (abs(xL-xH)<1.0d-12) then
            yi = 0.5*(yL+yH)
        else
            yi  = yL  + ((xi -xL )/(xH -xL ))*(yH -yL )
        end if

    end subroutine LinInterp1_Extrap

    subroutine LinInterp_Extrap(n,x,y,ni,xi,yi)
    ! this does linear interpolation of (x,y) at points xi
    ! requires x to be sorted in ascending order
    ! extrapolates out of range
    ! does interpolation at vector of nodes
        
        implicit none
        integer, intent(in) :: n, ni
        real*8, intent(in) :: x(:),y(:),xi(:)   
        real*8, intent(out) :: yi(:)
        real*8 :: xL(ni),xH(ni),yL(ni),yH(ni)
        integer :: i,locL(ni)

        do i = 1,ni

            LocL(i) = MAXLOC(x,1,MASK=xi(i)>x)
	
            if (xi(i)<=x(1)) then
                LocL(i) = 1
            end if

            if (LocL(i)>=n) then
                LocL(i) = n-1
            end if

            xL(i) = x(locL(i))
            xH(i) = x(locL(i)+1)
            yL(i) = y(locL(i))
            yH(i) = y(locL(i)+1)
	
            yi(i) = yL(i) + ((xi(i)-xL(i))/(xH(i)-xL(i)))*(yH(i)-yL(i))

        end do
        
    end subroutine LinInterp_Extrap
    
    subroutine BiLinInterp1_Extrap(nx,x,ny,y,f,xi,yi,fi)
    ! this does linear interpolation of f(x,y) at points (xi,yi)
    ! requires x and y to be sorted in ascending order
    ! extrapolates out of range
    ! works only for one point (xi,yi)

        integer, intent(in) :: nx,ny
        real*8, intent(in) :: x(:),y(:),f(:,:),xi,yi
        real*8, intent(out) :: fi
        real*8 :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy
        integer :: xlocL,ylocL

        xlocL = MAXLOC(x,1,MASK=xi>x)
        ylocL = MAXLOC(y,1,MASK=yi>y)

        if (xi<=x(1)) then
            xlocL = 1
        end if

        if (xLocL>=nx) then 
            xLocL = nx-1
        end if

        if (yi<=y(1)) then
            ylocL = 1
        end if

        if (yLocL>=ny) then 
            yLocL = ny-1
        end if

        xL  = x(xlocL)
        xH  = x(xlocL +1)
        yL  = y(ylocL)
        yH  = y(ylocL +1)
        fLL = f(xlocL,ylocL)
        fLH = f(xlocL,ylocL+1)
        fHL = f(xlocL+1,ylocL)
        fHH = f(xlocL+1,ylocL+1)

        dxdy = (xH-xL)*(yH-yL)
        fi = fLL*(xH-xi)*(yH-yi)/(dxdy) + fHL*(xi-xL)*(yH-yi)/(dxdy) + fLH*(xH-xi)*(yi-yL)/(dxdy) + fHH*(xi-xL)*(yi-yL)/(dxdy)
        
    end subroutine BiLinInterp1_Extrap

    subroutine BiLinInterp_Extrap(nx,x,ny,y,f,xi,yi,fi)
    ! this does linear interpolation of f(x,y) at points (xi,yi)
    ! requires x and y to be sorted in ascending order
    ! xi and yi are vectors of the same length ni
    ! extrapolates out of range

        integer, intent(in) :: nx,ny
        real*8, intent(in) :: x(:),y(:),f(:,:),xi(:),yi(:)
        real*8, intent(out) :: fi(:)
        real*8 :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy
        integer :: xlocL,ylocL,i,ni

        ni = size(xi)

        do i = 1,ni
            xlocL = MAXLOC(x,1,MASK=xi(i)>x)
            ylocL = MAXLOC(y,1,MASK=yi(i)>y)

            if (xi(i)<=x(1)) then
                xlocL = 1
            end if

            if (xLocL>=nx) then 
                xLocL = nx-1
            end if

            if (yi(i)<=y(1)) then
                ylocL = 1
            end if

            if (yLocL>=ny) then 
                yLocL = ny-1
            end if

            xL  = x(xlocL)
            xH  = x(xlocL +1)
            yL  = y(ylocL)
            yH  = y(ylocL +1)
            fLL = f(xlocL,ylocL)
            fLH = f(xlocL,ylocL+1)
            fHL = f(xlocL+1,ylocL)
            fHH = f(xlocL+1,ylocL+1)

            dxdy = (xH-xL)*(yH-yL)
            fi(i) = fLL*(xH-xi(i))*(yH-yi(i))/(dxdy) + fHL*(xi(i)-xL)*(yH-yi(i))/(dxdy) + fLH*(xH-xi(i))*(yi(i)-yL)/(dxdy) &
            &+ fHH*(xi(i)-xL)*(yi(i)-yL)/(dxdy)
        end do

    end subroutine BiLinInterp_Extrap
    
    subroutine LinInterp1(n,x,y,xi,yi)
    ! this does linear interpolation of (x,y) at points only point,xi
    ! requires x to be sorted in ascending order
    ! does interpolation at just one node
    
        integer, intent(in) :: n
        real*8, intent(in) :: x(:),y(:),xi
        real*8, intent(out) :: yi
        real*8 :: xL,xH,yL,yH
        integer :: locL
    
        LocL = MAXLOC(x,1,MASK=xi>x)

        if (xi<=x(1)) then
            if (abs(xi-x(1))<1d-10) then
                LocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (xi>=x(n)) then
            if (abs(xi-x(n))<1d-10) then
                LocL = n-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if

        xL  = x(locL)
        xH  = x(locL +1)
        yL  = y(locL)
        yH  = y(locL +1)

        if (abs(xL-xH)<1.0d-12) then
            yi = 0.5*(yL+yH)
        else
            yi  = yL  + ((xi -xL )/(xH -xL ))*(yH -yL )
        end if

    end subroutine LinInterp1

    subroutine LinInterp(n,x,y,ni,xi,yi)
    ! this does linear interpolation of (x,y) at points xi
    ! requires x to be sorted in ascending order
    ! does interpolation at vector of nodes
        
        implicit none
        integer, intent(in) :: n, ni
        real*8, intent(in) :: x(:),y(:),xi(:)   
        real*8, intent(out) :: yi(:)
        real*8 :: xL(ni),xH(ni),yL(ni),yH(ni)
        integer :: i,locL(ni)

        do i = 1,ni

            LocL(i) = MAXLOC(x,1,MASK=xi(i)>x)
	
            if (xi(i)<=x(1)) then
                if (abs(xi(i)-x(1))<1d-10) then
                    LocL = 1
                else
                    write(*,*) "Interpolation: Point below lower bound of grid"
                    return
                end if
            end if

            if (xi(i)>=x(n)) then
                if (abs(xi(i)-x(n))<1d-10) then
                    LocL = n-1
                else
                    write(*,*) "Interpolation: Point above upper bound of grid"
                    return
                end if
            end if

            xL(i) = x(locL(i))
            xH(i) = x(locL(i)+1)
            yL(i) = y(locL(i))
            yH(i) = y(locL(i)+1)
	
            yi(i) = yL(i) + ((xi(i)-xL(i))/(xH(i)-xL(i)))*(yH(i)-yL(i))

        end do
        
    end subroutine LinInterp
    
    subroutine BiLinInterp1(nx,x,ny,y,f,xi,yi,fi)
    ! this does linear interpolation of f(x,y) at points (xi,yi)
    ! requires x and y to be sorted in ascending order
    ! works only for one point (xi,yi)

        integer, intent(in) :: nx,ny
        real*8, intent(in) :: x(:),y(:),f(:,:),xi,yi
        real*8, intent(out) :: fi
        real*8 :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy
        integer :: xlocL,ylocL

        xlocL = MAXLOC(x,1,MASK=xi>x)
        ylocL = MAXLOC(y,1,MASK=yi>y)

        if (xi<=x(1)) then
            if (abs(xi-x(1))<1d-10) then
                xLocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (xi>=x(nx)) then
            if (abs(xi-x(nx))<1d-10) then
                xLocL = nx-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if
        
        if (yi<=y(1)) then
            if (abs(yi-y(1))<1d-10) then
                yLocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (yi>=y(ny)) then
            if (abs(yi-y(ny))<1d-10) then
                yLocL = ny-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if

        xL  = x(xlocL)
        xH  = x(xlocL +1)
        yL  = y(ylocL)
        yH  = y(ylocL +1)
        fLL = f(xlocL,ylocL)
        fLH = f(xlocL,ylocL+1)
        fHL = f(xlocL+1,ylocL)
        fHH = f(xlocL+1,ylocL+1)

        dxdy = (xH-xL)*(yH-yL)
        fi = fLL*(xH-xi)*(yH-yi)/(dxdy) + fHL*(xi-xL)*(yH-yi)/(dxdy) + fLH*(xH-xi)*(yi-yL)/(dxdy) + fHH*(xi-xL)*(yi-yL)/(dxdy)
        
    end subroutine BiLinInterp1

    subroutine BiLinInterp(nx,x,ny,y,f,xi,yi,fi)
    ! this does linear interpolation of f(x,y) at points (xi,yi)
    ! requires x and y to be sorted in ascending order
    ! xi and yi are vectors of the same length ni

        integer, intent(in) :: nx,ny
        real*8, intent(in) :: x(:),y(:),f(:,:),xi(:),yi(:)
        real*8, intent(out) :: fi(:)
        real*8 :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy
        integer :: xlocL,ylocL,i,ni

        ni = size(xi)

        do i = 1,ni
            xlocL = MAXLOC(x,1,MASK=xi(i)>x)
            ylocL = MAXLOC(y,1,MASK=yi(i)>y)

            if (xi(i)<=x(1)) then
                if (abs(xi(i)-x(1))<1d-10) then
                    xLocL = 1
                else
                    write(*,*) "Interpolation: Point below lower bound of grid"
                    return
                end if
            end if

            if (xi(i)>=x(nx)) then
                if (abs(xi(i)-x(nx))<1d-10) then
                    xLocL = nx-1
                else
                    write(*,*) "Interpolation: Point above upper bound of grid"
                    return
                end if
            end if
        
            if (yi(i)<=y(1)) then
                if (abs(yi(i)-y(1))<1d-10) then
                    yLocL = 1
                else
                    write(*,*) "Interpolation: Point below lower bound of grid"
                    return
                end if
            end if

            if (yi(i)>=y(ny)) then
                if (abs(yi(i)-y(ny))<1d-10) then
                    yLocL = ny-1
                else
                    write(*,*) "Interpolation: Point above upper bound of grid"
                    return
                end if
            end if

            xL  = x(xlocL)
            xH  = x(xlocL +1)
            yL  = y(ylocL)
            yH  = y(ylocL +1)
            fLL = f(xlocL,ylocL)
            fLH = f(xlocL,ylocL+1)
            fHL = f(xlocL+1,ylocL)
            fHH = f(xlocL+1,ylocL+1)

            dxdy = (xH-xL)*(yH-yL)
            fi(i) = fLL*(xH-xi(i))*(yH-yi(i))/(dxdy) + fHL*(xi(i)-xL)*(yH-yi(i))/(dxdy) + fLH*(xH-xi(i))*(yi(i)-yL)/(dxdy) &
            &+ fHH*(xi(i)-xL)*(yi(i)-yL)/(dxdy)
        end do

    end subroutine BiLinInterp
    
    subroutine TriLinInterp1(nx,x,ny,y,nz,z,f,xi,yi,zi,fi)
    ! this does linear interpolation of f(x,y,z) at points (xi,yi,zi)
    ! requires x and y and z to be sorted in ascending order
    ! works only for one point (xi,yi,zi)

        integer, intent(in) :: nx,ny,nz
        real*8, intent(in) :: x(:),y(:),z(:),f(:,:,:),xi,yi,zi
        real*8, intent(out) :: fi
        real*8 :: xL,xH,yL,yH,zL,zH,fLLL,fLLH,fLHL,fLHH,fHHH,fHHL,fHLH,fHLL
        integer :: xlocL,ylocL,zlocL
        real*8 :: xd,yd,zd
        real*8 :: c00,c01,c10,c11,c0,c1

        xlocL = MAXLOC(x,1,MASK=xi>x)
        ylocL = MAXLOC(y,1,MASK=yi>y)
        zlocL = MAXLOC(z,1,MASK=zi>z)

        if (xi<=x(1)) then
            if (abs(xi-x(1))<1d-10) then
                xLocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (xi>=x(nx)) then
            if (abs(xi-x(nx))<1d-10) then
                xLocL = nx-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if
        
        if (yi<=y(1)) then
            if (abs(yi-y(1))<1d-10) then
                yLocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (yi>=y(ny)) then
            if (abs(yi-y(ny))<1d-10) then
                yLocL = ny-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if
        
        if (zi<=z(1)) then
            if (abs(zi-z(1))<1d-10) then
                zLocL = 1
            else
                write(*,*) "Interpolation: Point below lower bound of grid"
                return
            end if
        end if

        if (zi>=z(nz)) then
            if (abs(zi-z(nz))<1d-10) then
                zLocL = nz-1
            else
                write(*,*) "Interpolation: Point above upper bound of grid"
                return
            end if
        end if

        xL  = x(xlocL)
        xH  = x(xlocL +1)
        yL  = y(ylocL)
        yH  = y(ylocL +1)
        zL  = z(zlocL)
        zH  = z(zlocL +1)
        
        fLLL = f(xlocL,ylocL,zlocL)
        fLLH = f(xlocL,ylocL,zlocL+1)
        fLHL = f(xlocL,ylocL+1,zlocL)
        fLHH = f(xlocL,ylocL+1,zlocL+1)
        fHHH = f(xlocL+1,ylocL+1,zlocL+1)
        fHHL = f(xlocL+1,ylocL+1,zlocL)
        fHLH = f(xlocL+1,ylocL,zlocL+1)
        fHLL = f(xlocL+1,ylocL,zlocL)

        xd = (xi-xL)/(xH-xL)
        yd = (yi-yL)/(yH-yL)
        zd = (zi-zL)/(zH-zL)
        
        c00 = fLLL*(1d0-xd) + fHLL*xd
        c01 = fLLH*(1d0-xd) + fHLH*xd
        c10 = fLHL*(1d0-xd) + fHHL*xd
        c11 = fLHH*(1d0-xd) + fHHH*xd
        
        c0 = c00*(1d0-yd) + c10*yd
        c1 = c01*(1d0-yd) + c11*yd
        
        fi = c0*(1d0-zd) + c1*zd
        
    end subroutine TriLinInterp1

end module
