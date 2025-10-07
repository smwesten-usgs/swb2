! F95 program to give useful properties of the (first 6) integer kinds
! It assumes huge(k6) < huge(1.0d0) (true in all 4 compilers I tested)
! By J.F.Harper, Mathematics, Victoria U, Wellington NZ. 22 Nov 2011
program intkinds
  implicit none
  integer    ,parameter:: maxnk = 6, dp = kind(1d0), &
       k1 = selected_int_kind(0)
  integer(k1),parameter:: i1   = 1_k1
  integer    ,parameter:: sik2 = selected_int_kind(range(i1)+1), &
       k2 = (sign(1,sik2)*(sik2-k1) + (sik2+k1))/2
! If kn different from the previous one, it has a larger range.
  integer(k2),parameter:: i2   = 1_k2
  integer    ,parameter:: sik3 = selected_int_kind(range(i2)+1), &
       k3 = (sign(1,sik3)*(sik3-k2) + (sik3+k2))/2
  integer(k3),parameter:: i3   = 1_k3
  integer    ,parameter:: sik4 = selected_int_kind(range(i3)+1), &
       k4 = (sign(1,sik4)*(sik4-k3) + (sik4+k3))/2
  integer(k4),parameter:: i4   = 1_k4
  integer    ,parameter:: sik5 = selected_int_kind(range(i4)+1), &
       k5 = (sign(1,sik5)*(sik5-k4) + (sik5+k4))/2
  integer(k5),parameter:: i5   = 1_k5
  integer    ,parameter:: sik6 = selected_int_kind(range(i5)+1), &
       k6 = (sign(1,sik6)*(sik6-k5) + (sik6+k5))/2
  integer(k6),parameter:: i6   = 1_k6
  integer,parameter:: karray(0:maxnk) = (/-1,k1,k2,k3,k4,k5,k6/)
  integer:: i,iolen,k,nk = -1 ! But see the first do loop below.
  integer(k6):: iprops(maxnk,6)
  real(dp)    :: dpihuge
  character   :: ck(maxnk)*7 = ' '
  iprops(1,1:3) = (/digits(i1),radix(i1),range(i1)/) ! RHS default integer
  iprops(2,1:3) = (/digits(i2),radix(i2),range(i2)/)
  iprops(3,1:3) = (/digits(i3),radix(i3),range(i3)/)
  iprops(4,1:3) = (/digits(i4),radix(i4),range(i4)/)
  iprops(5,1:3) = (/digits(i5),radix(i5),range(i5)/)
  iprops(6,1:3) = (/digits(i6),radix(i6),range(i6)/)
! iprops(:,4  ) = iolen in the second do loop below.
  iprops(1,5:6) = (/int(bit_size(i1),k1),huge(i1)/) ! RHS kind k1
  iprops(2,5:6) = (/int(bit_size(i2),k2),huge(i2)/) ! RHS kind k2
  iprops(3,5:6) = (/int(bit_size(i3),k3),huge(i3)/) ! etc
  iprops(4,5:6) = (/int(bit_size(i4),k4),huge(i4)/)
  iprops(5,5:6) = (/int(bit_size(i5),k5),huge(i5)/)
  iprops(6,5:6) = (/int(bit_size(i6),k6),huge(i6)/)
  do k = maxnk,1,-1
     if (nk < 0 .and. karray(k) /= karray(k-1)) nk = k
     if (iprops(k,6) == int(huge(1),k6)) ck(k) = 'default'
  end do ! so nk = min(6,number of different integer kinds)
  do k = 1,nk
     if (k==1) inquire(iolength=iolen) i1
     if (k==2) inquire(iolength=iolen) i2
     if (k==3) inquire(iolength=iolen) i3
     if (k==4) inquire(iolength=iolen) i4
     if (k==5) inquire(iolength=iolen) i5
     if (k==6) inquire(iolength=iolen) i6
     iprops(k,4) = iolen
     if (karray(k)==kind(1)) ck(k) = 'default'
  end do
  print *
  print *,'Integer kind digits radix range iolen bit_size  huge'
  do k = 1,nk
     print "(1X,A7,I3,5I6,8X,I0)",ck(k),karray(k),(iprops(k,i),i=1,6)
     dpihuge = real(iprops(k,6),dp)
     print "(50X,A,I0,A,T64,A,ES9.2)", &
          '= 2**',nint(log(dpihuge)/log(2._dp)),' - 1','~',dpihuge
  end do
  print *,merge( &
       'No higher integer kind is available.     ', &
       'Warning: there may be more integer kinds.',nk<maxnk)
end program intkinds 
