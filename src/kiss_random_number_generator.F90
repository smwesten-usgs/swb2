module kiss_random_number_generator

  use iso_c_binding, only   : c_int, c_float
  use iso_fortran_env, only : int64, real128
  implicit none

  integer (int64) :: x = 1234567890987654321_int64
  integer (int64) :: y = 362436362436362436_int64
  integer (int64) :: z = 1066149217761810_int64
  integer (int64) :: c = 123456123456123456_int64
  integer (int64) :: t

contains

! original post found here:
! https://www.thecodingforums.com/threads/64-bit-kiss-rngs.673657/

! 64-bit KISS RNGs
!
! Consistent with the Keep It Simple Stupid (KISS) principle,
! I have previously suggested 32-bit KISS Random Number
! Generators (RNGs) that seem to have been frequently adopted.
!
! Having had requests for 64-bit KISSes, and now that
! 64-bit integers are becoming more available, I will
! describe here a 64-bit KISS RNG, with comments on
! implementation for various languages, speed, periods
! and performance after extensive tests of randomness.
!
! This 64-bit KISS RNG has three components, each nearly
! good enough to serve alone. The components are:
! Multiply-With-Carry (MWC), period (2^121+2^63-1)
! Xorshift (XSH), period 2^64-1
! Congruential (CNG), period 2^64
!
! Compact C and Fortran listings are given below. They
! can be cut, pasted, compiled and run to see if, after
! 100 million calls, results agree with that provided
! by theory, assuming the default seeds.
!
! Users may want to put the content in other forms, and,
! for general use, provide means to set the 250 seed bits
! required in the variables x,y,z (64 bits) and c (58 bits)
! that have been given default values in the test versions.
!
! The C version uses #define macros to enumerate the few
! instructions that MWC, XSH and CNG require. The KISS
! macro adds MWC+XSH+CNG mod 2^64, so that KISS can be
! inserted at any place in a C program where a random 64-bit
! integer is required.
! Fortran's requirement that integers be signed makes the
! necessary code more complicated, hence a function KISS().
!
! C version; test by invoking macro KISS 100 million times
! -----------------------------------------------------------------
! #include <stdio.h>
!
! static unsigned long long
! x=1234567890987654321ULL,c=123456123456123456ULL,
! y=362436362436362436ULL,z=1066149217761810ULL,t;
!
! #define MWC (t=(x<<58)+c, c=(x>>6), x+=t, c+=(x<t), x)
! #define XSH ( y^=(y<<13), y^=(y>>17), y^=(y<<43) )
! #define CNG ( z=6906969069LL*z+1234567 )
! #define KISS (MWC+XSH+CNG)
!
! int main(void)
! {int i;
! for(i=0;i<100000000;i++) t=KISS;
! (t==1666297717051644203ULL) ?
! printf("100 million uses of KISS OK"):
! printf("Fail");
! }
!
! ---------------------------------------------------------------
! Fortran version; test by calling KISS() 100 million times
! ---------------------------------------------------------------
! program testkiss
! implicit integer*8(a-z)
! do i=1,100000000; t=KISS(); end do
! if(t.eq.1666297717051644203_8) then
! print*,"100 million calls to KISS() OK"
! else; print*,"Fail"
! end if; end
!
! function KISS()
! implicit integer*8(a-z)
! data x,y,z,c /1234567890987654321_8, 362436362436362436_8,&
! 1066149217761810_8, 123456123456123456_8/
! save x,y,z,c
! m(x,k)=ieor(x,ishft(x,k)) !statement function
! s(x)=ishft(x,-63) !statement function
! t=ishft(x,58)+c
! if(s(x).eq.s(t)) then; c=ishft(x,-6)+s(x)
! else; c=ishft(x,-6)+1-s(x+t); endif
! x=t+x
! y=m(m(m(y,13_8),-17_8),43_8)
! z=6906969069_8*z+1234567
! KISS=x+y+z
! return; end
! ---------------------------------------------------------------
!
! Output from using the macro KISS or the function KISS()	is
! MWC+XSH+CNG mod 2^64.

!-------------------------------------------------------------------------------

  subroutine kiss64_initialize(seed) 

    integer (kind=c_int), optional :: seed
    integer (int64)                  :: indx
    integer (int64)                  :: pseudorandom_value

    x = 1234567890987654321_int64
    y = 362436362436362436_int64
    z = 1066149217761810_int64
    c = 123456123456123456_int64

    if (present(seed)) then

      do indx=1,seed
        pseudorandom_value = kiss64_rng()
      end do  

    endif

  end subroutine kiss64_initialize

!-------------------------------------------------------------------------------

  function kiss64_uniform_rng()                     result(unif)

    real (c_float)      :: unif

    ! [ LOCALS ]
    integer (int64)   :: x
    real (real128)    :: range

    range = 2.0_real128 * real(huge(1_int64), kind=real128)

    x = kiss64_rng()
    unif = ( real(x, kind=real128) + real(huge(1_int64), kind=real128)) / range

  end function kiss64_uniform_rng

!-------------------------------------------------------------------------------

  function bitwise_exclusive_or_operation(x, k)        result(m)

    integer (int64) :: x, k
    integer (int64) :: m

    m = ieor(x, ishft(x,k))

  end function bitwise_exclusive_or_operation

!-------------------------------------------------------------------------------

  function shift_bits(x)                               result(y)

    integer (int64) :: x
    integer (int64) :: y

    y = ishft(x, -63)

  end function shift_bits

!-------------------------------------------------------------------------------

  function kiss64_rng()                                   result(pseudorandom_value)
    integer (int64) :: pseudorandom_value

    t = ishft(x, 58) + c
    
    if (shift_bits(x) .eq. shift_bits(t)) then
       c = ishft(x, -6) + shift_bits(x)
    else
       c = ishft(x, -6) + 1 - shift_bits(x + t)
    endif

    x = t + x
    y = bitwise_exclusive_or_operation(bitwise_exclusive_or_operation(bitwise_exclusive_or_operation(y,13_int64),-17_int64), 43_int64)
    z = 6906969069_int64 * z + 1234567
    pseudorandom_value = x + y + z
  
  end function kiss64_rng

!-------------------------------------------------------------------------------

end module kiss_random_number_generator
