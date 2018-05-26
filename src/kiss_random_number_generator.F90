module kiss_random_number_generator
! period 5*2^1320480*(2^64-1)

  implicit none

  integer,parameter :: I8=selected_int_kind(18)
  integer,parameter :: I10=selected_int_kind(21)
  integer,parameter :: R8=selected_real_kind(18,18)
  integer,parameter :: R10=selected_real_kind(21,12)

  integer(kind=I8)  :: Q(20632)
  integer(kind=I8)  :: carry=36243678541_I8
  integer(kind=I8)  :: xcng=12367890123456_I8
  integer(kind=I8)  :: xs=521288629546311_I8
  integer(kind=I8)  :: indx=20633_I8

  integer(kind=I8)  :: I8_max = huge(xs)
  integer(kind=I8)  :: I8_min = -(huge(xs) + 1)
  integer(kind=I10)  :: I10_range = 2*huge(xs) + 1

contains

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
!
! CNG is easily implemented on machines with 64-bit integers,
! as arithmetic is automatically mod 2^64, whether integers
! are considered signed or unsigned. The CNG statement is
! z=6906969069*z+1234567.
! When I established the lattice structure of congruential
! generators in the 60's, a search produced 69069 as an easy-
! to-remember multiplier with nearly cubic lattices in 2,3,4,5-
! space, so I tried concatenating, using 6906969069 as
! my first test multiplier. Remarkably---a seemingly one in many
! hundreds chance---it turned out to also have excellent lattice
! structure in 2,3,4,5-space, so that's the one chosen.
! (I doubt if lattice structure of CNG has much influence on the
! composite 64-bit KISS produced via MWC+XSH+CNG mod 2^64.)
!
!
! XSH, the Xorshift component, described in
! www.jstatsoft.org/v08/i14/paper
! uses three invocations of an integer "xor"ed with a shifted
! version of itself.
! The XSH component used for this KISS is, in C notation:
! y^=(y<<13); y^=(y>>17); y^=(y<<43)
! with Fortran equivalents y=ieor(y,ishft(y,13)), etc., although
! this can be effected by a Fortran statement function:
! f(y,k)=ieor(y,ishft(y,k))
! y=f(f(f(y,13),-17),43)
! As with lattice structure, choice of the triple 13,-17,43 is
! probably of no particular importance; any one of the 275 full-
! period triples listed in the above article is likely to provide
! a satisfactory component XSH for the composite MWC+XSH+CNG.
!
! The choice of multiplier 'a' for the multiply-with-carry (MWC)
! component of KISS is not so easily made. In effect, a multiply-
! with-carry sequence has a current value x and current "carry" c,
! and from each given x,c a new x,c pair is constructed by forming
! t=a*x+c, then x=t mod b=2^64 and c=floor(t/b).
! This is easily implemented for 32-bit computers that permit
! forming a*t+c in 64 bits, from which the new x is the bottom and
! the new c the top 32-bits.
!
! When a,x and c are 64-bits, not many computers seem to have an easy
! way to form t=a*x+c in 128 bits, then extract the top and bottom
! 64-bit segments. For that reason, special choices for 'a' are
! needed among those that satisfy the general requirement that
! p=a*b-1 is a prime for which b=2^64 has order (p-1)/2.
!
! My choice---and the only one of this form---is a=2^58+1. Then the
! top 64 bits of an imagined 128-bit t=a*x+c may be obtained as
! (using C notation) (x>>6)+ 1 or 0, depending
! on whether the 64-bit parts of (x<<58)+c+x cause an overflow.
! Since (x<<58)+c cannot itself cause overflow (c will always be <a),
! we get the carry as c=(x>>6) plus overflow from (x<<58)+x.
!
! This is easily done in C with unsigned integers, using a different
! kind of 't': t=(x<<58)+c; c=(x>>6); x=t+x; c=c+(x<t);
! For Fortran and others that permit only signed integers, more work
! is needed.
! Equivalent mod 2^64 versions of t=(x<<58)+c and c=(x>>6) are easy,
! and if s(x) represents (x>>63) in C or ishft(x,-66) in Fortran,
! then for signed integers, the new carry c comes from the rule
! if s(x) equals s(t) then c=(x>>6)+s(x) else c=(x>>6)+1-s(x+t)
!
! Speed:
! A C version of this KISS RNG takes 18 nanosecs for each
! 64-bit random number on my desktop (Vista) PC, thus
! producing KISSes at a rate exceeding 55 million per second.
! Fortran or other integers-must-be-signed compilers might get
! "only" around 40 million per second.
!
! Setting seeds:
! Use of KISS or KISS() as a general 64-bit RNG requires specifying
! 3*64+58=250 bits for seeds, 64 bits each for x,y,z and 58 for c,
! resulting in a composite sequence with period around 2^250.
! The actual period is
! (2^250+2^192+2^64-2^186-2^129)/6 ~= 2^(247.42) or 10^(74.48).
! We "lose" 1+1.58=2.58 bits from maximum possible period, one bit
! because b=2^64, a square, cannot be a primitive root of p=ab-1,
! so the best possible order for b is (p-1)/2.
! The periods of MWC and XSH have gcd 3=2^1.58, so another 1.58
! bits are "lost" from the best possible period we could expect
! from 250 seed bits.
!
! Some users may think 250 seed bits are an unreasonable requirement.
! A good seeding procedure might be to assume the default seed
! values then let the user choose none, one, two,..., or all
! of x,y,z, and c to be reseeded.
!
! Tests:
! Latest tests in The Diehard Battery, available at
! http://i.cs.hku.hk/~diehard/
! were applied extensively. Those tests that specifically required
! 32-bit integers were applied to the leftmost 32 bits
! (e,g, KISS>>32;), then to the middle 32-bits ((KISS<<16)>>32;)
! then to the rightmost 32 bits, ( (KISS<<32)>>32).
! There were no extremes in the more than 700 p-values returned
! by the tests, nor indeed for similar tests applied to just two of the
! KISS components: MWC+XSH, then MWC+CNG, then XSH+CNG.
!
! The simplicity, speed, period around 2^250 and performance on
! tests of randomness---as well as ability to produce exactly
! the same 64-bit patterns, whether considered signed or unsigned
! integers---make this 64-bit KISS well worth considering for
! adoption or adaption to languages other than C or Fortran,
! as has been done for 32-bit KISSes.
!
! George Marsaglia

!

!-------------------------------------------------------------------------------

  function kiss64_uniform_rng()                     result(unif)

    real (kind=R8)      :: unif

    ! [ LOCALS ]
    integer (kind=I8)   :: x

    x = kiss64_rng()
    unif = ( real(x, kind=R8) - real(I8_min, kind=R8) ) / real(I10_range,kind=R10)

  end function kiss64_uniform_rng

!-------------------------------------------------------------------------------

  function kiss64_rng()                             result(x)

    integer(kind=I8) :: x
    if(indx <= 20632) then
      x=Q(indx)
      indx=indx+1
    else
      x=refill()
    endif
    xcng = xcng * 6906969069_I8+123
    xs = ieor(xs,ishft(xs,13))
    xs = ieor(xs,ishft(xs,-17))
    xs = ieor(xs,ishft(xs,43))
    x = x + xcng + xs

  end function kiss64_rng

!-------------------------------------------------------------------------------

  subroutine initialize_kiss_rng(seed)

    integer(kind=I8), optional    :: seed
    integer(kind=I8)              :: i,x

    do i=1,20632 !fill Q with Congruential+Xorshift
      xcng=xcng*6906969069_I8+123
      xs=ieor(xs,ishft(xs,13))
      xs=ieor(xs,ishft(xs,-17))
      xs=ieor(xs,ishft(xs,43))
      Q(i)=xcng+xs
    end do

    if ( present(seed) ) then

      ! 'seed' simply specifies where in our long string of pseudo-random
      ! numbers we begin
      do i=1, seed
        x = kiss64_rng()
      enddo

    endif

  end subroutine initialize_kiss_rng

!-------------------------------------------------------------------------------

  function refill()                                 result(s)
    integer(kind=I8) :: i, s, z, h
    do i=1, 20632
      h=iand(carry,1_I8)
      z = ishft(ishft(Q(i),41),-1)                                             &
          + ishft(ishft(Q(i),39),-1)                                           &
          + ishft(carry,-1)
      carry=ishft(Q(i),-23)+ishft(Q(i),-25)+ishft(z,-63)
      Q(i)=not(ishft(z,1)+h)
    end do
    indx=2
    s=Q(1)

  end function refill

end module kiss_random_number_generator
