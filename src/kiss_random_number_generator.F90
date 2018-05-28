module kiss_random_number_generator
! period 5*2^1320480*(2^64-1)

  implicit none

  integer,parameter :: I8=8
  integer,parameter :: I16=16
  integer,parameter :: R8=8
  integer,parameter :: R16=16

  integer(kind=I8)  :: Q(20632)
  integer(kind=I8)  :: carry=36243678541_I8
  integer(kind=I8)  :: xcng=12367890123456_I8
  integer(kind=I8)  :: xs=521288629546311_I8
  integer(kind=I8)  :: indx=20633_I8

  integer(kind=I8)  :: I8_max = huge(xs)
  integer(kind=I8)  :: I8_min = -(huge(xs))
  integer(kind=I16)  :: I16_range = 2*huge(xs)+1

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


!   from
!   https://www.thecodingforums.com/threads/superkiss-for-32-and-64-bit-rngs-in-both-c-and-fortran.706893/

!   On Nov 3 I posted
!
!   RNGs: A Super KISS sci.math, comp.lang.c, sci.crypt
!
!   a KISS (Keep-It-Simple-Stupid) RNG combining,
!   by addition mod 2^32, three simple RNGs:
!   CMWC(Complementary-Multiply-With-Carry)+CNG(Congruential)
!   +XS(Xorshift)
!   with resulting period greater than 10^402575.
!
!   The extreme period comes from finding a prime p=a*b^r+1 for
!   which the order of b has magnitude near p-1, then use
!   the CMWC method, the mathematics of which I outline here:
!
!   Let Z be the set of all "vectors" of the form
!   [x_1,...,x_r;c] with 0<=x_i<b and 0<=c<a.
!   Then Z has ab^r elements, and if the function f() on Z is
!   f([x_1,x2,...,x_r;c])=
!   [x_2,...,x_r,b-1-(t mod b);trunc((t/b)], t=a*x_1+c
!   then f() has an inverse on Z: for each z in Z there is exactly
!   one w in Z for which f(w)=z:
!   f^{-1}([x_1,x2,...,x_r;c])=
!   [trunc((v/a),x_1,...,x_{r-1}; v mod a], v=cb+(b-1)-x_r
!
!   Thus a directed graph based on z->f(z) will consist only
!   of disjoint loops of size s=order(b,p) and there will be
!   L=ab^r/s such loops.
!
!   A random uniform choice of z from Z is equally likely to
!   fall in any one of the L loops, and then each "vector" in
!   the sequence obtained by iterating f:
!   f(z), f(f(z)), f(f(f(z))),...
!   will have a uniform distribution over that loop, and the sequence
!   determined by taking the r'th element from each "vector",
!   (the output of the CMWC RNG) will be periodic with period
!   the order of b for the prime p=ab^r+1, and that sequence
!   will produce, in reverse order, the base-b expansion of a
!   rational k/p for some k determined by choice of the seed z.
!
!   For that Nov 3 post, I had found that the order of b=2^32
!   for the prime p=7010176*b^41790+1 is 54767*2^1337279, about
!   10^402566, thus providing an easily-implemented
!   KISS=CMWC+CNG+XS RNG with immense period and
!   excellent performance on tests of randomness.
!
!   That easy implementation required carrying out the essential
!   parts of the CMWC function f(): form t=7010176*x+c in 64 bits,
!   extract the top 32 bits for the new c, the bottom 32 for
!   the new x---easy to do in C, not easy in Fortran.
!   And if we want 64-bit random numbers, with B=2^64, our prime
!   becomes 7010176*B^20985+1, for which the period of B is
!   54767*2^1337278, still immense, but in C, with 64-bit x,c
!   there seems no easy way to form t=7010176*x+c in 128 bits,
!   then extract the top and bottom 64 bit halves.
!
!   So base b=2^32 works for C but not Fortran,
!   and base B=2^64 works for neither C nor Fortran.
!
!   I offer here is a prime that provides CMWC RNGs for both
!   32- and 64-bits, and for both C and Fortran, and with
!   equally massive periods, again greater than 2^(1.3million):
!
!   p=640*b^41265+1 = 2748779069440*B^20632+1 = 5*2^1320487+1.
!
!   That prime came from the many who have dedicated their
!   efforts and computer time to prime searches. After some
!   three weeks of dedicated computer time using pfgw with
!   scrypt, I found the orders of b and B:
!   5*2^1320481 for b=2^32, 5*2^1320480 for B=2^64.
!
!   It is the choice of the "a" that makes it feasible to get
!   the top and bottom valves of t=a*x+c, yet stay within the
!   integer sizes the C or Fortran compilers are set for.
!   In the above prime: a=640=2^9+2^7 for b=2^32 and
!   a=2748779069440=2^41+2^39 for B=2^64.
!   Thus, for example with b=2^32 and using only 32-bit C code,
!   with a supposed 128-bit t=(2^9+2^7)*x+c, the top and bottom
!   32-bits of t may be obtained by setting, say,
!   h=(c&1); z=(x<<9)>>1 + (x<<7)>>1 + c>>1;
!   then the top half of that t would be
!   c=(x>>23)+(x>>25)+(z>>31);
!   and the bottom half, before being complemented, would be
!   x=(z<<1)+h;
!
!   When B=2^64 we need only change to
!   h=(c&1); z=(x<<41)>>1 + (x<<39)>>1 + c>>1;
!   c=(x>>23)+(x>>25)+(z>>63);
!
!   These C operations all have Fortran equivalents, and will
!   produce the required bit patterns, whether integers are
!   considered signed or unsigned. (In C, one must make sure
!   that the >> operation performs a logical right shift,
!   perhaps best done via "unsigned" declarations.)
!
!   The CMWC z "vector" elements [x_1,x_2,...,x_r] are kept in
!   an array, Q[] in C, Q() in Fortran, with a separate current
!   "carry". This is all spelled out in the following examples:
!   code for 32- and 64-bit SuperKiss RNGs for C and Fortran.
!
!   Note that in these sample listings, the Q array is seeded
!   by CNG+XS, based on the seed values specified in the
!   initial declarations. For many simulation studies, the
!   73 bits needed to seed the initial xcng, xs and carry<a
!   for the 32-bit version, or 169 bits needed for the 64-bit
!   version, may be adequate.
!   But more demanding applications may require a significant
!   portion of the >1.3 million seed bits that Q requires.
!   See text and comments from the Nov 3 posting.
!
!   I am indebted to an anonymous mecej4 for providing the basic
!   form and KIND declarations of the Fortran versions.
!
!   Please let me and other readers know if the results are not
!   as specified when run with your compilers, or if you can
!   provide equivalent versions in other programming languages.
!
!   George Marsaglia
!
!   --------------------------------------------------------
!   Here is SUPRKISS64.c, the immense-period 64-bit RNG. I
!   invite you to cut, paste, compile and run to see if you
!   get the result I do. It should take around 20 seconds.
!   --------------------------------------------------------
!   /* SUPRKISS64.c, period 5*2^1320480*(2^64-1) */
!   #include <stdio.h>
!   static unsigned long long Q[20632],carry=36243678541LL,
!   xcng=12367890123456LL,xs=521288629546311LL,indx=20632;
!
!   #define CNG ( xcng=6906969069LL*xcng+123 )
!   #define XS ( xs^=xs<<13,xs^=xs>>17,xs^=xs<<43 )
!   #define SUPR ( indx<20632 ? Q[indx++] : refill() )
!   #define KISS SUPR+CNG+XS
!
!   unsigned long long refill( )
!   {int i; unsigned long long z,h;
!   for(i=0;i<20632;i++){ h=(carry&1);
!   z=((Q<<41)>>1)+((Q<<39)>>1)+(carry>>1);
!   carry=(Q>>23)+(Q>>25)+(z>>63);
!   Q=~((z<<1)+h); }
!   indx=1; return (Q[0]);
!   }
!
!   int main()
!   {int i; unsigned long long x;
!   for(i=0;i<20632;i++) Q=CNG+XS;
!   for(i=0;i<1000000000;i++) x=KISS;
!   printf("Does x=4013566000157423768\n x=%LLd.\n",x);
!   }
!   ---------------------------------------------------------
!
!   Here is SUPRKISS32.c, the immense-period 32-bit RNG. I
!   invite you to cut, paste, compile and run to see if you
!   get the result I do. It should take around 10 seconds.
!   ---------------------------------------------------------
!   /*suprkiss64.c
!   b=2^64; x[n]=(b-1)-[(2^41+2^39)*x[n-20632]+carry mod b]
!   period 5*2^1320480>10^397505
!   This version of SUPRKISS doesn't use t=a*x+c in 128 bits,
!   but uses only 64-bit stuff, takes 20 nanos versus 7.5 for
!   the 32-bit unsigned long long t=a*x+c version.
!   */
!
!   /* SUPRKISS64.c, period 5*2^1320480*(2^64-1) */
!   #include <stdio.h>
!   static unsigned long long Q[20632],carry=36243678541LL,
!   xcng=12367890123456LL,xs=521288629546311LL,indx=20632;
!
!   #define CNG ( xcng=6906969069LL*xcng+123 )
!   #define XS ( xs^=xs<<13,xs^=xs>>17,xs^=xs<<43 )
!   #define SUPR ( indx<20632 ? Q[indx++] : refill() )
!   #define KISS SUPR+CNG+XS
!
!   unsigned long long refill( )
!   {int i; unsigned long long z,h;
!   for(i=0;i<20632;i++){ h=(carry&1);
!   z=((Q<<41)>>1)+((Q<<39)>>1)+(carry>>1);
!   carry=(Q>>23)+(Q>>25)+(z>>63);
!   Q=~((z<<1)+h); }
!   indx=1; return (Q[0]);
!   }
!
!   int main()
!   {int i; unsigned long long x;
!   for(i=0;i<20632;i++) Q=CNG+XS;
!   for(i=0;i<1000000000;i++) x=KISS;
!   printf("Does x=4013566000157423768\n x=%LLd.\n",x);
!   }
!
!   -----------------------------------------------------------
!
!   And here are equivalent Fortran versions, which, absent
!   C's inline features, seem to need ~10% more run time.
!
!   -----------------------------------------------------------
!   module suprkiss64_M ! period 5*2^1320480*(2^64-1)
!   integer,parameter :: I8=selected_int_kind(18)
!   integer(kind=I8) :: Q(20632),carry=36243678541_I8, &
!   xcng=12367890123456_I8,xs=521288629546311_I8,indx=20633_I8
!   contains
!   function KISS64() result(x)
!   integer(kind=I8) :: x
!   if(indx <= 20632)then; x=Q(indx); indx=indx+1
!   else; x=refill(); endif
!   xcng=xcng*6906969069_I8+123
!   xs=ieor(xs,ishft(xs,13))
!   xs=ieor(xs,ishft(xs,-17))
!   xs=ieor(xs,ishft(xs,43))
!   x=x+xcng+xs
!   return; end function KISS64
!
!   function refill() result(s)
!   integer(kind=I8) :: i,s,z,h
!   do i=1,20632
!   h=iand(carry,1_I8)
!   z = ishft(ishft(Q(i),41),-1)+ &
!   ishft(ishft(Q(i),39),-1)+ &
!   ishft(carry,-1)
!   carry=ishft(Q(i),-23)+ishft(Q(i),-25)+ishft(z,-63)
!   Q(i)=not(ishft(z,1)+h)
!   end do
!   indx=2; s=Q(1)
!   return; end function refill
!
!   end module suprkiss64_M
!
!   program testKISS64
!   use suprkiss64_M
!   integer(kind=I8) :: i,x
!   do i=1,20632 !fill Q with Congruential+Xorshift
!   xcng=xcng*6906969069_I8+123
!   xs=ieor(xs,ishft(xs,13))
!   xs=ieor(xs,ishft(xs,-17))
!   xs=ieor(xs,ishft(xs,43))
!   Q(i)=xcng+xs
!   end do
!   do i=1,1000000000_I8; x=KISS64(); end do
!   write(*,10) x
!   10 format(' Does x = 4013566000157423768 ?',/,6x,'x = ',I20)
!   end program testKISS64
!   -------------------------------------------------------------
!
!   module suprkiss32_M ! period 5*2^1320481*(2^32-1)
!   integer,parameter :: I4=selected_int_kind(9)
!   integer(kind=I4) :: Q(41265),carry=362_I4, &
!   xcng=1236789_I4,xs=521288629_I4,indx=41266_I4
!   contains
!   function KISS32() result(x)
!   integer(kind=I4):: x
!   if(indx <= 41265)then;x=Q(indx); indx=indx+1
!   else; x=refill(); endif
!   xcng=xcng*69069_I4+123
!   xs=ieor(xs,ishft(xs,13))
!   xs=ieor(xs,ishft(xs,-17));
!   xs=ieor(xs,ishft(xs,5))
!   x=x+xcng+xs
!   return; end function KISS32
!
!   function refill() result(s)
!   integer(kind=I4) :: i,s,z,h
!   do i = 1,41265
!   h = iand(carry,1_I4)
!   z = ishft(ishft(Q(i),9),-1)+ &
!   ishft(ishft(Q(i),7),-1)+ &
!   ishft(carry,-1)
!   carry=ishft(Q(i),-23)+ishft(Q(i),-25)+ishft(z,-31)
!   Q(i)=not(ishft(z,1)+h)
!   end do
!   indx=2; s=Q(1)
!   return; end function refill
!
!   end module suprkiss32_M
!
!   program testKISS32
!   use suprkiss32_M
!   integer(kind=I4) :: i,x
!   do i=1,41265 !fill Q with Congruential+Xorshift
!   xcng=xcng*69069_I4+123
!   xs=ieor(xs,ishft(xs,13))
!   xs=ieor(xs,ishft(xs,-17))
!   xs=ieor(xs,ishft(xs,5))
!   Q(i)=xcng+xs
!   end do
!   do i=1,1000000000_I4; x=KISS32(); end do
!   write(*,10) x
!   10 format(' Does x = 1809478889 ?',/,6x,'x =',I11)
!   end program testKISS32
!
!   ---------------------------------------------------------------
!
!
! geo, Nov 27, 2009
! #1

!-------------------------------------------------------------------------------

  function kiss64_uniform_rng()                     result(unif)

    real (kind=R8)      :: unif

    ! [ LOCALS ]
    integer (kind=I8)   :: x

    x = kiss64_rng()
    unif = ( real(x, kind=R8) - real(I8_min, kind=R8) ) / real(I16_range,kind=R16)

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
