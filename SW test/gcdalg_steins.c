/****************************************************************************/
/*                                                                          */
/*                                 GCDALG0                                  */
/*                          Euclid's GCD Algorithm                          */
/*                                                                          */
/****************************************************************************/

/*
   This file contains the function for the GCD algorithm.  It just implements
   Stein's algorithm.  The functions included are:
      GCD  -  compute the GCD of its arguments.

   Revision History:
      15 May 00  Glen George            Initial revision.
      16 May 02  Glen George            Updated comments.
      06 Mar 19  Sundar Pandian         revised for Stein's SW Implementation
*/



/* library include files */
#include  <stdio.h>
#include  <stdlib.h>
#include  <math.h>

/* local include files */
#include  "gcdtest.h"




/*
   GCD(unsigned int, unsigned int)

   Description:      This function computes the GCD of its arguments.  If
                     either argument is zero, zero is returned.

   Arguments:        m (unsigned int) - first value for which to compute the
                        GCD
                     n (unsigned int) - second value for which to compute the
                        GCD
   Return Value:     (unsigned int) - GCD of the two arguments.

   Inputs:           None.
   Outputs:          None.

   Error Handling:   If either argument is zero (0), zero (0) is returned.

   Algorithms:       Stein's Algorithm

              Pseudocode
                    k = 0
                    while (a is even AND b is even)
                        k ++ 
                        a /= 2
                        b /= 2
                    if (a is odd)
                        t = -b
                    else
                        t = a
                    do
                        while (t is even)
                            t /= 2
                        if (t > 0)
                            a = t
                        else
                            b = -t
                        t = a-b
                    until (t = 0)
                    return a * 2^k

   Data Structures:  None.

   Last Modified:    06 Mar 19
*/

unsigned int  GCD(unsigned int m, unsigned int n)
{
    /* variables */
    int           outer_iter_cnt = 0; /* count the outer loop number of iterations */
    int           inner_iter_cnt = 0; /* count the inner loop number of iterations */
    int           k = 0; /* number of shifts, for removing powers of 2 
                            and adding back later */
    int           t; /* loop temp variable */

    int           out_m = m; /* save m to print edge cases */
    int           out_n = n; /* save n to print edge cases */

    /* check to be sure neither argument is 0 */
    if ((m == 0) || (n == 0))
        /* have a zero argument - return with zero */
    return  0;

    /* arguments are OK */

    /* divide out powers of 2 from m and n until one of them is odd */
    while ((m % 2 == 0) && (n % 2 == 0)) {
        k += 1;
        m /= 2;
        n /= 2;
    }

    /* start out temp variable before looping */
    if (m % 2 == 1)
        t = -n;
    else
        t = m;

    /* loop to compute the GCD */
    while (t != 0)  {
        /* an iteration of the loop */
        outer_iter_cnt+=1;
        inner_iter_cnt = 0;
        
        /* now do the algorithm */
        
        while (t % 2 == 0) {
            inner_iter_cnt+=1;
            t /= 2;
        }

        if (t >= 0)
            m = t;
        else
            n = -t;

        t = m - n;
    }

    /* done computing the GCD, save the iteration count and return */
    // if (inner_iter_cnt+outer_iter_cnt >= 5)
    if ((out_n == 9 || out_m == 9) && (inner_iter_cnt+outer_iter_cnt == 6))
        printf("%d\t%d\t%d\t%d\n", out_m, out_n, inner_iter_cnt, outer_iter_cnt);
    addIterCnt(inner_iter_cnt+outer_iter_cnt);

    return  m * (pow(2, k));

}