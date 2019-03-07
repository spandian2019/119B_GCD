/****************************************************************************/
/*                                                                          */
/*                                  GCDTEST                                 */
/*                      Test Program for GCD Algorithms                     */
/*                                                                          */
/****************************************************************************/

/*
   Description:      This program tests GCD functions.  It does this by
                     generating two 16-bit random values and then sending
                     them to the functions EuclidGCD and GCD.  It compares
                     the results from those two functions.  The number of
                     tests to perform is entered on the command line.

   Input:            The number of tests to perform is entered on the command
                     line.
   Output:           Any errors in the test results and a summary of the tests
                     is output to stdout.

   Return Code:      If there is an error in the command line argument (the
                     number of tests) ARG_ERR_EXIT is returned, if there is a
                     failure in the testing ERROR_EXIT is returned, otherwise
                     GOOD_EXIT is returned.

   User Interface:   The user inputs the number of tests to perform on the
                     command line.  The test results are output to stdout.

   Error Handling:   The number of tests is checked and if invalid, an error
                     message is output and a non-zero return code generated.

   Algorithms:       Euclid's algorithm is used to test the GCD results.
   Data Structures:  None.


   Revision History:
      10 May 98  Glen George            Initial revision.
      16 May 98  Glen George            Changed all variables associated with
                        the number of tests to long ints so
                    can do more tests.
      15 May 00  Glen George            Changed the statistic output to use
                        better wording.
      15 May 00  Glen George            Updated comments.
      16 May 02  Glen George            Updated comments.
*/



/* library include files */
#include  <limits.h>
#include  <time.h>
#include  <stdio.h>
#include  <stdlib.h>

/* local include files */
#include  "gcdtest.h"




int  main(int narg, char *args[])
{
    /* variables */
    long int      test_cnt;     /* number of tests to perform */

    unsigned int  m;            /* one test value */
    unsigned int  n;            /* the other test value */

    long int      error_cnt = 0;    /* number of errors */
    long int      i;            /* general loop index */



    /* check the command line arguments */
    if (narg != 2)  {
        fprintf(stderr, USAGE_MSG, args[0]);
        return  ARG_ERR_EXIT;
    }

    /* get the number of tests from the only argument */
    test_cnt = atol(args[1]);
    if (test_cnt <= 0)  {
    fprintf(stderr, "%s:  illegal number of tests (%s), must be positive", args[0], args[1]);
        fprintf(stderr, USAGE_MSG, args[0]);
        return  ARG_ERR_EXIT;
    }


    /* no error, do the tests */

    /* seed the random number generator */
    srand(time(NULL));
    
    /* now do the testing */
    for (i = 0; i < test_cnt; i++)  {

    /* get two values to test with */
    m = (rand()%32768) + (rand()%32768) + 1;    /* to make a 16-bit value */
    n = (rand()%32768) + (rand()%32768) + 1;
    
    if (i == 0)
        printf("%d\n", m);

    /* check the GCD algorithm */
    if (EuclidGCD(m, n) != GCD(m, n))  {

        /* have an error - report it */
        printf("\nEuclidGCD(%u, %u) = %u != %u = GCD(%u, %u)\n", m, n, EuclidGCD(m, n), GCD(m, n), m, n);
        /* increment the error count */
        error_cnt++;
        }
    }

    /* done testing - report the results */
    printf("\n%ld Tests:  %ld PASS    %ld FAIL\n", test_cnt, test_cnt - error_cnt, error_cnt);
    /* output the statistics too */
    ShowStatistics();

    /* now exit with the proper return code */
    if (error_cnt != 0)
        return  ERROR_EXIT;
    else
        return  GOOD_EXIT;

}




/*
   EuclidGCD(unsigned int, unsigned int)

   Description:      This function computes the GCD of its arguments.  If
                     either argument is zero, zero is returned.

   Arguments:        m (unsigned int) - first value for which to compute the
                        GCD.
             n (unsigned int) - second value for which to compute the
                        GCD.
   Return Value:     (unsigned int) - GCD of the two arguments.

   Inputs:           None.
   Outputs:          None.

   Error Handling:   If either argument is zero (0), zero (0) is returned.

   Algorithms:       Euclid's Division Algorithm.
   Data Structures:  None.

   Last Modified:    10 May 98
*/

unsigned int  EuclidGCD(unsigned int m, unsigned int n)
{
    /* variables */
    unsigned int  r;        /* the remainder */


    /* check to be sure neither argument is 0 */
    if ((m == 0) || (n == 0))
        /* have a zero argument - return with zero */
    return  0;


    /* arguments are OK */

    /* loop to compute the GCD */
    while (n != 0)  {
        r = m % n;
    m = n;
    n = r;
    }


    /* done computing the GCD, return */
    return  m;

}




/* static variables for statistic collection */
static int       min = INT_MAX; /* minimum number of iterations seen */
static int       max = INT_MIN; /* maximum number of iterations seen */
static long int  no_pts = 0;    /* number of iteration counts seen */
static double    sum = 0.0; /* sum of the iteration counts (for average) */




/*
   addIterCnt(int)

   Description:      This function keeps track of statistics for iteration
                     counts.  It is passed the number of iterations and it
                     stores the minimum, maximum, and average number of
                     iterations.

   Arguments:        cnt (int) - number of iterations on which to collect
                                 statistics.
   Return Value:     None.

   Inputs:           None.
   Outputs:          None.

   Error Handling:   None.

   Algorithms:       None.
   Data Structures:  static variables are used to collect information.

   Last Modified:    10 May 98
*/

void  addIterCnt(int cnt)
{
    /* variables */
      /* none */


    /* check for min and max */
    if (cnt < min)
        min = cnt;
    if (cnt > max)
        max = cnt;

    /* keep track of averaging information */
    no_pts++;       /* another count */
    sum += cnt;     /* keep track of sum */


    /* all done collecting info - return */
    return;

}




/*
   ShowStatistics()

   Description:      This function outputs the statistics that have been kept
                     by addIterCnt.  It just outputs the minimum number of
             iterations, the maximum number of iterations, and the
             average number of iterations.

   Arguments:        None.
   Return Value:     None.

   Inputs:           None.
   Outputs:          The minimum, maximum, and average number of iterations
             are output to stdout.

   Error Handling:   None.

   Algorithms:       None.
   Data Structures:  static variables are used to collect information.

   Last Modified:    15 May 00
*/

void  ShowStatistics(void)
{
    /* variables */
      /* none */


    /* just output all the information */
    printf("\nMinimum Number of Iterations: %d\n", min);
    printf("Maximum Number of Iterations: %d\n", max);
    printf("Average Number of Iterations: %lf\n", (sum / no_pts));


    /* all done outputting info - return */
    return;

}