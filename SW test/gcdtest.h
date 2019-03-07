/****************************************************************************/
/*                                                                          */
/*                                 GCDTEST.H                                */
/*                      Test Program for GCD Algorithms                     */
/*                               Include File                               */
/*                                                                          */
/****************************************************************************/

/*
   This file contains the general defintions for the GCD testing program (main
   program is gcdtest.c).


   Revision History:
      10 May 98  Glen George            Initial revision.
      16 May 98  Glen George            Added defintions for TRUE and FALSE.
      16 May 02  Glen George            Updated comments.
*/



/* Constants */

/* usage message */
#define  USAGE_MSG   "usage:  %s  no_tests"

/* exit codes */
#define  ARG_ERR_EXIT   2   /* an argument error */
#define  ERROR_EXIT 1   /* a testing error */
#define  GOOD_EXIT  0   /* a good exit condition */

/* boolean values */
#define  FALSE      0
#define  TRUE       !FALSE




/* Macros */
    /* none */




/* Structures, Unions, and typedefs */
    /* none */




/* Function Prototypes */

unsigned int  EuclidGCD(unsigned int, unsigned int);    /* Euclid's algorithm for GCD */
unsigned int  GCD(unsigned int, unsigned int);      /* GCD algorithm to test */
void          addIterCnt(int);              /* add iteration count to stats */
void          ShowStatistics(void);         /* output the statistics */