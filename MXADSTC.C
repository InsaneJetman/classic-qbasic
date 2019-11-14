#include <string.h>

/* Function Prototypes force either correct data typing or compiler warnings.
 * Note all functions exported to BASIC and all BASIC callback (extern)
 * functions are declared with the far pascal calling convention.
 * WARNING: This must be compiled with the Medium memory model (/AM)
 */

char * pascal addstring( char far *s1, int s1len,
              char far *s2, int s2len );
extern void far pascal StringAssign( char far *source, int slen,
                       char far *dest, int dlen );

/* Declare global char array to contain new BASIC string descriptor.
 */
char BASICDesc[4];

char * pascal addstring( char far *s1, int s1len,
              char far *s2, int s2len )
{
    char TS1[50];
    char TS2[50];
    char TSBig[100];

    /* Use the BASIC callback StringAssign to retrieve information
     * from the descriptors, s1 and s2, and place them in the temporary
     * arrays TS1 and TS2.
     */
    StringAssign( s1, 0, TS1, 49 );	/* Get S1 as array of char */
    StringAssign( s2, 0, TS2, 49 );	/* Get S2 as array of char */

    /* Copy the data from TS1 into TSBig, then append the data from
     * TS2.
     */
    memcpy( TSBig, TS1, s1len );
    memcpy( &TSBig[s1len], TS2, s2len );

    StringAssign( TSBig, s1len + s2len, BASICDesc, 0 );

    return BASICDesc;
}

/*
 * If, for example, we wanted to return not just one variable length string,
 * but rather the variable length string and the reverse of that:
 *
 * call addstring( "foo ", 4, "bar", 3, a$, r$ )
 *
 * you get "foo bar" in a$ and "rab oof" in r$.
 *
 * Say you give me s1, and s2 (and their respective lengths) on input; for
 * output, I want s3 and s4.
 *
 * Change the StringAssign for TSBig to assign to s3 instead of BASICDesc.
 *
 * Add the following lines of code:
 *
 *     TSBig[s1len + s2len] = '\0';
 *     strrev( TSBig );
 *     StringAssign( TSBig, s1len + s2len, s4, 0 );
 *
 * Delete the return statement.
 *
 * Change the prototype and function header to say:
 *
 * void far pascal addstring
 *
 * instead of
 *
 * char far * pascal addstring
 */
