#include <stdio.h>
#include <string.h>

/* Function Prototypes force either correct data typing or compiler warnings.
 * Note all functions exported to BASIC and all BASIC callback (extern)
 * functions are declared with the far pascal calling convention.
 * IMPORTANT: This must be compiled with the Medium memory model (/AM)
 */
void far pascal shakespeare( void );
extern void far pascal addstring( char  ** s1, int * s1len,
				    char ** s2, int * s2len,
				    char ** s3, int * s3len );

void far pascal shakespeare( void )
{
    char * s1 = "To be or not to be;";
    int  s1len;
    char * s2 = " that is the question.";
    int  s2len;
    char s3[100];
    int  s3len;
    char * s3add = s3;

    s1len = strlen( s1 );
    s2len = strlen( s2 );
    addstring( &s1, &s1len, &s2, &s2len, &s3add, &s3len );

    s3[s3len] = '\0';
    printf("\n%s", s3 );
}
