'*** MATB.BAS - Matrix Math Routines for the Matrix Math Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  NOTE:  This sample source code toolbox is intended to demonstrate some
'  of the extended capabilities of Microsoft BASIC 7.1 Professional Development
'  system that can help to leverage the professional developer's time more
'  effectively.  While you are free to use, modify, or distribute the routines
'  in this module in any way you find useful, it should be noted that these are
'  examples only and should not be relied upon as a fully-tested "add-on"
'  library.
'
'  Purpose:
'This toolbox contains routines which perform elementary operations on systems
'of linear equations represented as matrices.  The functions return integer
'error codes in the name and results in the parameter list.  The functions
'matbs?% and matlu?% found in this module are intended for internal use only.
'Error codes returned:
'     0  no error                     -1  matrix not invertible
'    -2  matrix not square            -3  inner dimensions different
'    -4  matrix dimensions different  -5  result matrix dimensioned incorrectly
'    any other codes returned are standard BASIC errors
'
'-------------------------------------------------------------------
'MatDet, MatSEqn, and MatInv all use LU-decomposition to implement Gaussian
'elimination.  A brief explanation of what is meant by an LU matrix is given
'below, followed by simplified versions of the two internal routines used to
'do all elimination.
'
'What is meant by an LU matrix:
'An upper triangle matrix (one with all nonzero entries on or above the main
'diagonal) can be solved immediately.  The goal of Gaussian elimination is to
'transform a non upper triangle system into an equivalent triangular one.
'
'Given a system of equations represented in matrix form by Ax=b, we need a
'linear transformation L such that LA=U where U is and upper triangular matrix.
'Then Ux=LAx=Lb and Ux=Lb is an upper triangular system.
'
'This library explicitly calculates U, but L is never saved in its own array.
'When we do a row operation to create a zero below the main diagonal, we no
'longer need to save that value because we know it is zero.  This leaves the
'space available to save the multiplier used in the row operation.  When
'elimination is completed (ie, when the matrix is upper triangular), these
'multipliers give us a complete record of what we did to A to make it upper
'triangular.  This is equivalent to saying the multipliers represent L.  We now
'have a U and an L stored in the same matrix!  This type of matrix will be
'referred to as an LU matrix, or just LU.
'
'The following code fragments get LU and backsolve Ux=Lb.  The actual routines
'used in the toolbox are much more involved because they implement total
'pivoting and implicit row scaling to reduce round off errors.  However, all the
'extras (pivoting, scaling, error checking) are extraneous to the main routines,
'which total only 20 lines.  If you are unfamilar with this type of matrix math,
'gaining an understanding of these 20 lines is a very good introduction.  Try
'working through a 2x2 or 3x3 example by hand to see what is happening.  The
'numerical techniques used to reduce round off error will not be discussed.
'
'-------------------------------------------------------------------
'Given the coefficient matrix A(1 TO N, 1 TO N) and the vector b(1 TO N),
'the following fragments will find x(1 TO N) satisfying Ax=b using Gaussian
'elimination.
'
'matlu:
'Perform row operations to get all zeroes below the main diagonal.
'Define Rj(1 TO N) to be the vector corresponding to the jth row of A.
'Let Rrow = Rrow + m*Rpvt where m = -Rrow(pvt)/Rpvt(pvt).
'Then A(row, pvt)=0.
'
'** FOR pvt = 1 TO (N - 1)
'**    FOR row = (pvt + 1) TO N
'**       'Save m for later use in the space just made 0.
'**       A(row, pvt) = -A(row, pvt) / A(pvt, pvt)
'**       'Do the row operation.
'**       FOR col = (pvt + 1) TO N
'**          A(row, col) = A(row, col) + A(row, pvt) * A(pvt, col)
'**       NEXT col
'**    NEXT row
'** NEXT pvt
'
'matbs:
'Do the same row operations on b using the multipliers saved in A.
'
'** FOR pvt = 1 TO (N - 1)
'**    FOR row = (pvt + 1) TO N
'**       b(row) = b(row) + A(row, pvt) * b(pvt)
'**    NEXT row
'** NEXT pvt
'
'Backsolve Ux=Lb to find x.
'                               N
'For r = N to 1, x(r) = [b(r) - ä (A(r,c)*x(c))]/A(r,r)
'                              c=r+1
'** FOR row = N TO 1 STEP -1
'**    x(row) = b(row)
'**    FOR col = (row + 1) TO N
'**       x(row) = x(row) - A(row, col) * x(col)
'**    NEXT col
'**    x(row) = x(row) / A(row, row)
'** NEXT row
'
'===================================================================
'$INCLUDE: 'matb.bi'
DECLARE FUNCTION matbsD% (A() AS DOUBLE, b() AS DOUBLE, x() AS DOUBLE)
DECLARE FUNCTION matbsS% (A() AS SINGLE, b() AS SINGLE, x() AS SINGLE)
DECLARE FUNCTION matluD% (A() AS DOUBLE)
DECLARE FUNCTION matluS% (A() AS SINGLE)
DIM SHARED lo AS INTEGER, up AS INTEGER
DIM SHARED continue AS INTEGER, count AS INTEGER
DIM SHARED rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
END

'=======================MatAddC%====================================
'MatAddC% adds two currency type matrices and places the sum in
'the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() + Beta()
'===================================================================
FUNCTION MatAddC% (Alpha() AS CURRENCY, Beta() AS CURRENCY)
ON LOCAL ERROR GOTO cadderr: MatAddC% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and add elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) + Beta(row%, col%)
   NEXT col%
NEXT row%
caddexit:
EXIT FUNCTION
cadderr:
   MatAddC% = (ERR + 5) MOD 200 - 5
   RESUME caddexit
END FUNCTION

'=======================MatAddD%====================================
'MatAddD% adds two double precision matrices and places the sum in
'the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() + Beta()
'===================================================================
FUNCTION MatAddD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
ON LOCAL ERROR GOTO dadderr: MatAddD% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and add elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) + Beta(row%, col%)
   NEXT col%
NEXT row%
daddexit:
EXIT FUNCTION
dadderr:
   MatAddD% = (ERR + 5) MOD 200 - 5
   RESUME daddexit
END FUNCTION

'=======================MatAddI%====================================
'MatAddI% adds two integer matrices and places the sum in
'the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() + Beta()
'===================================================================
FUNCTION MatAddI% (Alpha() AS INTEGER, Beta() AS INTEGER)
ON LOCAL ERROR GOTO iadderr: MatAddI% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and add elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) + Beta(row%, col%)
   NEXT col%
NEXT row%
iaddexit:
EXIT FUNCTION
iadderr:
   MatAddI% = (ERR + 5) MOD 200 - 5
   RESUME iaddexit
END FUNCTION

'=======================MatAddL%====================================
'MatAddL% adds two long integer matrices and places the sum in
'the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() + Beta()
'===================================================================
FUNCTION MatAddL% (Alpha() AS LONG, Beta() AS LONG)
ON LOCAL ERROR GOTO ladderr: MatAddL% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and add elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) + Beta(row%, col%)
   NEXT col%
NEXT row%
laddexit:
EXIT FUNCTION
ladderr:
   MatAddL% = (ERR + 5) MOD 200 - 5
   RESUME laddexit
END FUNCTION

'=======================MatAddS%====================================
'MatAddS% adds two single precision matrices and places the sum in
'the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() + Beta()
'===================================================================
FUNCTION MatAddS% (Alpha() AS SINGLE, Beta() AS SINGLE)
ON LOCAL ERROR GOTO sadderr: MatAddS% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and add elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) + Beta(row%, col%)
   NEXT col%
NEXT row%
saddexit:
EXIT FUNCTION
sadderr:
   MatAddS% = (ERR + 5) MOD 200 - 5
   RESUME saddexit
END FUNCTION

'========================matbsD=====================================
'matbsD% takes a matrix in LU form, found by matluD%, and a vector b
'and solves the system Ux=Lb for x. matrices A,b,x are double precision.
'
'Parameters: LU matrix in A, corresponding pivot vectors in rpvt and cpvt,
'            right side in b
'
'Returns: solution in x, b is modified, rest unchanged
'===================================================================
FUNCTION matbsD% (A() AS DOUBLE, b() AS DOUBLE, x() AS DOUBLE)
ON LOCAL ERROR GOTO dbserr: matbsD% = 0
'do row operations on b using the multipliers in L to find Lb
FOR pvt% = lo TO (up - 1)
   c% = cpvt(pvt%)
   FOR row% = (pvt% + 1) TO up
      r% = rpvt(row%)
      b(r%) = b(r%) + A(r%, c%) * b(rpvt(pvt%))
   NEXT row%
NEXT pvt%
'backsolve Ux=Lb to find x
FOR row% = up TO lo STEP -1
   c% = cpvt(row%)
   r% = rpvt(row%)
   x(c%) = b(r%)
   FOR col% = (row% + 1) TO up
      x(c%) = x(c%) - A(r%, cpvt(col%)) * x(cpvt(col%))
   NEXT col%
   x(c%) = x(c%) / A(r%, c%)
NEXT row%
dbsexit:
EXIT FUNCTION
dbserr:
   matbsD% = ERR
   RESUME dbsexit
END FUNCTION

'========================matbsS=====================================
'matbsS% takes a matrix in LU form, found by matluS%, and a vector b
'and solves the system Ux=Lb for x. matrices A,b,x are single precision.
'
'Parameters: LU matrix in A, corresponding pivot vectors in rpvt and cpvt,
'            right side in b
'
'Returns: solution in x, b is modified, rest unchanged
'===================================================================
FUNCTION matbsS% (A() AS SINGLE, b() AS SINGLE, x() AS SINGLE)
ON LOCAL ERROR GOTO sbserr: matbsS% = 0
'do row operations on b using the multipliers in L to find Lb
FOR pvt% = lo TO (up - 1)
   c% = cpvt(pvt%)
   FOR row% = (pvt% + 1) TO up
      r% = rpvt(row%)
      b(r%) = b(r%) + A(r%, c%) * b(rpvt(pvt%))
   NEXT row%
NEXT pvt%
'backsolve Ux=Lb to find x
FOR row% = up TO lo STEP -1
   c% = cpvt(row%)
   r% = rpvt(row%)
   x(c%) = b(r%)
   FOR col% = (row% + 1) TO up
      x(c%) = x(c%) - A(r%, cpvt(col%)) * x(cpvt(col%))
   NEXT col%
   x(c%) = x(c%) / A(r%, c%)
NEXT row%
sbsexit:
EXIT FUNCTION
sbserr:
   matbsS% = ERR
   RESUME sbsexit
END FUNCTION

'========================MatDetC%===================================
'MatDetC% finds the determinant of a square, currency type matrix
'
'Parameters: A(n x n) matrix, det@ to return the determinant
'
'Returns: matrix A in LU form, determinant
'===================================================================
FUNCTION MatDetC% (A() AS CURRENCY, det@)
ON LOCAL ERROR GOTO cdeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
'make temporary double precision matrix to find pivots
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS DOUBLE
FOR row% = lo TO up
   FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
      Tmp(row%, col%) = CDBL(A(row%, col%))
   NEXT col%
NEXT row%
errcode% = matluD%(Tmp())              'Get LU matrix
IF NOT continue THEN
   IF errcode% = 199 THEN det@ = 0@
   ERROR errcode%
ELSE
   detD# = 1#                          '+/- determinant = product of the pivots
   FOR pvt% = lo TO up
      detD# = detD# * Tmp(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%                           'count contains the total number of row
   det@ = (-1@) ^ count * CCUR(detD#)  'and column switches due to pivoting.
   IF errcode% THEN ERROR errcode%     'multiply the determinant by -1 for
END IF                                 'each switch.
cdetexit:
ERASE rpvt, cpvt, Tmp
MatDetC% = errcode%
EXIT FUNCTION
cdeterr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME cdetexit
END FUNCTION

'========================MatDetD%===================================
'MatDetD% finds the determinant of a square, double precision matrix
'
'Parameters: A(n x n) matrix, det# to return the determinant
'
'Returns: matrix A in LU form, determinant
'===================================================================
FUNCTION MatDetD% (A() AS DOUBLE, det#)
ON LOCAL ERROR GOTO ddeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())             'Get LU matrix
IF NOT continue THEN
   IF errcode% = 199 THEN det# = 0#
   ERROR errcode%
ELSE
   det# = 1#                        '+/- determinant = product of the pivots
   FOR pvt% = lo TO up
      det# = det# * A(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%                         'count contains the total number of row
   det# = (-1) ^ count * det#        'and column switches due to pivoting.
   IF errcode% THEN ERROR errcode%   'multiply the determinant by -1 for
END IF                               'each switch
ddetexit:             
ERASE rpvt, cpvt
MatDetD% = errcode%
EXIT FUNCTION
ddeterr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME ddetexit
END FUNCTION

'========================MatDetI%===================================
'MatDetI% finds the determinant of a square, integer matrix
'
'Parameters: A(n x n) matrix, det% to return the determinant
'
'Returns: matrix A unchanged, determinant
'===================================================================
FUNCTION MatDetI% (A() AS INTEGER, det%)
ON LOCAL ERROR GOTO ideterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
'make temporary single precision matrix to find pivots
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS SINGLE
FOR row% = lo TO up
   FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
      Tmp(row%, col%) = CSNG(A(row%, col%))
   NEXT col%
NEXT row%
errcode% = matluS%(Tmp())              'Get LU matrix
IF NOT continue THEN
   IF errcode% = 199 THEN det% = 0
   ERROR errcode%
ELSE
   detS! = 1!                          '+/- determinant = product of the pivots
   FOR pvt% = lo TO up
      detS! = detS! * Tmp(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%                           'count contains the total number of row
   det% = (-1) ^ count * CINT(detS!)   'and column switches due to pivoting.
   IF errcode% THEN ERROR errcode%     'multiply the determinant by -1 for
END IF                                 'each switch
idetexit:
ERASE rpvt, cpvt, Tmp
MatDetI% = errcode%
EXIT FUNCTION
ideterr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME idetexit
END FUNCTION

'========================MatDetL%===================================
'MatDetL% finds the determinant of a square, long integer matrix
'
'Parameters: A(n x n) matrix, det& to return the determinant
'
'Returns: matrix A unchanged, determinant
'===================================================================
FUNCTION MatDetL% (A() AS LONG, det&)
ON LOCAL ERROR GOTO ldeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
'make temporary double precision matrix to find pivots
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS DOUBLE
FOR row% = lo TO up
   FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
      Tmp(row%, col%) = CDBL(A(row%, col%))
   NEXT col%
NEXT row%
errcode% = matluD%(Tmp())              'Get LU matrix
IF NOT continue THEN
   IF errcode% = 199 THEN det& = 0&
   ERROR errcode%
ELSE
   detD# = 1#                          '+/- determinant = product of the pivots
   FOR pvt% = lo TO up
      detD# = detD# * Tmp(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%                           'count contains the total number of row
   det& = (-1&) ^ count * CLNG(detD#)  'and column switches due to pivoting.
   IF errcode% THEN ERROR errcode%     'multiply the determinant by -1 for
END IF                                 'each switch
ldetexit:
ERASE rpvt, cpvt, Tmp
MatDetL% = errcode%
EXIT FUNCTION
ldeterr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME ldetexit
END FUNCTION

'========================MatDetS%===================================
'MatDetS% finds the determinant of a square, single precision matrix
'
'Parameters: A(n x n) matrix, det! to return the determinant
'
'Returns: matrix A in LU form, determinant
'===================================================================
FUNCTION MatDetS% (A() AS SINGLE, det!)
ON LOCAL ERROR GOTO sdeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())                'Get LU matrix
IF NOT continue THEN
   IF errcode% = 199 THEN det! = 0!
   ERROR errcode%
ELSE
   det! = 1!                           '+/- determinant = product of the pivots
   FOR pvt% = lo TO up
      det! = det! * A(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%                           'count contains the total number of row
   det! = (-1) ^ count * det!          'and column switches due to pivoting.
   IF errcode% THEN ERROR errcode%     'multiply the determinant by -1 for
END IF                                 'each switch
sdetexit:
ERASE rpvt, cpvt
MatDetS% = errcode%
EXIT FUNCTION
sdeterr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME sdetexit
END FUNCTION

'========================MatInvC%===================================
'MatInvC% uses the matluD% and matbsD procedures to invert a square, currency
'type matrix.  Let e(N) contain all zeroes except for the jth position, which
'is 1.  Then the jth column of A^-1 is x, where Ax=e.
'
'Parameters: A(n x n) matrix
'
'Returns: A^-1
'===================================================================
FUNCTION MatInvC% (A() AS CURRENCY)
ON LOCAL ERROR GOTO cinverr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
'duplicate A() in a double precision work matrix, Tmp()
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS DOUBLE
DIM e(lo TO up) AS DOUBLE, x(lo TO up) AS DOUBLE
FOR row% = lo TO up
   FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
      Tmp(row%, col%) = CDBL(A(row%, col%))
   NEXT col%
NEXT row%
errcode% = matluD%(Tmp())                    'Put LU in Tmp
IF NOT continue THEN ERROR errcode%
FOR col% = lo TO up                          'Find A^-1 one column at a time
   e(col%) = 1#
   bserrcode% = matbsD%(Tmp(), e(), x())
   IF bserrcode% THEN ERROR bserrcode%
   FOR row% = lo TO up
      A(row%, col%) = CCUR(x(row%))          'Put the column into A
      e(row%) = 0#
   NEXT row%
NEXT col%
IF errcode% THEN ERROR errcode%
cinvexit:
ERASE Tmp, e, x, rpvt, cpvt
MatInvC% = errcode%
EXIT FUNCTION
cinverr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME cinvexit
END FUNCTION

'========================MatInvD%===================================
'MatInvD% uses the matluD% and matbsD procedures to invert a square, double
'precision matrix.  Let e(N) contain all zeroes except for the jth position,
'which is 1.  Then the jth column of A^-1 is x, where Ax=e.
'
'Parameters: A(n x n) matrix
'
'Returns: A^-1
'===================================================================
FUNCTION MatInvD% (A() AS DOUBLE)
ON LOCAL ERROR GOTO dinverr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM Ain(lo TO up, lo TO up) AS DOUBLE
DIM e(lo TO up) AS DOUBLE, x(lo TO up) AS DOUBLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())                     'Get LU matrix
IF NOT continue THEN ERROR errcode%
FOR col% = lo TO up                         'Find A^-1 one column at a time
   e(col%) = 1#
   bserrcode% = matbsD%(A(), e(), x())
   IF bserrcode% THEN ERROR bserrcode%
   FOR row% = lo TO up
      Ain(row%, col%) = x(row%)
      e(row%) = 0#
   NEXT row%
NEXT col%
FOR col% = lo TO up                         'Put A^-1 in A
   FOR row% = lo TO up
      A(row%, col%) = Ain(row%, col%)
   NEXT row%
NEXT col%
IF errcode% THEN ERROR errcode%
dinvexit:
ERASE e, x, Ain, rpvt, cpvt
MatInvD% = errcode%
EXIT FUNCTION
dinverr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME dinvexit
END FUNCTION

'========================MatInvS%===================================
'MatInvS% uses the matluS% and matbsS procedures to invert a square, single
'precision matrix.  Let e(N) contain all zeroes except for the jth position,
'which is 1. Then the jth column of A^-1 is x, where Ax=e.
'
'Parameters: A(n x n) matrix
'
'Returns: A^-1
'===================================================================
FUNCTION MatInvS% (A() AS SINGLE)
ON LOCAL ERROR GOTO sinverr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM Ain(lo TO up, lo TO up) AS SINGLE
DIM e(lo TO up) AS SINGLE, x(lo TO up) AS SINGLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())                     'Get LU matrix
IF NOT continue THEN ERROR errcode%
FOR col% = lo TO up                         'find A^-1 one column at a time
   e(col%) = 1!
   bserrcode% = matbsS%(A(), e(), x())
   IF bserrcode% THEN ERROR bserrcode%
   FOR row% = lo TO up
      Ain(row%, col%) = x(row%)
      e(row%) = 0!
   NEXT row%
NEXT col%
FOR col% = lo TO up                         'put A^-1 in A
   FOR row% = lo TO up
      A(row%, col%) = Ain(row%, col%)
   NEXT row%
NEXT col%
IF errcode% THEN ERROR errcode%
sinvexit:
ERASE e, x, Ain, rpvt, cpvt
MatInvS% = errcode%
EXIT FUNCTION
sinverr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME sinvexit
END FUNCTION

'========================matluD%====================================
'matluD% does Gaussian elimination with total pivoting to put a square, double
'precision matrix in LU form. The multipliers used in the row operations to
'create zeroes below the main diagonal are saved in the zero spaces.
'
'Parameters: A(n x n) matrix, rpvt(n) and cpvt(n) permutation vectors
'            used to index the row and column pivots
'
'Returns: A in LU form with corresponding pivot vectors; the total number of
'         pivots in count, which is used to find the sign of the determinant.
'===================================================================
FUNCTION matluD% (A() AS DOUBLE)
ON LOCAL ERROR GOTO dluerr: errcode% = 0
'Checks if A is square, returns error code if not
IF NOT (lo = LBOUND(A, 2) AND up = UBOUND(A, 2)) THEN ERROR 198
DIM rownorm(lo TO up) AS DOUBLE
count = 0                            'initialize count, continue
continue = -1
FOR row% = lo TO up                  'initialize rpvt and cpvt
   rpvt(row%) = row%
   cpvt(row%) = row%
   rownorm(row%) = 0#                'find the row norms of A()
   FOR col% = lo TO up
      rownorm(row%) = rownorm(row%) + ABS(A(row%, col%))
   NEXT col%
   IF rownorm(row%) = 0# THEN        'if any rownorm is zero, the matrix
      continue = 0                   'is singular, set error, exit and
      ERROR 199                      'do not continue
   END IF
NEXT row%
FOR pvt% = lo TO (up - 1)
'Find best available pivot
   max# = 0#                         'checks all values in rows and columns not
   FOR row% = pvt% TO up             'already used for pivoting and saves the
      r% = rpvt(row%)                'largest absolute number and its position
      FOR col% = pvt% TO up
         c% = cpvt(col%)
         temp# = ABS(A(r%, c%)) / rownorm(r%)
         IF temp# > max# THEN
            max# = temp#
            bestrow% = row%          'save the position of new max#
            bestcol% = col%
         END IF
      NEXT col%
   NEXT row%
   IF max# = 0# THEN                 'if no nonzero number is found, A is
      continue = 0                   'singular, send back error, do not continue
      ERROR 199
   ELSEIF pvt% > 1 THEN              'check if drop in pivots is too much
      IF max# < (deps# * oldmax#) THEN errcode% = 199
   END IF
   oldmax# = max#
   IF rpvt(pvt%) <> rpvt(bestrow%) THEN
      count = count + 1                    'if a row or column pivot is
      SWAP rpvt(pvt%), rpvt(bestrow%)      'necessary, count it and permute
   END IF                                  'rpvt or cpvt. Note: the rows and
   IF cpvt(pvt%) <> cpvt(bestcol%) THEN    'columns are not actually switched,
      count = count + 1                    'only the order in which they are
      SWAP cpvt(pvt%), cpvt(bestcol%)      'used.
   END IF
'Eliminate all values below the pivot
   rp% = rpvt(pvt%)
   cp% = cpvt(pvt%)
   FOR row% = (pvt% + 1) TO up
      r% = rpvt(row%)
      A(r%, cp%) = -A(r%, cp%) / A(rp%, cp%)  'save multipliers
      FOR col% = (pvt% + 1) TO up
         c% = cpvt(col%)                      'complete row operations
         A(r%, c%) = A(r%, c%) + A(r%, cp%) * A(rp%, c%)
      NEXT col%
   NEXT row%
NEXT pvt%
IF A(rpvt(up), cpvt(up)) = 0# THEN
   continue = 0                      'if last pivot is zero or pivot drop is
   ERROR 199                         'too large, A is singular, send back error
ELSEIF (ABS(A(rpvt(up), cpvt(up))) / rownorm(rpvt(up))) < (deps# * oldmax#) THEN
   errcode% = 199                    'if pivot is not identically zero then
END IF                               'continue remains TRUE
IF errcode% THEN ERROR errcode%
dluexit:
matluD% = errcode%
EXIT FUNCTION
dluerr:
   IF errcode% < 199 THEN continue = 0
   errcode% = ERR
   RESUME dluexit
END FUNCTION

'========================matluS%====================================
'matluS% does Gaussian elimination with total pivoting to put a square, single
'precision matrix in LU form. The multipliers used in the row operations to
'create zeroes below the main diagonal are saved in the zero spaces.
'
'Parameters: A(n x n) matrix, rpvt(n) and cpvt(n) permutation vectors
'            used to index the row and column pivots
'
'Returns: A in LU form with corresponding pivot vectors; the total number of
'         pivots in count, which is used to find the sign of the determinant.
'===================================================================
FUNCTION matluS% (A() AS SINGLE)
ON LOCAL ERROR GOTO sluerr: errcode% = 0
'Checks if A is square, returns error code if not
IF NOT (lo = LBOUND(A, 2) AND up = UBOUND(A, 2)) THEN ERROR 198
DIM rownorm(lo TO up) AS SINGLE
count = 0                            'initialize count, continue
continue = -1
FOR row% = lo TO up                  'initialize rpvt and cpvt
   rpvt(row%) = row%
   cpvt(row%) = row%
   rownorm(row%) = 0!                'find the row norms of A()
   FOR col% = lo TO up
      rownorm(row%) = rownorm(row%) + ABS(A(row%, col%))
   NEXT col%
   IF rownorm(row%) = 0! THEN        'if any rownorm is zero, the matrix
      continue = 0                   'is singular, set error, exit and do
      ERROR 199                      'not continue
   END IF
NEXT row%
FOR pvt% = lo TO (up - 1)
'Find best available pivot
   max! = 0!                         'checks all values in rows and columns not
   FOR row% = pvt% TO up             'already used for pivoting and finds the
      r% = rpvt(row%)                'number largest in absolute value relative
      FOR col% = pvt% TO up          'to its row norm
         c% = cpvt(col%)
         temp! = ABS(A(r%, c%)) / rownorm(r%)
         IF temp! > max! THEN
            max! = temp!
            bestrow% = row%          'save the position of new max!
            bestcol% = col%
         END IF
      NEXT col%
   NEXT row%
   IF max! = 0! THEN                 'if no nonzero number is found, A is
      continue = 0                   'singular, send back error, do not continue
      ERROR 199
   ELSEIF pvt% > 1 THEN              'check if drop in pivots is too much
      IF max! < (seps! * oldmax!) THEN errcode% = 199
   END IF
   oldmax! = max!
   IF rpvt(pvt%) <> rpvt(bestrow%) THEN
      count = count + 1                    'if a row or column pivot is
      SWAP rpvt(pvt%), rpvt(bestrow%)      'necessary, count it and permute
   END IF                                  'rpvt or cpvt. Note: the rows and
   IF cpvt(pvt%) <> cpvt(bestcol%) THEN    'columns are not actually switched,
      count = count + 1                    'only the order in which they are
      SWAP cpvt(pvt%), cpvt(bestcol%)      'used.
   END IF
'Eliminate all values below the pivot
   rp% = rpvt(pvt%)
   cp% = cpvt(pvt%)
   FOR row% = (pvt% + 1) TO up
      r% = rpvt(row%)
      A(r%, cp%) = -A(r%, cp%) / A(rp%, cp%)  'save multipliers
      FOR col% = (pvt% + 1) TO up
         c% = cpvt(col%)                      'complete row operations
         A(r%, c%) = A(r%, c%) + A(r%, cp%) * A(rp%, c%)
      NEXT col%
   NEXT row%
NEXT pvt%
IF A(rpvt(up), cpvt(up)) = 0! THEN
   continue = 0                      'if last pivot is zero or pivot drop is
   ERROR 199                         'too large, A is singular, send back error
ELSEIF (ABS(A(rpvt(up), cpvt(up))) / rownorm(rpvt(up))) < (seps! * oldmax!) THEN
   errcode% = 199                    'if pivot is not identically zero then
END IF                               'continue remains TRUE
IF errcode% THEN ERROR errcode%
sluexit:
matluS% = errcode%
EXIT FUNCTION
sluerr:
   errcode% = ERR
   IF errcode% < 199 THEN continue = 0
   RESUME sluexit
END FUNCTION

'=======================MatMultC%===================================
'MatMultC% multiplies two currency type matrices and places the
'product in a result matrix
'
'Parameters: matrices Alpha,Beta,Gamma
'
'Returns: Gamma() = Alpha() * Beta()
'===================================================================
FUNCTION MatMultC% (Alpha() AS CURRENCY, Beta() AS CURRENCY, Gamma() AS CURRENCY)
ON LOCAL ERROR GOTO cmulterr: MatMultC% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
   ERROR 197                   'check inside dimensions
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
   ERROR 195                   'check dimensions of result matrix
END IF
'loop through, Gamma(row,col)=inner product of Alpha(row,*) and Beta(*,col)
FOR row% = LBOUND(Gamma, 1) TO UBOUND(Gamma, 1)
   FOR col% = LBOUND(Gamma, 2) TO UBOUND(Gamma, 2)
      Gamma(row%, col%) = 0@
      FOR inside% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
         Gamma(row%, col%) = Gamma(row%, col%) + Alpha(row%, inside%) * Beta(inside%, col%)
      NEXT inside%
   NEXT col%
NEXT row%
cmultexit:
EXIT FUNCTION
cmulterr:
   MatMultC% = (ERR + 5) MOD 200 - 5
   RESUME cmultexit
END FUNCTION

'=======================MatMultD%===================================
'MatMultD% multiplies two double precision matrices and places the
'product in a result matrix
'
'Parameters: matrices Alpha,Beta,Gamma
'
'Returns: Gamma() = Alpha() * Beta()
'===================================================================
FUNCTION MatMultD% (Alpha() AS DOUBLE, Beta() AS DOUBLE, Gamma() AS DOUBLE)
ON LOCAL ERROR GOTO dmulterr: MatMultD% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
   ERROR 197                   'check inside dimensions
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
   ERROR 195                   'check dimensions of result matrix
END IF
'loop through, Gamma(row,col)=inner product of Alpha(row,*) and Beta(*,col)
FOR row% = LBOUND(Gamma, 1) TO UBOUND(Gamma, 1)
   FOR col% = LBOUND(Gamma, 2) TO UBOUND(Gamma, 2)
      Gamma(row%, col%) = 0#
      FOR inside% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
         Gamma(row%, col%) = Gamma(row%, col%) + Alpha(row%, inside%) * Beta(inside%, col%)
      NEXT inside%
   NEXT col%
NEXT row%
dmultexit:
EXIT FUNCTION
dmulterr:
   MatMultD% = (ERR + 5) MOD 200 - 5
   RESUME dmultexit
END FUNCTION

'=======================MatMultI%===================================
'MatMultI% multiplies two integer matrices and places the product in
'a result matrix
'
'Parameters: matrices Alpha,Beta,Gamma
'
'Returns: Gamma() = Alpha() * Beta()
'===================================================================
FUNCTION MatMultI% (Alpha() AS INTEGER, Beta() AS INTEGER, Gamma() AS INTEGER)
ON LOCAL ERROR GOTO imulterr: MatMultI% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
   ERROR 197                   'check inside dimensions
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
   ERROR 195                   'check dimensions of result matrix
END IF
'loop through, Gamma(row,col)=inner product of Alpha(row,*) and Beta(*,col)
FOR row% = LBOUND(Gamma, 1) TO UBOUND(Gamma, 1)
   FOR col% = LBOUND(Gamma, 2) TO UBOUND(Gamma, 2)
      Gamma(row%, col%) = 0
      FOR inside% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
         Gamma(row%, col%) = Gamma(row%, col%) + Alpha(row%, inside%) * Beta(inside%, col%)
      NEXT inside%
   NEXT col%
NEXT row%
imultexit:
EXIT FUNCTION
imulterr:
   MatMultI% = (ERR + 5) MOD 200 - 5
   RESUME imultexit
END FUNCTION

'=======================MatMultL%===================================
'MatMultL% multiplies two long integer matrices and places the product
'in a result matrix
'
'Parameters: matrices Alpha,Beta,Gamma
'
'Returns: Gamma() = Alpha() * Beta()
'===================================================================
FUNCTION MatMultL% (Alpha() AS LONG, Beta() AS LONG, Gamma() AS LONG)
ON LOCAL ERROR GOTO lmulterr: MatMultL% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
   ERROR 197                   'check inside dimensions
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
   ERROR 195                   'check dimensions of result matrix
END IF
'loop through, Gamma(row,col)=inner product of Alpha(row,*) and Beta(*,col)
FOR row% = LBOUND(Gamma, 1) TO UBOUND(Gamma, 1)
   FOR col% = LBOUND(Gamma, 2) TO UBOUND(Gamma, 2)
      Gamma(row%, col%) = 0&
      FOR inside% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
         Gamma(row%, col%) = Gamma(row%, col%) + Alpha(row%, inside%) * Beta(inside%, col%)
      NEXT inside%
   NEXT col%
NEXT row%
lmultexit:
EXIT FUNCTION
lmulterr:
   MatMultL% = (ERR + 5) MOD 200 - 5
   RESUME lmultexit
END FUNCTION

'=======================MatMultS%===================================
'MatMultS% multiplies two single precision matrices and places the
'product in a result matrix
'
'Parameters: matrices Alpha,Beta,Gamma
'
'Returns: Gamma() = Alpha() * Beta()
'===================================================================
FUNCTION MatMultS% (Alpha() AS SINGLE, Beta() AS SINGLE, Gamma() AS SINGLE)
ON LOCAL ERROR GOTO smulterr: MatMultS% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
   ERROR 197                   'check inside dimensions
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
   ERROR 195                   'check dimensions of result matrix
END IF
'loop through, Gamma(row,col)=inner product of Alpha(row,*) and Beta(*,col)
FOR row% = LBOUND(Gamma, 1) TO UBOUND(Gamma, 1)
   FOR col% = LBOUND(Gamma, 2) TO UBOUND(Gamma, 2)
      Gamma(row%, col%) = 0!
      FOR inside% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
         Gamma(row%, col%) = Gamma(row%, col%) + Alpha(row%, inside%) * Beta(inside%, col%)
      NEXT inside%
   NEXT col%
NEXT row%
smultexit:
EXIT FUNCTION
smulterr:
   MatMultS% = (ERR + 5) MOD 200 - 5
   RESUME smultexit
END FUNCTION

'========================MatSEqnC%==================================
'MatSEqnC% solves a system of n linear equations, Ax=b, and puts the
'answer in b. A is first put in LU form by matluC%, then matbsC is called
'to solve the system.  matrices A,b are currency type.
'
'Parameters: A(n x n) contains coefficient matrix, b(N) contains the right side
'
'Returns: A in LU form, solution in b
'===================================================================
FUNCTION MatSEqnC% (A() AS CURRENCY, b() AS CURRENCY)
ON LOCAL ERROR GOTO cseqnerr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
'duplicate A(), b() in temporary double precision matrices Tmp(), btmp()
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS DOUBLE
DIM x(lo TO up) AS DOUBLE, btmp(lo TO up) AS DOUBLE
FOR row% = lo TO up
   FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
      Tmp(row%, col%) = CDBL(A(row%, col%))
   NEXT col%
NEXT row%
errcode% = matluD%(Tmp())                   'Get LU matrix
IF NOT continue THEN ERROR errcode%
'check dimensions of b, make double precision copy if ok.
IF (lo <> LBOUND(b)) OR (up <> UBOUND(b)) THEN ERROR 197
FOR row% = lo TO up
   btmp(row%) = CDBL(b(row%))
NEXT row%
bserrcode% = matbsD%(Tmp(), btmp(), x())    'Backsolve system
IF bserrcode% THEN ERROR bserrcode%
FOR row% = lo TO up
   b(row%) = CCUR(x(row%))                  'Put solution in b for return
NEXT row%
IF errcode% THEN ERROR errcode%
cseqnexit:
ERASE Tmp, btmp, x, rpvt, cpvt
MatSEqnC% = errcode%
EXIT FUNCTION
cseqnerr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME cseqnexit
END FUNCTION

'========================MatSEqnD%==================================
'MatSEqnD% solves a system of n linear equations, Ax=b, and puts the
'answer in b. A is first put in LU form by matluD%, then matbsD is called
'to solve the system.  matrices A,b are double precision.
'
'Parameters: A(n x n) contains coefficient matrix, b(N) contains the right side
'
'Returns: A in LU form, solution in b
'===================================================================
FUNCTION MatSEqnD% (A() AS DOUBLE, b() AS DOUBLE)
ON LOCAL ERROR GOTO dseqnerr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM x(lo TO up) AS DOUBLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())                      'Get LU matrix
IF NOT continue THEN ERROR errcode%
'check dimensions of b
IF (lo <> LBOUND(b)) OR (up <> UBOUND(b)) THEN ERROR 197
bserrcode% = matbsD%(A(), b(), x())          'Backsolve system
IF bserrcode% THEN ERROR bserrcode%
FOR row% = lo TO up
   b(row%) = x(row%)                         'Put solution in b for return
NEXT row%
IF errcode% THEN ERROR errcode%
dseqnexit:
ERASE x, rpvt, cpvt
MatSEqnD% = errcode%
EXIT FUNCTION
dseqnerr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME dseqnexit
END FUNCTION

'========================MatSEqnS%==================================
'MatSEqnS% solves a system of n linear equations, Ax=b, and puts the
'answer in b. A is first put in LU form by matluS%, then matbsS is called
'to solve the system.  matrices A,b are single precision.
'
'Parameters: A(n x n) contains coefficient matrix, b(N) contains the right side
'
'Returns: A in LU form, solution in b
'===================================================================
FUNCTION MatSEqnS% (A() AS SINGLE, b() AS SINGLE)
ON LOCAL ERROR GOTO sseqnerr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM x(lo TO up) AS SINGLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())                      'Get LU matrix
IF NOT continue THEN ERROR errcode%
'check dimensions of b
IF (lo <> LBOUND(b)) OR (up <> UBOUND(b)) THEN ERROR 197
bserrcode% = matbsS%(A(), b(), x())          'Backsolve system
IF bserrcode% THEN ERROR bserrcode%
FOR row% = lo TO up
   b(row%) = x(row%)                         'Put solution in b for return
NEXT row%
IF errcode% THEN ERROR errcode%
sseqnexit:
ERASE x, rpvt, cpvt
MatSEqnS% = errcode%
EXIT FUNCTION
sseqnerr:
   errcode% = (ERR + 5) MOD 200 - 5
   RESUME sseqnexit
END FUNCTION

'=======================MatSubC%====================================
'MatSubC% takes the difference of two currency type matrices and
'places the result in the first.
'
'Params: matrices Alpha,Beta
'
'Returns: Alpha=Alpha-Beta
'===================================================================
FUNCTION MatSubC% (Alpha() AS CURRENCY, Beta() AS CURRENCY)
ON LOCAL ERROR GOTO csuberr: MatSubC% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and subtract elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) - Beta(row%, col%)
   NEXT col%
NEXT row%
csubexit:
EXIT FUNCTION
csuberr:
   MatSubC% = (ERR + 5) MOD 200 - 5
   RESUME csubexit:
END FUNCTION

'=======================MatSubD%====================================
'MatSubD% takes the difference of two double precision matrices and
'places the result in the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() - Beta()
'===================================================================
FUNCTION MatSubD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
ON LOCAL ERROR GOTO dsuberr: MatSubD% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and subtract elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) - Beta(row%, col%)
   NEXT col%
NEXT row%
dsubexit:
EXIT FUNCTION
dsuberr:
   MatSubD% = (ERR + 5) MOD 200 - 5
   RESUME dsubexit:
END FUNCTION

'=======================MatSubI%====================================
'MatSubI% takes the difference of two integer matrices and places the
'result in the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() - Beta()
'===================================================================
FUNCTION MatSubI% (Alpha() AS INTEGER, Beta() AS INTEGER)
ON LOCAL ERROR GOTO isuberr: MatSubI% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and subtract elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) - Beta(row%, col%)
   NEXT col%
NEXT row%
isubexit:
EXIT FUNCTION
isuberr:
   MatSubI% = (ERR + 5) MOD 200 - 5
   RESUME isubexit:
END FUNCTION

'=======================MatSubL%====================================
'MatSubL% takes the difference of two long integer matrices and places
'the result in the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() - Beta()
'===================================================================
FUNCTION MatSubL% (Alpha() AS LONG, Beta() AS LONG)
ON LOCAL ERROR GOTO lsuberr: MatSubL% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and subtract elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) - Beta(row%, col%)
   NEXT col%
NEXT row%
lsubexit:
EXIT FUNCTION
lsuberr:
   MatSubL% = (ERR + 5) MOD 200 - 5
   RESUME lsubexit:
END FUNCTION

'=======================MatSubS%====================================
'MatSubS% takes the difference of two single precision matrices and
'places the result in the first.
'
'Parameters: matrices Alpha,Beta
'
'Returns: Alpha() = Alpha() - Beta()
'===================================================================
FUNCTION MatSubS% (Alpha() AS SINGLE, Beta() AS SINGLE)
ON LOCAL ERROR GOTO ssuberr: MatSubS% = 0
'check if Alpha, Beta have same dimensions if not, exit and send back error
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
'loop through and subtract elements
FOR row% = LBOUND(Alpha, 1) TO UBOUND(Alpha, 1)
   FOR col% = LBOUND(Alpha, 2) TO UBOUND(Alpha, 2)
      Alpha(row%, col%) = Alpha(row%, col%) - Beta(row%, col%)
   NEXT col%
NEXT row%
ssubexit:
EXIT FUNCTION
ssuberr:
   MatSubS% = (ERR + 5) MOD 200 - 5
   RESUME ssubexit:
END FUNCTION

