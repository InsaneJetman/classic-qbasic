'============================MATB.BI================================
'If you make calls to functions in the matrix math toolbox, you must
'include this file in your module-level code with an $INCLUDE metacommand.
'==========Function Declarations for Matrix Math Toolbox============

DECLARE FUNCTION MatAddC% (Alpha() AS CURRENCY, Beta() AS CURRENCY)
DECLARE FUNCTION MatAddD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
DECLARE FUNCTION MatAddI% (Alpha() AS INTEGER, Beta() AS INTEGER)
DECLARE FUNCTION MatAddL% (Alpha() AS LONG, Beta() AS LONG)
DECLARE FUNCTION MatAddS% (Alpha() AS SINGLE, Beta() AS SINGLE)
DECLARE FUNCTION MatDetC% (A() AS CURRENCY, det@)
DECLARE FUNCTION MatDetD% (A() AS DOUBLE, det#)
DECLARE FUNCTION MatDetI% (A() AS INTEGER, det%)
DECLARE FUNCTION MatDetL% (A() AS LONG, det&)
DECLARE FUNCTION MatDetS% (A() AS SINGLE, det!)
DECLARE FUNCTION MatInvC% (A() AS CURRENCY)
DECLARE FUNCTION MatInvD% (A() AS DOUBLE)
DECLARE FUNCTION MatInvS% (A() AS SINGLE)
DECLARE FUNCTION MatMultC% (Alpha() AS CURRENCY, Beta() AS CURRENCY, Gamma() AS CURRENCY)
DECLARE FUNCTION MatMultD% (Alpha() AS DOUBLE, Beta() AS DOUBLE, Gamma() AS DOUBLE)
DECLARE FUNCTION MatMultI% (Alpha() AS INTEGER, Beta() AS INTEGER, Gamma() AS INTEGER)
DECLARE FUNCTION MatMultL% (Alpha() AS LONG, Beta() AS LONG, Gamma() AS LONG)
DECLARE FUNCTION MatMultS% (Alpha() AS SINGLE, Beta() AS SINGLE, Gamma() AS SINGLE)
DECLARE FUNCTION MatSEqnC% (A() AS CURRENCY, b() AS CURRENCY)
DECLARE FUNCTION MatSEqnD% (A() AS DOUBLE, b() AS DOUBLE)
DECLARE FUNCTION MatSEqnS% (A() AS SINGLE, b() AS SINGLE)
DECLARE FUNCTION MatSubC% (Alpha() AS CURRENCY, Beta() AS CURRENCY)
DECLARE FUNCTION MatSubD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
DECLARE FUNCTION MatSubI% (Alpha() AS INTEGER, Beta() AS INTEGER)
DECLARE FUNCTION MatSubL% (Alpha() AS LONG, Beta() AS LONG)
DECLARE FUNCTION MatSubS% (Alpha() AS SINGLE, Beta() AS SINGLE)

'===================================================================
'In theory, a matrix can be exactly singular.  Numerically, exact singularity
'is a rare occurrence because round off error turns exact zeroes into very
'small numbers. This data corruption makes it necessary to set a criterion to
'determine how small a number has to be before we flag it as zero and call
'the matrix singular.
'
'The following constants set the maximum drop allowed in weighted pivot
'values without error code -1 (matrix singular) being returned.  To increase
'the singularity sensitivity of the MatDet, MatInv, and MatSEqn routines;
'increase the size of seps! for single precision calls or deps# for double
'precision calls.  Similarly, decreasing the size of the constants will make
'the routines less sensitive to singularity.
'
'If the matrix toolbox is in a library (setup builds MATB???.LIB) or quick
'library (MATBEFR.QLB), these files must be rebuilt using the new constants.
'To do this, compile MATB.BAS (use /o,/e) and replace MATB???.OBJ in the
'library with the new .OBJ file.
'===================================================================

CONST seps! = .00001
CONST deps# = .00000000001#
