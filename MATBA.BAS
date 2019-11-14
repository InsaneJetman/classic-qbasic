'*** MATBA.BAS - Matrix Math Routines for the Alt-Math Matrix Math Toolbox in
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
'  Purpose:  These are the same matrix math routines found in MATB.BAS
'  with the exclusion of currency datatype routines, to allow the routines
'  to be compiled with the BC /FPa alternate math pack option.
'
'**************************************************************************
'
'$INCLUDE: 'matba.bi'
DECLARE FUNCTION matbsD% (A() AS DOUBLE, b() AS DOUBLE, x() AS DOUBLE)
DECLARE FUNCTION matbsS% (A() AS SINGLE, b() AS SINGLE, x() AS SINGLE)
DECLARE FUNCTION matluD% (A() AS DOUBLE)
DECLARE FUNCTION matluS% (A() AS SINGLE)
DIM SHARED lo AS INTEGER, up AS INTEGER
DIM SHARED continue AS INTEGER, count AS INTEGER
DIM SHARED rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
END

FUNCTION MatAddD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
ON LOCAL ERROR GOTO dadderr: MatAddD% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatAddI% (Alpha() AS INTEGER, Beta() AS INTEGER)
ON LOCAL ERROR GOTO iadderr: MatAddI% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatAddL% (Alpha() AS LONG, Beta() AS LONG)
ON LOCAL ERROR GOTO ladderr: MatAddL% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatAddS% (Alpha() AS SINGLE, Beta() AS SINGLE)
ON LOCAL ERROR GOTO sadderr: MatAddS% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION matbsD% (A() AS DOUBLE, b() AS DOUBLE, x() AS DOUBLE)
ON LOCAL ERROR GOTO dbserr: matbsD% = 0
FOR pvt% = lo TO (up - 1)
        c% = cpvt(pvt%)
        FOR row% = (pvt% + 1) TO up
                r% = rpvt(row%)
                b(r%) = b(r%) + A(r%, c%) * b(rpvt(pvt%))
        NEXT row%
NEXT pvt%
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

FUNCTION matbsS% (A() AS SINGLE, b() AS SINGLE, x() AS SINGLE)
ON LOCAL ERROR GOTO sbserr: matbsS% = 0
FOR pvt% = lo TO (up - 1)
        c% = cpvt(pvt%)
        FOR row% = (pvt% + 1) TO up
                r% = rpvt(row%)
                b(r%) = b(r%) + A(r%, c%) * b(rpvt(pvt%))
        NEXT row%
NEXT pvt%
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

FUNCTION MatDetD% (A() AS DOUBLE, det#)
ON LOCAL ERROR GOTO ddeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())
IF NOT continue THEN
   IF errcode% = 199 THEN det# = 0#
   ERROR errcode%
ELSE
   det# = 1#
   FOR pvt% = lo TO up
        det# = det# * A(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%
   det# = (-1) ^ count * det#
   IF errcode% THEN ERROR errcode%
END IF
ddetexit:             
ERASE rpvt, cpvt
MatDetD% = errcode%
EXIT FUNCTION
ddeterr:
        errcode% = (ERR + 5) MOD 200 - 5
        RESUME ddetexit
END FUNCTION

FUNCTION MatDetI% (A() AS INTEGER, det%)
ON LOCAL ERROR GOTO ideterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS SINGLE
FOR row% = lo TO up
        FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
                Tmp(row%, col%) = CSNG(A(row%, col%))
        NEXT col%
NEXT row%
errcode% = matluS%(Tmp())
IF NOT continue THEN
   IF errcode% = 199 THEN det% = 0
   ERROR errcode%
ELSE
   detS! = 1!
   FOR pvt% = lo TO up
        detS! = detS! * Tmp(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%
   det% = (-1) ^ count * CINT(detS!)
   IF errcode% THEN ERROR errcode%
END IF
idetexit:
ERASE rpvt, cpvt, Tmp
MatDetI% = errcode%
EXIT FUNCTION
ideterr:
        errcode% = (ERR + 5) MOD 200 - 5
        RESUME idetexit
END FUNCTION

FUNCTION MatDetL% (A() AS LONG, det&)
ON LOCAL ERROR GOTO ldeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
DIM Tmp(lo TO up, LBOUND(A, 2) TO UBOUND(A, 2)) AS DOUBLE
FOR row% = lo TO up
        FOR col% = LBOUND(A, 2) TO UBOUND(A, 2)
                Tmp(row%, col%) = CDBL(A(row%, col%))
        NEXT col%
NEXT row%
errcode% = matluD%(Tmp())
IF NOT continue THEN
   IF errcode% = 199 THEN det& = 0&
   ERROR errcode%
ELSE
   detD# = 1#
   FOR pvt% = lo TO up
        detD# = detD# * Tmp(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%
   det& = (-1&) ^ count * CLNG(detD#)
   IF errcode% THEN ERROR errcode%
END IF
ldetexit:
ERASE rpvt, cpvt, Tmp
MatDetL% = errcode%
EXIT FUNCTION
ldeterr:
        errcode% = (ERR + 5) MOD 200 - 5
        RESUME ldetexit
END FUNCTION

FUNCTION MatDetS% (A() AS SINGLE, det!)
ON LOCAL ERROR GOTO sdeterr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())
IF NOT continue THEN
   IF errcode% = 199 THEN det! = 0!
   ERROR errcode%
ELSE
   det! = 1!
   FOR pvt% = lo TO up
        det! = det! * A(rpvt(pvt%), cpvt(pvt%))
   NEXT pvt%
   det! = (-1) ^ count * det!
   IF errcode% THEN ERROR errcode%
END IF
sdetexit:
ERASE rpvt, cpvt
MatDetS% = errcode%
EXIT FUNCTION
sdeterr:
        errcode% = (ERR + 5) MOD 200 - 5
        RESUME sdetexit
END FUNCTION

FUNCTION MatInvD% (A() AS DOUBLE)
ON LOCAL ERROR GOTO dinverr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM Ain(lo TO up, lo TO up) AS DOUBLE
DIM e(lo TO up) AS DOUBLE, x(lo TO up) AS DOUBLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())
IF NOT continue THEN ERROR errcode%
FOR col% = lo TO up
        e(col%) = 1#
        bserrcode% = matbsD%(A(), e(), x())
        IF bserrcode% THEN ERROR bserrcode%
        FOR row% = lo TO up
                Ain(row%, col%) = x(row%)
                e(row%) = 0#
        NEXT row%
NEXT col%
FOR col% = lo TO up
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

FUNCTION MatInvS% (A() AS SINGLE)
ON LOCAL ERROR GOTO sinverr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM Ain(lo TO up, lo TO up) AS SINGLE
DIM e(lo TO up) AS SINGLE, x(lo TO up) AS SINGLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())
IF NOT continue THEN ERROR errcode%
FOR col% = lo TO up
        e(col%) = 1!
        bserrcode% = matbsS%(A(), e(), x())
        IF bserrcode% THEN ERROR bserrcode%
        FOR row% = lo TO up
                Ain(row%, col%) = x(row%)
                e(row%) = 0!
        NEXT row%
NEXT col%
FOR col% = lo TO up
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

FUNCTION matluD% (A() AS DOUBLE)
ON LOCAL ERROR GOTO dluerr: errcode% = 0
IF NOT (lo = LBOUND(A, 2) AND up = UBOUND(A, 2)) THEN ERROR 198
DIM rownorm(lo TO up) AS DOUBLE
count = 0
continue = -1
FOR row% = lo TO up
        rpvt(row%) = row%
        cpvt(row%) = row%
        rownorm(row%) = 0#
        FOR col% = lo TO up
                rownorm(row%) = rownorm(row%) + ABS(A(row%, col%))
        NEXT col%
        IF rownorm(row%) = 0# THEN
                continue = 0
                ERROR 199
        END IF
NEXT row%
FOR pvt% = lo TO (up - 1)
        max# = 0#
        FOR row% = pvt% TO up
                r% = rpvt(row%)
                FOR col% = pvt% TO up
                        c% = cpvt(col%)
                        temp# = ABS(A(r%, c%)) / rownorm(r%)
                        IF temp# > max# THEN
                                max# = temp#
                                bestrow% = row%
                                bestcol% = col%
                        END IF
                NEXT col%
        NEXT row%
        IF max# = 0# THEN
                continue = 0
                ERROR 199
        ELSEIF pvt% > 1 THEN
                IF max# < (deps# * oldmax#) THEN errcode% = 199
        END IF
        oldmax# = max#
        IF rpvt(pvt%) <> rpvt(bestrow%) THEN
                count = count + 1
                SWAP rpvt(pvt%), rpvt(bestrow%)
        END IF
        IF cpvt(pvt%) <> cpvt(bestcol%) THEN
                count = count + 1
                SWAP cpvt(pvt%), cpvt(bestcol%)
        END IF
        rp% = rpvt(pvt%)
        cp% = cpvt(pvt%)
        FOR row% = (pvt% + 1) TO up
                r% = rpvt(row%)
                A(r%, cp%) = -A(r%, cp%) / A(rp%, cp%)
                FOR col% = (pvt% + 1) TO up
                        c% = cpvt(col%)
                        A(r%, c%) = A(r%, c%) + A(r%, cp%) * A(rp%, c%)
                NEXT col%
        NEXT row%
NEXT pvt%
IF A(rpvt(up), cpvt(up)) = 0# THEN
        continue = 0
        ERROR 199
ELSEIF (ABS(A(rpvt(up), cpvt(up))) / rownorm(rpvt(up))) < (deps# * oldmax#) THEN
        errcode% = 199
END IF
IF errcode% THEN ERROR errcode%
dluexit:
matluD% = errcode%
EXIT FUNCTION
dluerr:
        IF errcode% < 199 THEN continue = 0
        errcode% = ERR
        RESUME dluexit
END FUNCTION

FUNCTION matluS% (A() AS SINGLE)
ON LOCAL ERROR GOTO sluerr: errcode% = 0
IF NOT (lo = LBOUND(A, 2) AND up = UBOUND(A, 2)) THEN ERROR 198
DIM rownorm(lo TO up) AS SINGLE
count = 0
continue = -1
FOR row% = lo TO up
        rpvt(row%) = row%
        cpvt(row%) = row%
        rownorm(row%) = 0!
        FOR col% = lo TO up
                rownorm(row%) = rownorm(row%) + ABS(A(row%, col%))
        NEXT col%
        IF rownorm(row%) = 0! THEN
                continue = 0
                ERROR 199
        END IF
NEXT row%
FOR pvt% = lo TO (up - 1)
        max! = 0!
        FOR row% = pvt% TO up
                r% = rpvt(row%)
                FOR col% = pvt% TO up
                        c% = cpvt(col%)
                        temp! = ABS(A(r%, c%)) / rownorm(r%)
                        IF temp! > max! THEN
                                max! = temp!
                                bestrow% = row%
                                bestcol% = col%
                        END IF
                NEXT col%
        NEXT row%
        IF max! = 0! THEN
                continue = 0
                ERROR 199
        ELSEIF pvt% > 1 THEN
                IF max! < (seps! * oldmax!) THEN errcode% = 199
        END IF
        oldmax! = max!
        IF rpvt(pvt%) <> rpvt(bestrow%) THEN
                count = count + 1
                SWAP rpvt(pvt%), rpvt(bestrow%)
        END IF
        IF cpvt(pvt%) <> cpvt(bestcol%) THEN
                count = count + 1
                SWAP cpvt(pvt%), cpvt(bestcol%)
        END IF
        rp% = rpvt(pvt%)
        cp% = cpvt(pvt%)
        FOR row% = (pvt% + 1) TO up
                r% = rpvt(row%)
                A(r%, cp%) = -A(r%, cp%) / A(rp%, cp%)
                FOR col% = (pvt% + 1) TO up
                        c% = cpvt(col%)
                        A(r%, c%) = A(r%, c%) + A(r%, cp%) * A(rp%, c%)
                NEXT col%
        NEXT row%
NEXT pvt%
IF A(rpvt(up), cpvt(up)) = 0! THEN
        continue = 0
        ERROR 199
ELSEIF (ABS(A(rpvt(up), cpvt(up))) / rownorm(rpvt(up))) < (seps! * oldmax!) THEN
        errcode% = 199
END IF
IF errcode% THEN ERROR errcode%
sluexit:
matluS% = errcode%
EXIT FUNCTION
sluerr:
        errcode% = ERR
        IF errcode% < 199 THEN continue = 0
        RESUME sluexit
END FUNCTION

FUNCTION MatMultD% (Alpha() AS DOUBLE, Beta() AS DOUBLE, Gamma() AS DOUBLE)
ON LOCAL ERROR GOTO dmulterr: MatMultD% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
        ERROR 197
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
        ERROR 195
END IF
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

FUNCTION MatMultI% (Alpha() AS INTEGER, Beta() AS INTEGER, Gamma() AS INTEGER)
ON LOCAL ERROR GOTO imulterr: MatMultI% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
        ERROR 197
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
        ERROR 195
END IF
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

FUNCTION MatMultL% (Alpha() AS LONG, Beta() AS LONG, Gamma() AS LONG)
ON LOCAL ERROR GOTO lmulterr: MatMultL% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
        ERROR 197
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
        ERROR 195
END IF
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

FUNCTION MatMultS% (Alpha() AS SINGLE, Beta() AS SINGLE, Gamma() AS SINGLE)
ON LOCAL ERROR GOTO smulterr: MatMultS% = 0
IF (LBOUND(Alpha, 2) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 1)) THEN
        ERROR 197
ELSEIF (LBOUND(Alpha, 1) <> LBOUND(Gamma, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Gamma, 1)) OR (LBOUND(Beta, 2) <> LBOUND(Gamma, 2)) OR (UBOUND(Beta, 2) <> UBOUND(Gamma, 2)) THEN
        ERROR 195
END IF
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

FUNCTION MatSEqnD% (A() AS DOUBLE, b() AS DOUBLE)
ON LOCAL ERROR GOTO dseqnerr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM x(lo TO up) AS DOUBLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluD%(A())
IF NOT continue THEN ERROR errcode%
IF (lo <> LBOUND(b)) OR (up <> UBOUND(b)) THEN ERROR 197
bserrcode% = matbsD%(A(), b(), x())
IF bserrcode% THEN ERROR bserrcode%
FOR row% = lo TO up
        b(row%) = x(row%)
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

FUNCTION MatSEqnS% (A() AS SINGLE, b() AS SINGLE)
ON LOCAL ERROR GOTO sseqnerr: errcode% = 0
lo = LBOUND(A, 1)
up = UBOUND(A, 1)
DIM x(lo TO up) AS SINGLE
REDIM rpvt(lo TO up) AS INTEGER, cpvt(lo TO up) AS INTEGER
errcode% = matluS%(A())
IF NOT continue THEN ERROR errcode%
IF (lo <> LBOUND(b)) OR (up <> UBOUND(b)) THEN ERROR 197
bserrcode% = matbsS%(A(), b(), x())
IF bserrcode% THEN ERROR bserrcode%
FOR row% = lo TO up
        b(row%) = x(row%)
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

FUNCTION MatSubD% (Alpha() AS DOUBLE, Beta() AS DOUBLE)
ON LOCAL ERROR GOTO dsuberr: MatSubD% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatSubI% (Alpha() AS INTEGER, Beta() AS INTEGER)
ON LOCAL ERROR GOTO isuberr: MatSubI% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatSubL% (Alpha() AS LONG, Beta() AS LONG)
ON LOCAL ERROR GOTO lsuberr: MatSubL% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

FUNCTION MatSubS% (Alpha() AS SINGLE, Beta() AS SINGLE)
ON LOCAL ERROR GOTO ssuberr: MatSubS% = 0
IF (LBOUND(Alpha, 1) <> LBOUND(Beta, 1)) OR (UBOUND(Alpha, 1) <> UBOUND(Beta, 1)) OR (LBOUND(Alpha, 2) <> LBOUND(Beta, 2)) OR (UBOUND(Alpha, 2) <> UBOUND(Beta, 2)) THEN ERROR 196
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

