subroutine mechanism_rates(p,t,y,mnsp)
    USE photolysisRates
    USE zenithData1
    USE constraints
	use envVars, only: ro2

    implicit none

    ! calculates rate constants from arrhenius informtion
    double precision, intent(out) :: p(*)
    double precision, intent(in) :: t
    integer, intent(in) :: mnsp
    double precision, intent(in) :: y(mnsp)
    double precision:: kro2no3, temp
    double precision:: m, o2, n2, h2o, k0, ki, fc, f, k1, k2, k3, k4, kmt01, kmt02
    double precision:: kmt03, kmt04, kmt05, kmt06, kmt07, kmt08, kmt09,kmt10,kmt11
    double precision:: kmt12, kmt13, kmt14, kmt15, kmt16, kmt17, kfpan, kbpan
    double precision:: fcc,krc,fcd,krd,fc2,fc1,fc3,fc4,kr1,kr2,kr3,kr4
    double precision:: fc7,fc8,fc9,fc10,fc13,fc14,kr7,kr8,kr9,kr10,kr13,kr14
    double precision:: kc0, kci,kd0,kdi,fd,k10,k1i,f1,k20,k2i,k30,k3i,f3
    double precision:: k40,k4i,k70,k7i,f7,k80,k8i,f8
    double precision:: k90,k9i,f9,k100,k10i,f10,k130,k13i,f13,k140,k14i,f14
    double precision:: k160,k16i,kr16,fc16,f16
    double precision:: RH,dilute,jfac,roofOpen

    ! delcare variables missed in MCM definition
    double precision:: kroprim,krosec,kdec,kno3al,kapno,kapho2, K298CH3O2, KCH3O2
    double precision :: kro2ho2,kro2no, fa2,fa4
    integer :: i
    double precision :: dec
    double precision::  photoRateAtT
    double precision:: blh, pressure, dummy
	
	include 'modelConfiguration/mechanism-rate-declarations.f90'
	
    call ro2sum(ro2, y)
    dummy = y(1)

    dec = -1e16

    call getEnvVarsAtT(t,temp,rh,h2o,dec,pressure,m,blh,dilute,jfac,roofOpen)

	call atmosphere(O2, N2,m)
   



    !O2 = 0.2095*m
    !N2 = 0.7809*m



    ! * **** SIMPLE RATE COEFFICIENTS *****                     *

    ! * KRO2NO : RO2      + NO      = RO      + NO2
    ! *        : RO2      + NO      = RONO2
    ! * MCM PROTOCOL 2001
    KRO2NO    = 2.54D-12*EXP(360/TEMP)

    ! * KRO2HO2: RO2      + HO2     = ROOH    + O2
    ! * MCM PROTOCOL 2001
    KRO2HO2   = 2.91D-13*EXP(1300/TEMP)
    ! *
    ! * KAPHO2 : RCOO2    + HO2     = PRODUCTS
    ! * MCM PROTOCOL 2001
    KAPHO2    = 4.30D-13*EXP(1040/TEMP)
    ! *
    ! * KAPNO  : RCOO2    + NO      = PRODUCTS
    ! * MCM PROTOCOL 2001
    KAPNO = 8.10D-12*EXP(270/TEMP)
    ! *
    ! * KRO2NO3: RO2      + NO3     = PRODUCTS
    ! * MCM PROTOCOL 1997
    KRO2NO3   = 2.50D-12
    ! *
    ! * KNO3AL : NO3      + RCHO    = RCOO2   + HNO3
    ! * MCM PROTOCOL   1997
    KNO3AL    = 1.44D-12*EXP(-1862/TEMP)
    ! *
    ! * KDEC   : RO                 = PRODUCTS
    ! * MCM PROTOCOL 1997
    KDEC      = 1.00D+06
    !
    ! * KROPRIM : RO(primary)       = PRODUCTS
    ! * MCM PROTOCOL 2001
    KROPRIM   = 3.70D-14*EXP(-460/TEMP)
    ! *
    ! * KROSEC  : RO(secondary)     = PRODUCTS
    ! * MCM PROTOCOL 2001
    KROSEC    = 1.80D-14*EXP(-260/TEMP)
    !
    ! * *** COMPLEX RATE COEFFICIENTS *****                     *
    !
    ! * KFPAN KBPAN
    ! * Formation and decomposition of PAN
    ! * IUPAC 2001
    KC0     = 2.70D-28*M*(TEMP/300)**(-7.1)
    KCI     = 1.20D-11*(TEMP/300)**(-0.9)
    KRC     = KC0/KCI
    FCC     = 0.30
    FC      = 10**(LOG10(FCC)/(1+(LOG10(KRC))**2))
    KFPAN   = (KC0*KCI)*FC/(KC0+KCI)
    ! *
    KD0     = 4.90D-03*M*EXP(-12100/TEMP)
    KDI     = 5.40D+16*EXP(-13830/TEMP)
    KRD     = KD0/KDI
    FCD     = 0.30
    FD      = 10**(LOG10(FCD)/(1+(LOG10(KRD))**2))
    KBPAN   = (KD0*KDI)*FD/(KD0+KDI)
    ! *
    ! * KMT01  : O        + NO      = NO2
    ! * IUPAC 2001
    K10     = 1.00D-31*M*(TEMP/300)**(-1.6 )
    K1I     = 3.00D-11*(TEMP/300)**0.3
    KR1     = K10/K1I
    FC1     = 0.85
    F1      = 10**(LOG10(FC1)/(1+(LOG10(KR1))**2))
    KMT01   = (K10*K1I)*F1/(K10+K1I)
    ! *
    ! * KMT02  : O        + NO2     = NO3
    ! * IUPAC 2001
    K20     = 1.30D-31*M*(TEMP/300)**(-1.5)
    K2I     = 2.30D-11*(TEMP/200)**0.24
    KR2     = K20/K2I
    FC2     = 0.6
    Fa2      = 10**(LOG10(FC2)/(1+(LOG10(KR2))**2))
    KMT02   = (K20*K2I)*Fa2/(K20+K2I)
    ! *
    ! * KMT03  : NO2      + NO3     = N2O5
    ! * IUPAC 2001
    K30     = 3.60D-30*M*(TEMP/300)**(-4.1 )
    K3I     = 1.90D-12*(TEMP/300)**0.2
    KR3     = K30/K3I
    FC3     = 0.35
    F3      = 10**(LOG10(FC3)/(1+(LOG10(KR3))**2))
    KMT03   = (K30*K3I)*F3/(K30+K3I)
    ! *
    ! * KMT04  : N2O5               = NO2     + NO3
    ! * IUPAC 1997/2001
    K40     = 1.00D-03*M*(TEMP/300)**(-3.5)*EXP(-11000/TEMP)
    K4I     = 9.70D+14*(TEMP/300)**0.1*EXP(-11080/TEMP)
    KR4     = K40/K4I
    FC4     = 0.35
    Fa4      = 10**(LOG10(FC4)/(1+(LOG10(KR4))**2))
    KMT04   = (K40*K4I)*Fa4/(K40+K4I)
    ! *
    ! * KMT05  : OH       + CO(+O2) = HO2     + CO2
    ! * IUPAC 2001
    KMT05  = 1 + ((0.6*M)/(2.652D+19*(300/TEMP)))
    ! *
    ! * KMT06  : HO2      + HO2     = H2O2    + O2
    ! * Water Enhancement Factor
    ! * IUPAC 1992/2001
    KMT06  = 1 + (1.40D-21*EXP(2200/TEMP)*H2O)
    ! *
    ! * KMT07  : OH       + NO      = HONO
    ! * IUPAC 2001
    K70     = 7.40D-31*M*(TEMP/300)**(-2.4 )
    K7I     = 3.30D-11*(TEMP/300)**(-0.3    )
    KR7     = K70/K7I
    FC7     = EXP(-TEMP/1420)
    F7      = 10**(LOG10(FC7)/(1+(LOG10(KR7))**2))
    KMT07   = (K70*K7I)*F7/(K70+K7I)
    ! *
    ! * KMT08  : OH       + NO2     = HNO3
    ! * IUPAC 2002
    K80     = 3.30D-30*M*(TEMP/300)**(-3.0)
    K8I     = 4.10D-11
    KR8     = K80/K8I
    FC8     = 0.4
    F8      = 10**(LOG10(FC8)/(1+(LOG10(KR8))**2))
    KMT08   = (K80*K8I)*F8/(K80+K8I)
    ! *
    ! * KMT09  : HO2      + NO2     = HO2NO2
    ! * IUPAC 1997/2001
    K90     = 1.80D-31*M*(TEMP/300)**(-3.2 )
    K9I     = 4.70D-12
    KR9     = K90/K9I
    FC9     = 0.6
    F9      = 10**(LOG10(FC9)/(1+(LOG10(KR9))**2))
    KMT09   = (K90*K9I)*F9/(K90+K9I)
    ! *
    ! * KMT10  : HO2NO2             = HO2     + NO2
    ! * IUPAC 2001
    K100     = 4.10D-05*M*EXP(-10650/TEMP)
    K10I     = 4.80D+15*EXP(-11170/TEMP)
    KR10     = K100/K10I
    FC10     = 0.6
    F10      = 10**(LOG10(FC10)/(1+(LOG10(KR10))**2))
    KMT10    = (K100*K10I)*F10/(K100+K10I)
    ! *
    ! * KMT11  : OH       + HNO3    = H2O     + NO3
    ! * IUPAC 2001
    K1     = 2.40D-14*EXP(460/TEMP)
    K3     = 6.50D-34*EXP(1335/TEMP)
    K4     = 2.70D-17*EXP(2199/TEMP)
    K2     = (K3*M)/(1+(K3*M/K4))
    KMT11  = K1 + K2
    ! *                                                         *
    ! * KMT12 : OH    +   SO2  =  HSO3
    ! * IUPAC 2001
    K0 = 4.00D-31*((TEMP/300)**(-3.3))*M
    KI = 2.00D-12
    FC = 0.45
    F=10**(LOG10(FC)/(1.0+(SIGN(K0,KI)*(ABS(LOG10(K0/KI)))**2)))
    KMT12=(K0*KI*F)/(K0+KI)
    !
    ! * KMT13  : CH3O2    + NO2     = CH3O2NO2
    ! * IUPAC 2001
    K130     = 2.50D-30*((TEMP/300)**(-5.5))*M
    K13I     = 7.50D-12
    KR13     = K130/K13I
    FC13     = 0.36
    F13      = 10**(LOG10(FC13)/(1+(LOG10(KR13))**2))
    KMT13    = (K130*K13I)*F13/(K130+K13I)
    ! *
    ! * KMT14  : CH3O2NO2           = CH3O2   + NO2
    ! * IUPAC 2001
    K140     = 9.00D-05*EXP(-9690/TEMP)*M
    K14I     = 1.10D+16*EXP(-10560/TEMP)
    KR14     = K140/K14I
    FC14     = 0.36
    F14      = 10**(LOG10(FC14)/(1+(LOG10(KR14))**2))
    KMT14    = (K140*K14I)*F14/(K140+K14I)
    ! *
    ! * KMT15  :    OH  +  C2H4  =
    ! * IUPAC 2001
    K0=7.00D-29*((TEMP/300)**(-3.1))*M
    KI=9.00D-12
    FC=0.48
    F=10**(LOG10(FC)/(1.0+(SIGN(K0,KI)*(ABS(LOG10(K0/KI)))**2)))
    KMT15=(K0*KI*F)/(K0+KI)

    ! * KMT16  :  OH  +  C3H6
    ! * IUPAC 2001
    K160     = 8.00D-27*((TEMP/300)**(-3.5))*M
    K16I     = 3.00D-11
    KR16     = K160/K16I
    FC16     = 0.5
    F16      = 10**(LOG10(FC16)/(1+(LOG10(KR16))**2))
    KMT16    = (K160*K16I)*F16/(K160+K16I)

    ! * KMT17   :   OH   +    C2H2    =
    K0 = 5.00D-30*((TEMP/298)**(-1.5))*M
    KI = 9.40D-12*EXP(-700/TEMP)
    FC = (EXP(-TEMP/580) + EXP(-2320/TEMP))
    F=10**(LOG10(FC)/(1.0+(SIGN(K0,KI)*(ABS(LOG10(K0/KI)))**2)))
    KMT17=(K0*KI*F)/(K0+KI)

    do i = 1, nrOfPhotoRates
        if (useConstantValues .eq. 0) then
            if (cosx.lt.1.00d-10) then
                j(ck(i)) = 1.0d-30
            else
                j(ck(i)) = cl(i)*cosx**( cmm(i))*exp(-cnn(i)*secx)*transmissionFactor(i)*roofOpen*jfac
            endif
        else
            j(ck(i)) = cl(i)
        endif		
    enddo

    do  i =1,numConPhotoRates
        call getConstrainedQuantAtT2D(t,photoX,photoY,photoY2,photoNumberOfPoints(i),photoRateAtT, 2,i, &
            maxNumberOfDataPoints,numConPhotoRates)
        j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
    enddo

    include 'modelConfiguration/mechanism-rate-coefficients.f90'	
    return
end

include 'modelConfiguration/extraOutputSubroutines.f90'
