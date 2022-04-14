@
!	EXERC.BAT	VERSION 3	8/11/76
!
!!		TOPS-20 SYSTEM DEMONSTRATION PACKAGE
!!
!!BATCH CONTROL JOB FOR ALL THE SECTIONS OF THE TOPS-20 SYSTEM EXERCISER.
!
!**********************************************************************!
!EXECUTION INSTRUCTIONS:
!
!FOR CUSTOMER INSTALLATIONS RUN THIS PACKAGE ONLY AS DESCRIBED IN THE
!TOPS-20 SOFTWARE INSTALLAION GUIDE.
!
!AFTER THE PACKAGE BEGINS,
!IT WILL ISSUE A PLEASE COMMAND TO THE OPERATOR
!ASKING HIM TO MOUNT A TAPE ON A SPECIFIC DRIVE.
!AFTER THIS HAS BEEN DONE, FOLLOW THESE STEPS TO CONFIRM THAT
!THE TAPE HAS BEEN MOUNTED:
!
!	AT THE CTY, ENSURE THAT YOU ARE CONNECTED TO PTYCON.  
!	THERE MUST BE A PROMPT FROM PTYCON  ("PTYCON>") AT THE CTY.
!
!	TYPE:
!		B-X-$[CR]
!
!	WHERE 	B IS THE LITERAL LETTER "B"
!		- IS THE LITERAL CHARACTER "-"
!		X IS THE SUBJOB NUMBER WHICH WAS SPECIFIED IN
!		 THE REQUEST TO MOUNT THE TAPE.
!		$ IS THE ESCAPE CHARACTER
!
!
!TO ENSURE THAT THE TEST IS RUNNING , EXAMINE THE BATCH INPUT QUEUES
!FROM TIME TO TIME.
!JOBS SUBMITTED BY EXERC SHOULD RUN CONSECUTIVELY.  WHEN THE LAST JOB
!IS RUN (EXER1),  THE DEMO PACKAGE IS OVER.
!
!
!YOU MAY EXAMINE THE BATCH INPUT QUEUES FROM THE USER TERMINAL
!LOGGED IN AS <EXERCISER> BY TYPING:
!
!		[CTRL C]
!		[CTRL C]
!		Q[CR]
!
!
! 
!
!*************************************************************
!
!INSTRUCTIONS FOR USE AS THE MANUFACTURING ACCEPTANCE TEST PACKAGE.
!
!
!
!********WHAT FOLLOWS DOES NOT APPLY FOR CUSTOMER INSTALLATIONS
!
!	THE SYSTEM DEMO PACKAGE RUNS AS AT LEAST 2 SEPERATE
!	CONCURRENT JOBS.  ONE OF THESE WILL CONSIST OF THE
!	JOB EXERC.BAT, WHICH WILL RUN INDEFINITELY IF THE ENTIRE
!	DEMO PACKAGE IS TO RUN INDEFINITELY, OTHERWISE IT WILL RUN JUST 
!	ONCE.  THE OTHER CONCURRENT
!	JOBS CONSIST OF CONSECUTIVE EXECUTIONS OF THE JOBS 
!	WHICH EXERC.BAT SUBMITS.
!
!
!	THE SOURCE FILES TO BE RUN ARE IN AREA<EXERCISER-LIBRARY>. 
!	THE TEMPORARY DATA FILES AND COMPILATION RESULTS ETC ARE TO BE
!	FOUND IN THE CONNECTED AREA, <EXERCISER>.
!
!	IF THE USER MODE DIAGNOSTICS ARE TO BE RUN THE AREA
!	<DIAGNOSTICS> MUST BE ESTABLISHED, WITH USER & DIRECTORY
!	GROUPS OF 100, AND ALL NECESSARY DIAGNOSTIC PROGRAMS
!	AND FILES IN IT. IN PARTICULAR, DIAMON.SAV AND KLUSR.CMD
!	MUST BE THERE, ALONG WITH ALL THE FILES IMPLIED BY THEM.
!
!	ALWAYS EXECUTE THE FOLLOWING COMMANDS:
!
!	FROM A USER TERMINAL, LOGIN AS <EXERCISER>,  PASSWORD    EXERCISER
!	!!!!!!! W A R N I N G !!!!!!!!! THE DEMO PACKAGE WILL NOT
!	RUN CORRECTLY UNLESS YOU ARE LOGGED IN AS EXERCISER.  
!	CONNECTING TO EXERCISER FROM ANOTHER JOB IS NOT ADEQUATE.
!
!	DELETE *.*
!	EXPUNGE
!	COPY <EXERCISER-LIBRARY>EXERC.BAT (TO) <EXERCISER>EXERC.BAT
!
!DEPENDING ON THE OPTIONS DESIRED, PERFORM ONE OR MORE OF THE
! FOLLOWING "COPY" COMMANDS:
!
!	COPY <EXERCISER-LIBRARY>DGNONE.TXT
!					-RUN USER MODE DIAGNOSTICS
!					FOR UPTO 3 CPU MINUTES.
!
!
!
!	COPY <EXERCISER-LIBRARY>DGNINF.TXT (TO) <EXERCISER>DGNINF.TXT
!					-RUN THE USER MODE DIAGNOSTICS
!					FOR UPTO 10 CPU HOURS.
!
!
!
!	COPY <EXERCISER-LIBRARY>DEMINF.TXT (TO) <EXERCISER>DEMINF.TXT
!					-RUN THE ENTIRE DEMO PACKAGE
!					INDEFINITELY.  IT MAY BE 
!					TERMINATED (AFTER COMPLETING
!					ONE MORE EXECUTION OF THE
!					PACKAGE) BY DELETEING DEMINF.TXT
!					AT ANY TIME.  NOTE-- WHEN THIS
!					FILE IS PRESENT, THE PRESENCE
!					OF DGNINF.TXT WILL BE IGNORED.
!
!
!
!
!DEPENDING ON THE OPTIONS DESIRED, USE ONE OF THE FOLLOWING "SUBMIT"
!COMMANDS:
!
!	SUBMIT EXERC.BAT/UNIQUE:0/TIME:200000/TAG:TPTST	
!					-INCLUDE MAGTAPE 
!					TESTING, USING PROGRAMS
!					WRITTEN IN HIGHER LEVEL 
!					LANGUAGES (FORTRAN)
!
!
!	SUBMIT EXERC.BAT/TAG:NOTST/UNIQUE:0/TIME:100000
!					-EXCLUDE USER LEVEL
!					MAGTAPE TESTS
!
!AFTER THE PACKAGE BEGINS, AND IF USER PROGRAM MAGTAPE TESTS
!ARE TO BE RUN,  THE PACKAGE WILL ISSUE A PLEASE COMMAND TO THE OPERATOR
!ASKING HIM TO MOUNT A TAPE ON A SPECIFIC DRIVE.
!AFTER THIS HAS BEEN DONE, FOLLOW THESE STEPS TO CONFIRM THAT
!THE TAPE HAS BEEN MOUNTED:
!
!	AT THE CTY, ENSURE THAT YOU ARE CONNECTED TO PTYCON.  
!	THERE MUST BE A PROMPT FROM PTYCON  ("PTYCON>") AT THE CTY.
!
!	TYPE:
!		B-X-$[CR]
!
!	WHERE 	B IS THE LITERAL LETTER "B"
!		- IS THE LITERAL CHARACTER "-"
!		X IS THE SUBJOB NUMBER WHICH WAS SPECIFIED IN
!		 THE REQUEST TO MOUNT THE TAPE.
!		$ IS THE ESCAPE CHARACTER
!
!
!TO ENSURE THAT THE TEST IS RUNNING , EXAMINE THE BATCH INPUT QUEUES
!FROM TIME TO TIME.  THE JOB "EXERC" SHOULD RUN AS ONE BATCH 
!JOB INDEFINITELY IF DEMINF.TXT IS PRESENT IN 
!<EXERCISER>.  UNDER THE OTHER DEFINED BATCH JOB, ALL THE
!JOBS SUBMITTED BY EXERC SHOULD RUN CONSECUTIVELY.  WHEN THE LAST JOB
!IS RUN (RANCBL), EXERC WILL SUBMIT THE JOBS AGAIN.
!
!
!YOU MAY EXAMINE THE BATCH INPUT QUEUES FROM THE USER TERMINAL
!LOGGED IN AS <EXERCISER> BY TYPING:
!
!		[CTRL C]
!		[CTRL C]
!		Q[CR]
!
!ALSO, THE LOG FILES OF THE VARIOUS BATCH JOBS WILL APPEAR ON THE
!PRINTER FROM TIME TO TIME.  YOU CAN TELL WHEN THE LATEST ONE APPEARED
!BY INSPECTING THE TIME STAMP ON THE HEADER OR TRAILER PAGE OF THE
!LISTING.  UNLESS THE SYSTEM IS HEAVILY LOADED DOWN BY SOME OTHER
!OPERATION, THE LOG FILES GET PRINTED EVERY 5 OR 10 MINUTES.
!
! 
!**********************************************************************

TPTST::

!ENTRY POINT FOR INITIATING THE USER PROGRAM MAGTAPE TESTS.
! THE FOLLOWING COMMAND LINES CREATE A FILE WHOSE EXISTENCE
! INDICATES THAT THE USER MAGTAPE TESTS BE PERFORMED.

@COPY (FROM) TTY: (TO) TPTST.TXT
@USER MAGTAPE TESTS SHOULD BE PERFORMED (B).
@^Z

NOTST::

@GOTO TAPX::

TAPX::

!CHECK TO SEE IF WE SHOULD RUN USER-MODE DIAGNOSTICS FOR THREE MINUTES.
@COPY DGNONE.TXT (TO) TTY:
@IF (ERROR) @GOTO DGN1::
!
!IF THE FILE EXISTS, SUBMIT THE JOB WITH A 3-MINUTE TIME LIMIT.
!
@COPY <EXERCISER-LIBRARY>DIAGNO.BAT (TO) <DIAGNOSTICS>DIAGNO.BAT
!
!CHECK IF FILE SHOULD BE SUBMITED WITH OUTPUT:NOLOG
@COPY <EXERCISER>PRNLOG.TXT (TO) TTY:
@IF (NOERROR) @GOTO DGN21::
@CONNECT (TO DIRECTORY) DIAGNOSTICS
@SUBMIT DIAGNO.BAT/UNIQUE:0/TIME:0300/OUTPUT:NOLOG
@CONNECT
@GOTO DGN2::
!
DGN21::
!
@SUBMIT DIAGNO.BAT/UNIQUE:0/TIME:0300
@GOTO DGN2::
!
!THE FOLLOWING CONNECT COMMAND BRINGS US BACK TO THE DIRECTORY
! WE WERE LOGGED IN UNDER.
@CONNECT
!
DGN1::
!
!CHECK TO SEE IF WE SHOULD RUN USER-MODE DIAGNOSTICS FOR A LONG TIME.
!
@COPY DGNINF.TXT (TO) TTY:
@IF (ERROR) @GOTO DGN2::
@COPY <EXERCISER-LIBRARY>DIAGNO.BAT (TO) <DIAGNOSTICS>DIAGNO.BAT
@CONNECT (TO DIRECTORY) DIAGNOSTICS
!CHECK TO SEE IF WE SHOULD SUBMIT WITH OUTPUT:NOLOG
@COPY <EXERCISER>PRNLOG.TXT (TO) TTY:
@IF (NOERROR) @GOTO DGN31::
@SUBMIT DIAGNO.BAT/UNIQUE:0/TIME:100000/OUTPUT:NOLOG
!THE FOLLOWING CONNECT COMMAND BRINGS US BACK TO THE DIRECTORY
! WE WERE LOGGED IN UNDER.
@CONNECT
@GOTO DGN2::
!
DGN31::
!
@SUBMIT DIAGNO.BAT/UNIQUE:0/TIME:100000
@CONNECT
@GOTO DGN2::
!
DGN2::
!
!
!SHOULD WE DO THE USER TAPE TESTS?
!IF THE FILE DOESN'T EXIST DON'T DO THE USER TAPE TESTS
@COPY TPTST.TXT (TO) TTY:
@IF (ERROR) @GOTO FX1::

!THE FOLLOWING SEQUENCE ATTEMPTS TO ASSIGN ANY ONE MAGTAPE DRIVE.

@ASSIGN MTA0:
@IF (ERROR) @GOTO AX::
!!WAITING FOR OPERATOR TO MOUNT A TAPE, WRITE ENABLED, ON MTA0:
@PLEASE MOUNT A TAPE, WRITE ENABLED, ON MTA0:
!NOTE -- THE JOB WILL HANG AT THIS POINT UNTIL THE OPERATOR
! CONFIRMS BY TYPING AN ALTMODE AT THE CTY
@GOTO FX::

AX::

@ASSIGN MTA1:
@IF (ERROR) @GOTO BX::
!!WAITING FOR OPERATOR TO MOUNT A TAPE, WRITE ENABLED, ON MTA1:
@PLEASE MOUNT A TAPE, WRITE ENABLED, ON MTA1:
!NOTE -- THE JOB WILL HANG AT THIS POINT UNTIL THE OPERATOR
! CONFIRMS BY TYPING AN ALTMODE AT THE CTY
@DEFINE MTA0: (AS) MTA1:
@GOTO FX::

BX::

@ASSIGN MTA2:
@IF (ERROR) @GOTO CX::
!!WAITING FOR THE OPERATOR TO MOUNT A TAPE, WRITE ENABLED, ON MTA2:
@PLEASE MOUNT A TAPE, WRITE ENABLED, ON MTA2:
!NOTE -- THE JOB WILL HANG AT THIS POINT UNTIL THE OPERATOR
! CONFIRMS BY TYPING AN ALTMODE AT THE CTY
@DEFINE MTA0: (AS) MTA2:
@GOTO FX::

CX::

@ASSIGN MTA3:
!IF NO TAPES ARE AVAILABLE, STOP THE JOB NOW AND TRY AGAIN IN 10 MINUTES
@IF (ERROR) @GOTO GX::
@PLEASE MOUNT A TAPE, WRITE ENABLED, ON MTA3:
!NOTE -- THE JOB WILL HANG AT THIS POINT UNTIL THE OPERATOR
! CONFIRMS BY TYPING AN ALTMODE AT THE CTY
@DEFINE MTA0: (AS) MTA3:
@GOTO FX::

GX::

!!NO TAPES AVAILABLE, ENTIRE JOB WILL BE REQUEUED.
@REQUEUE

FX::

!!TAPE HAS BEEN MOUNTED.

FX1::

@INFORMATION (ABOUT) DISK-USAGE (OF DIRECTORY)
@CONNECT <EXERCISER> EXERCISER
@DEFINE (LOGICAL NAME) DSK (AS) DSK:,<EXERCISER-LIBRARY>

@RUN ERRORS
@IF (NOERROR) @GOTO AA1
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++	TEST EXERC.BAT - MAIN DRIVER CONTROL FILE  ++++
@	FATAL ERROR WHILE CREATING ERRORS.LOG 
@++++	[END OF EXERC.LOG ERROR]		   ++++
@
@^Z

AA1::

!!ERROR.LOG CREATED -- ACCUMULATES ALL ERROR MESSAGES.

!COPY THE CONTROL FILES FROM THE LIBRARY DIRECTORY

AA::

!THE FOLLOWING PRINTS THE CURRENT CONTENTS OF EXERC.LOG
! AND DELETES IT SO THAT IT DOESN'T ACCUMULATE OUT OF BOUNDS.
!++++	NOTE -- EXERC.LOG WILL BE DELETED AT THIS POINT.	++++
@CHKPNT FOO

FOO::
!++++	EXERC.LOG CONTINUES AT THIS POINT	++++

@COPY EXERC.LOG (TO) LPT:EXERC.LOG
@DELETE EXERC.LOG


@IF (ERROR) @GOTO AAA::

AAA::
@EXPUNGE

!BRING OVER USER MAGTAPE TESTS.

@COPY (FROM) MTABKR.EXE (TO) MTABKR.EXE
@IF (NOERROR) @GOTO AA1::
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++  EXERC.BAT - NON-FATAL ERROR  ++++
@      CANNOT COPY FILE MTABKR.EXE FROM LIBRARY
@++++  [END OF EXERC.LOG ERROR]
@
@^Z

AA1::

@COPY (FROM) MTABAK.EXE (TO) MTABAK.EXE
@IF (NOERROR) @GOTO AA2::
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++  EXERC.BAT - NON-FATAL ERROR  ++++
@      CANNOT COPY FILE MTABAK.EXE FROM LIBRARY
@++++  [END OF EXERC.LOG ERROR]     ++++
@
@^Z

AA2::

@COPY (FROM) MTASKF.EXE (TO) MTASKF.EXE
@IF (NOERROR) @GOTO AA3::
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++  EXERC.BAT - NON-FATAL ERROR  ++++
@      CANNOT COPY MTASKF.EXE FROM LIBRARY
@++++  [END OF EXERC.LOG ERROR]     ++++
@
@^Z

AA3::

@COPY (FROM) MTASKR.EXE (TO) MTASKR.EXE
@IF (NOERROR) @GOTO AA4::
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++   EXERC.BAT - NON-FATAL ERROR  ++++
@       CANNOT COPY MTASKR.EXE FROM LIBRARY
@++++   [END OF EXERC.LOG ERROR]
@
@^Z

AA4::

@CHKPNT A

A::

BB::

@CHKPNT B

B::

@COPY (FROM) RANCBL.BAT (TO) RANCBL.BAT
@IF (NOERROR) @GOTO MM
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++	EXERC.BAT - NON-FATAL ERROR  ++++
@	CANNOT COPY FILE RANCBL.BAT FROM LIBRARY
@++++	[END OF EXERC.LOG ERROR]     ++++
@
@^Z

MM::

@CHKPNT M

M::

@COPY (FROM) RANFOR.BAT (TO) RANFOR.BAT
@IF (NOERROR) @GOTO NN
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++	EXERC.BAT - NON-FATAL ERROR  ++++
@	CANNOT COPY FILE RANFOR.BAT FROM LIBRARY
@++++	[END OF EXERC.LOG ERROR]     ++++
@
@^Z

NN::

@CHKPNT N

N::

!!ALL CONTROL FILES HAVE BEEN COPIED FROM THE LIBRARY DIRECTORY
!! <EXERCISER-LIBRARY> INTO <EXERCISER>
@CHKPNT O

O::

PP::

@CHKPNT Q

Q::

!	THIS IS THE SET OF FORTRAN RANDOMACCESS TESTS BOTH BINARY AND
!	FORMATTED I/O

SS::

@CHKPNT S

S::

!CHECK FOR OUTPUT:NOLOG
@COPY <EXERCISER>PRNLOG.TXT (TO) TTY:
@IF (NOERROR) @GOTO S1::
@SUBMIT RANFOR.BAT/UNIQUE:0/OUTPUT:NOLOG
@GOTO S2::
S1::
@SUBMIT RANFOR.BAT/UNIQUE:0
S2::
!!RANFOR.BAT SUBMITTED
@IF (NOERROR) @GOTO TT
@APPEND (SOURCE FILE) TTY: (TO) ERRORS.LOG
@++++	EXERC.BAT - ERROR SUBMITTING RANFOR.BAT  ++++
@	RANFOR JOB WILL NOT BE RUN
@++++	[END OF EXERC.LOG ERROR]		 ++++
@
@^Z

!	THIS IS THE COBOL I/O EXERCISER.  IT MOSTLY OPENS, CLOSES,
!	WRITES AND READS A BUNCH OF COBOL FILES IN VARIOUS MODES.
!	FOLLOWING THIS ARE MAGTAPE TESTS, STANDARD SEQUENTIAL,
!	I/O SEQUENTIAL SIXBIT RANDOM READ/WRITE, ASCII RANDOM
!	READ/WRITE, AND BINARY RANDOM READ/WRITE TESTS. THE
!	READ/WRITE NUMBER IS SPECIFIED IN THE .BAT FILE.

TT::

@CHKPNT T

T::

UU::

@CHKPNT U

U::

!CHECK FOR OUTPUT:NOLOG
@COPY <EXERCISER>PRNLOG.TXT (TO) TTY:
@IF (NOERROR) @GOTO U1::
@SUBMIT RANCBL.BAT/UNIQUE:0/OUTP