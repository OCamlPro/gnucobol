       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  HEXX         PIC X(35).
          88 HEXX-FILLER VALUE ALL "-".
       PROCEDURE        DIVISION.
       MAIN-PROCEDURE.
          PERFORM DO-CHECK.
          GOBACK.

       DO-CHECK.
          SET  HEXX-FILLER TO TRUE
          STRING FUNCTION HEX-OF (N"??") 
                DELIMITED BY SIZE INTO HEXX.
          IF HEXX NOT = "AC20AC20---------------------------"
             DISPLAY "UNEXPECTED HEX-VALUE OF N'??': " HEXX.
