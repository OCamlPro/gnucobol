       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CICS-ONLY.
      *
      * Tests the mock CICS preprocessor in isolation,
      * with NO EXEC SQL blocks.
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AT.
       OBJECT-COMPUTER. IBM-AT.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

           01 DFHEIBLK        PIC X(85).
           01 DFHCOMMAREA     PIC X(256).
           01 WS-MSG          PIC X(40) VALUE 'HELLO FROM CICS'.

       PROCEDURE DIVISION.

       000-MAIN.
           DISPLAY 'CICS-ONLY TEST'.

           EXEC CICS SEND
               FROM(WS-MSG)
               LENGTH(40)
               ERASE
           END-EXEC.

           DISPLAY 'SEND COMPLETE'.

           EXEC CICS RETURN END-EXEC.

           STOP RUN.
