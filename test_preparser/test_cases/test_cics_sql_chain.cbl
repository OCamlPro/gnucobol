       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CICS-SQL-CHAIN.
      *
      * This program tests the mainframe-style preprocessor chain:
      *   EXEC CICS → EXEC SQL → cobc
      *
      * When cobc processes this file with both --preparser=cics.conf
      * and --preparser=gixsql.conf:
      *   1. First pass: pplex hits EXEC CICS → YYACCEPT
      *      → runs mock CICS preprocessor → comments out CICS blocks,
      *        inserts CALL "DFHEIBLK", leaves EXEC SQL untouched
      *   2. Second pass (restart_preprocess): pplex hits EXEC SQL
      *      → YYACCEPT → runs gixpp → transforms SQL blocks
      *   3. Third pass: no more EXEC blocks → normal preprocessing
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AT.
       OBJECT-COMPUTER. IBM-AT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

      * CICS communication areas (mock definitions)
           01 DFHEIBLK        PIC X(85).
           01 DFHCOMMAREA     PIC X(256).
           01 WS-MAPNAME      PIC X(8) VALUE 'EMPMAP01'.
           01 WS-MAPSET       PIC X(8) VALUE 'EMPMAPS'.

      * SQL data areas
       EXEC SQL
        INCLUDE EMPREC
       END-EXEC.

           01 DATASRC PIC X(64).
           01 DBUSR   PIC X(64).
           01 DBPWD   PIC X(64).
           01 T1      PIC 9(3) VALUE 0.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.

       000-MAIN.
           DISPLAY 'CICS+SQL CHAIN TEST'.

      * ── CICS: Receive user input ──────────────────
           EXEC CICS RECEIVE
               MAP(WS-MAPNAME)
               MAPSET(WS-MAPSET)
               INTO(DFHCOMMAREA)
           END-EXEC.

           DISPLAY 'CICS RECEIVE DONE'.

      * ── SQL: Connect and query ────────────────────
           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_PWD" UPON ENVIRONMENT-NAME.
           ACCEPT DBPWD FROM ENVIRONMENT-VALUE.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR USING :DBPWD
           END-EXEC.

           IF SQLCODE NOT = 0 THEN
              DISPLAY 'SQL CONNECT FAILED: ' SQLCODE
              GO TO 900-CICS-RETURN
           END-IF.

       100-QUERY.
           EXEC SQL
               SELECT COUNT(*) INTO :T1 FROM EMPTABLE
           END-EXEC.

           DISPLAY 'EMPLOYEE COUNT: ' T1.

           EXEC SQL CONNECT RESET END-EXEC.

      * ── CICS: Send response back ──────────────────
       200-SEND-RESPONSE.
           EXEC CICS SEND
               MAP(WS-MAPNAME)
               MAPSET(WS-MAPSET)
               FROM(DFHCOMMAREA)
               ERASE
           END-EXEC.

           DISPLAY 'CICS SEND DONE'.

      * ── CICS: Return control ──────────────────────
       900-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EMP1')
           END-EXEC.

           STOP RUN.
