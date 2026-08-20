       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-TRIPLE-CHAIN.
      *
      * This program tests a 3-way preprocessor chain:
      *   EXEC CICS → EXEC DLI → EXEC SQL → cobc
      *
      * Simulates a mainframe program that:
      *   1. Uses CICS for terminal I/O
      *   2. Uses IMS/DL/I for hierarchical database access
      *   3. Uses SQL for relational database queries
      *
      * Each preprocessor handles its own EXEC blocks and
      * passes the rest through for the next preprocessor.
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AT.
       OBJECT-COMPUTER. IBM-AT.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * CICS areas
           01 DFHEIBLK        PIC X(85).
           01 DFHCOMMAREA     PIC X(256).

      * IMS/DLI areas
           01 DLI-FUNC        PIC X(4).
           01 PCB-POINTER     USAGE POINTER.
           01 IO-AREA          PIC X(256).
           01 SSA1            PIC X(50).

      * SQL areas
           01 DATASRC PIC X(64).
           01 DBUSR   PIC X(64).
           01 SQL-COUNT PIC 9(4) VALUE 0.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.

       000-START.
           DISPLAY 'TRIPLE CHAIN TEST: CICS + DLI + SQL'.

      * ── Step 1: CICS Receive ──────────────────────
           EXEC CICS RECEIVE
               INTO(DFHCOMMAREA)
           END-EXEC.

           DISPLAY 'STEP 1: CICS RECEIVE COMPLETE'.

      * ── Step 2: IMS/DLI Get Unique ────────────────
           MOVE 'GU  ' TO DLI-FUNC.

           EXEC DLI GU
               USING PCB-POINTER
               INTO IO-AREA
               SSA(SSA1)
           END-EXEC.

           DISPLAY 'STEP 2: DLI GET UNIQUE COMPLETE'.

      * ── Step 3: SQL Query ─────────────────────────
           DISPLAY "DATASRC" UPON ENVIRONMENT-NAME.
           ACCEPT DATASRC FROM ENVIRONMENT-VALUE.
           DISPLAY "DATASRC_USR" UPON ENVIRONMENT-NAME.
           ACCEPT DBUSR FROM ENVIRONMENT-VALUE.

           EXEC SQL
              CONNECT TO :DATASRC USER :DBUSR
           END-EXEC.

           IF SQLCODE NOT = 0 THEN
              DISPLAY 'SQL CONNECT FAILED: ' SQLCODE
              GO TO 900-RETURN
           END-IF.

           EXEC SQL
               SELECT COUNT(*) INTO :SQL-COUNT
               FROM EMPTABLE
           END-EXEC.

           DISPLAY 'STEP 3: SQL QUERY RESULT: ' SQL-COUNT.

           EXEC SQL CONNECT RESET END-EXEC.

      * ── Step 4: DLI Replace ───────────────────────
           MOVE 'REPL' TO DLI-FUNC.

           EXEC DLI REPL
               USING PCB-POINTER
               FROM IO-AREA
           END-EXEC.

           DISPLAY 'STEP 4: DLI REPLACE COMPLETE'.

      * ── Step 5: CICS Send + Return ────────────────
           EXEC CICS SEND
               FROM(DFHCOMMAREA)
               ERASE
           END-EXEC.

           DISPLAY 'STEP 5: CICS SEND COMPLETE'.

       900-RETURN.
           EXEC CICS RETURN END-EXEC.

           STOP RUN.
