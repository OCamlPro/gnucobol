       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-INSERT-UPDATE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AT.
       OBJECT-COMPUTER. IBM-AT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       EXEC SQL
        INCLUDE EMPREC
       END-EXEC.

           01 DATASRC PIC X(64).
           01 DBUSR   PIC X(64).
           01 DBPWD   PIC X(64).
           01 REC-COUNT PIC 9(4) VALUE 0.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.

       000-CONNECT.
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
              DISPLAY 'CONNECT FAILED: ' SQLCODE
              GO TO 999-EXIT
           END-IF.

       100-INSERT.
           MOVE 999 TO ENO.
           MOVE 'TestLast' TO LNAME.
           MOVE 'TestFirst' TO FNAME.
           MOVE '123 Test St' TO STREET.
           MOVE 'TestCity' TO CITY.
           MOVE 'TS' TO ST.
           MOVE '99999' TO ZIP.
           MOVE 'TST1' TO DEPT.
           MOVE 100.00 TO PAYRATE.
           MOVE 1.50 TO COM.

           EXEC SQL
               INSERT INTO EMPTABLE
               (ENO, LNAME, FNAME, STREET, CITY, ST, ZIP,
                DEPT, PAYRATE, COM)
               VALUES
               (:ENO, :LNAME, :FNAME, :STREET, :CITY, :ST,
                :ZIP, :DEPT, :PAYRATE, :COM)
           END-EXEC.

           DISPLAY 'INSERT SQLCODE: ' SQLCODE.

       200-UPDATE.
           MOVE 'Updated' TO LNAME.

           EXEC SQL
               UPDATE EMPTABLE
               SET LNAME = :LNAME
               WHERE ENO = :ENO
           END-EXEC.

           DISPLAY 'UPDATE SQLCODE: ' SQLCODE.

       300-VERIFY.
           EXEC SQL
               SELECT COUNT(*) INTO :REC-COUNT
               FROM EMPTABLE WHERE ENO = :ENO
           END-EXEC.

           DISPLAY 'VERIFY COUNT: ' REC-COUNT.

       400-CLEANUP.
           EXEC SQL
               DELETE FROM EMPTABLE WHERE ENO = 999
           END-EXEC.

           DISPLAY 'DELETE SQLCODE: ' SQLCODE.

           EXEC SQL CONNECT RESET END-EXEC.

       999-EXIT.
           STOP RUN.
