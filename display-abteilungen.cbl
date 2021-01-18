      ******************************************************************
      *    Display details of selected Abteilung
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-ABTEILUNGEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "select-abteil.cbl".
           COPY "select-arbeiter.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-abteil.cbl".
           COPY "fd-arbeiter.cbl".

       WORKING-STORAGE SECTION.
       77  ARBEITER-FILE-AT-END PIC X.
       77  ABTEIL-FILE-AT-END   PIC X.

       01  DISPLAY-ABTEILUNG-ID  PIC 9(4).
       01  ACCEPT-ABTEIL-NAME    PIC X(100).
       01  FINDED-ABTEILUNG-ID   PIC 9(4).
       01  ARBEITER-SALARY       PIC ZZZZ9V9.

      ******************************************************************
      *    After add of new Abteilung or Arbeiter need to change:
       01  AMOUNT-VON-ABTEILUNGEN PIC 9(1) VALUE 2.
       01  AMOUNT-VON-ARBEITER    PIC 9(2) VALUE 7.
      ******************************************************************

       01  TITLE-LINE.
           05 FILLER PIC X(2)  VALUE "ID".
           05 FILLER PIC X(5)  VALUE SPACE.
           05 FILLER PIC X(7)  VALUE "Familie".
           05 FILLER PIC X(14) VALUE SPACE.
           05 FILLER PIC X(4)  VALUE "Name".
           05 FILLER PIC X(17) VALUE SPACE.
           05 FILLER PIC X(6)  VALUE "Salary".

       01  TABLE-ABTEIL-RECORD OCCURS 2 TIMES INDEXED BY ABTEIL-INDEX.
           05 TABLE-ABTEILUNG-ID   PIC 9(4).
           05 TABLE-ABTEILUNG-NAME PIC X(100).
           05 TABLE-PRAEMIE        PIC 9(3).

       01  TABLE-ARBEITER-RECORD OCCURS 7 TIMES
               INDEXED BY ARBEITER-INDEX.
           05 TABLE-ARBEITER-ID             PIC 9(6).
           05 TABLE-ARBEITER-ABTEILUNG-ID   PIC 9(4).
           05 TABLE-CHIEF-ID                PIC 9(6).
           05 TABLE-FAMILIE                 PIC X(20).
           05 TABLE-NAME                    PIC X(20).
           05 TABLE-SALARY                  PIC ZZZZ9V9.
           05 TABLE-EINSTELL-DATUM          PIC XXXXXXXXXX.

       PROCEDURE DIVISION.
       PROGRAM-START.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS
             UNTIL ACCEPT-ABTEIL-NAME = 0.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O ABTEIL-FILE.
           OPEN I-O ARBEITER-FILE.
           PERFORM LOAD-ABTEIL-TABLE.
           CLOSE ABTEIL-FILE.
           PERFORM LOAD-ARBEITER-TABLE.

       CLOSING-PROCEDURE.
           CLOSE ARBEITER-FILE.

       MAIN-PROCESS.
           DISPLAY "Enter ABTEILUNG NAME".
           DISPLAY "Testabteilung,".
           DISPLAY "AMS".
           DISPLAY "or".
           DISPLAY "0 for exit".
           ACCEPT ACCEPT-ABTEIL-NAME.
           IF ACCEPT-ABTEIL-NAME NOT = 0
               PERFORM LOOK-UP-ABTEILUNG-ID
               PERFORM LOOK-UP-ARBEITER-VON-ABTEILUNG.

       LOOK-UP-ABTEILUNG-ID.
           SET ABTEIL-INDEX TO 1.
           SEARCH TABLE-ABTEIL-RECORD
               AT END
               MOVE ZERO TO DISPLAY-ABTEILUNG-ID
               WHEN ACCEPT-ABTEIL-NAME =
               TABLE-ABTEILUNG-NAME(ABTEIL-INDEX)
               MOVE TABLE-ABTEILUNG-ID(ABTEIL-INDEX) TO
               FINDED-ABTEILUNG-ID.

      ******************************************************************
      *    ABTEIL TABLE
      ******************************************************************
       LOAD-ABTEIL-TABLE.
           PERFORM CLEAR-ABTEIL-TABLE.
           SET ABTEIL-INDEX TO 1.
           PERFORM READ-NEXT-ABTEIL-RECORD.
           PERFORM LOAD-ONE-ABTEIL-RECORD
               UNTIL ABTEIL-FILE-AT-END = "Y" OR
               ABTEIL-INDEX > AMOUNT-VON-ABTEILUNGEN.

       CLEAR-ABTEIL-TABLE.
           PERFORM CLEAR-ONE-ABTEIL-TABLE-ROW
               VARYING ABTEIL-INDEX FROM 1 BY 1
               UNTIL ABTEIL-INDEX > AMOUNT-VON-ABTEILUNGEN.

       CLEAR-ONE-ABTEIL-TABLE-ROW.
           MOVE SPACE TO TABLE-ABTEIL-RECORD(ABTEIL-INDEX).

       LOAD-ONE-ABTEIL-RECORD.
           MOVE ABTEILUNG-ID TO TABLE-ABTEILUNG-ID(ABTEIL-INDEX).
           MOVE ABTEILUNG-NAME TO TABLE-ABTEILUNG-NAME(ABTEIL-INDEX).
           PERFORM READ-NEXT-ABTEIL-RECORD.
           IF ABTEIL-FILE-AT-END NOT = "Y"
               SET ABTEIL-INDEX UP BY 1
               IF ABTEIL-INDEX > AMOUNT-VON-ABTEILUNGEN
                   DISPLAY "TABLE FULL".

       READ-NEXT-ABTEIL-RECORD.
           MOVE "N" TO ABTEIL-FILE-AT-END.
           READ ABTEIL-FILE NEXT RECORD
               AT END
               MOVE "Y" TO ABTEIL-FILE-AT-END.

      ******************************************************************
      *    ARBEITER TABLE
      ******************************************************************
       LOAD-ARBEITER-TABLE.
           PERFORM CLEAR-ARBEITER-TABLE.
           SET ARBEITER-INDEX TO 1.
           PERFORM READ-NEXT-ARBEITER-RECORD.
           PERFORM LOAD-ONE-ARBEITER-RECORD
               UNTIL ARBEITER-FILE-AT-END = "Y" OR
               ARBEITER-INDEX > AMOUNT-VON-ARBEITER.

       CLEAR-ARBEITER-TABLE.
           PERFORM CLEAR-ONE-ARBEITER-TABLE-ROW
               VARYING ARBEITER-INDEX FROM 1 BY 1
               UNTIL ARBEITER-INDEX > AMOUNT-VON-ARBEITER.

       CLEAR-ONE-ARBEITER-TABLE-ROW.
           MOVE SPACE TO TABLE-ARBEITER-RECORD(ARBEITER-INDEX).

       LOAD-ONE-ARBEITER-RECORD.
           MOVE ARBEITER-ID TO TABLE-ARBEITER-ID(ARBEITER-INDEX).
           MOVE ARBEITER-ABTEILUNG-ID TO
             TABLE-ARBEITER-ABTEILUNG-ID(ARBEITER-INDEX).
           MOVE FAMILIE TO TABLE-FAMILIE(ARBEITER-INDEX).
           MOVE ARBEITER-NAME TO TABLE-NAME(ARBEITER-INDEX).
           COMPUTE ARBEITER-SALARY = SALARY / 10.
           MOVE ARBEITER-SALARY TO TABLE-SALARY(ARBEITER-INDEX).

           PERFORM READ-NEXT-ARBEITER-RECORD.
           IF ARBEITER-FILE-AT-END NOT = "Y"
               SET ARBEITER-INDEX UP BY 1
               IF ARBEITER-INDEX > AMOUNT-VON-ARBEITER
                   DISPLAY "TABLE FULL".

       READ-NEXT-ARBEITER-RECORD.
           MOVE "N" TO ARBEITER-FILE-AT-END.
           READ ARBEITER-FILE NEXT RECORD
               AT END
               MOVE "Y" TO ARBEITER-FILE-AT-END.

       LOOK-UP-ARBEITER-VON-ABTEILUNG.
           DISPLAY TITLE-LINE.

           PERFORM LOOP-ARBEITER-TABLE
             VARYING ARBEITER-INDEX
             FROM 1 BY 1
             UNTIL ARBEITER-INDEX > AMOUNT-VON-ARBEITER.

       LOOP-ARBEITER-TABLE.
           IF ACCEPT-ABTEIL-NAME NOT = 0 AND
               TABLE-ARBEITER-ABTEILUNG-ID(ARBEITER-INDEX) =
               FINDED-ABTEILUNG-ID
               DISPLAY TABLE-ARBEITER-ID(ARBEITER-INDEX) " "
                   TABLE-FAMILIE(ARBEITER-INDEX) " "
                   TABLE-NAME(ARBEITER-INDEX) " "
                   TABLE-SALARY(ARBEITER-INDEX).

       END PROGRAM DISPLAY-ABTEILUNGEN.
