      ******************************************************************
      *    Create an Empty abteil.dat File.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ABTEIL-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "select-abteil.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-abteil.cbl".

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       PROGRAM-START.
           OPEN OUTPUT ABTEIL-FILE.
           CLOSE ABTEIL-FILE.

       PROGRAM-DONE.
           STOP RUN.
       END PROGRAM CREATE-ABTEIL-FILE.
