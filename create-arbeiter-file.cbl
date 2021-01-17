      ******************************************************************
      *    Create an Empty arbeiter.dat File.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-ARBEITER-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "select-arbeiter.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-arbeiter.cbl".

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       PROGRAM-START.
           OPEN OUTPUT ARBEITER-FILE.
           CLOSE ARBEITER-FILE.

       PROGRAM-DONE.
           STOP RUN.
       END PROGRAM CREATE-ARBEITER-FILE.
