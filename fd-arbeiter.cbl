      ******************************************************************
      *    Primary Key - ARBEITER-ID
      *    Alternate Key with duplicates - all fields except ARBEITER-ID
      ******************************************************************
       FD  ARBEITER-FILE.
       01  ARBEITER-RECORD.
           05 ARBEITER-ID    PIC 9(6).
           05 ARBEITER-ABTEILUNG-ID   PIC 9(4).
           05 CHIEF-ID       PIC 9(6).
           05 FAMILIE        PIC X(100).
           05 ARBEITER-NAME  PIC X(100).
           05 SALARY         PIC 99999V9.
           05 EINSTELL-DATUM PIC XXXXXXXXXX.
