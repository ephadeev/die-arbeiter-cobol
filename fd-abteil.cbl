      ******************************************************************
      *    Primary Key - ABTEILUNG-ID
      *    ABTEILUNG-NAME & PRAEMIE are required
      *    ABTEILUNG-NAME - ALTERNATE KEY
      *    PRAEMIE - ALTERNATE KEY WITH DUPLICATES
      ******************************************************************
       FD  ABTEIL-FILE.
       01  ABTEIL-RECORD.
           05 ABTEILUNG-ID   PIC 9(4).
           05 ABTEILUNG-NAME PIC X(100).
           05 PRAEMIE        PIC 9(3).
