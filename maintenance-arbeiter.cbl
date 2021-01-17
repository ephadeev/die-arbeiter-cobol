      ******************************************************************
      * Add, Change, Inquire, Delete
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINTENANCE-ARBEITER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "select-arbeiter.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-arbeiter.cbl".
       WORKING-STORAGE SECTION.
       77  MENU-PICK         PIC 9.
           88 MENU-PICK-IS-VALID VALUES 0 THRU 4.
       77  THE-MODE          PIC X(7).
       77  WHICH-FIELD       PIC 9.
       77  OK-TO-DELETE      PIC X.
       77  RECORD-FOUND      PIC X.
       77  ARBEITER-ID-FIELD PIC Z(6).

       77  UPPER-ALPHA         PIC X(26) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       77  LOWER-ALPHA         PIC X(26) VALUE
           "abcdefghijklmnopqrstuvwxyz".

       01  TABLE-ABTEIL-RECORD OCCURS 50 TIMES
               INDEXED BY ABTEIL-INDEX.
           05 TABLE-ABTEILUNG-ID   PIC 9(4).
           05 TABLE-ABTEILUNG-NAME PIC X(100).
           05 TABLE-PRAEMIE        PIC 9(3).

       PROCEDURE DIVISION.
       PROGRAM-START.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O ARBEITER-FILE.

       CLOSING-PROCEDURE.
           CLOSE ARBEITER-FILE.

       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTEIN-THE-FILE
               UNTIL MENU-PICK = 0.

      ******************************************************************
      *    MENU
      ******************************************************************
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM GET-THE-PICK.
           PERFORM MENU-RETRY
               UNTIL MENU-PICK-IS-VALID.

       DISPLAY-THE-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "    PLEASE SELECT:".
           DISPLAY " ".
           DISPLAY "          1.  ADD RECORDS".
           DISPLAY "          2.  CHANGE A RECORD".
           DISPLAY "          3.  LOOK UP A RECORD".
           DISPLAY "          4.  DELETE A RECORD".
           DISPLAY " ".
           DISPLAY "          0.  EXIT".
           PERFORM SCROLL-LINE 2 TIMES.

       GET-THE-PICK.
           DISPLAY "YOUR CHOICE (0-4)?".
           ACCEPT MENU-PICK.

       MENU-RETRY.
           DISPLAY "INVALID SELECTION - PLEASE RE-TRY.".
           PERFORM GET-THE-PICK.

       CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".

       MAINTEIN-THE-FILE.
           PERFORM DO-THE-PICK.
           PERFORM GET-MENU-PICK.

       DO-THE-PICK.
           IF MENU-PICK = 1
               PERFORM ADD-MODE
           ELSE
           IF MENU-PICK = 2
               PERFORM CHANGE-MODE
           ELSE
           IF MENU-PICK = 3
               PERFORM INQUIRE-MODE
           ELSE
           IF MENU-PICK = 4
               PERFORM DELETE-MODE.

      ******************************************************************
      *    ADD
      ******************************************************************
       ADD-MODE.
           MOVE "ADD" TO THE-MODE.
           PERFORM GET-NEW-ARBEITER-ID.
           PERFORM ADD-RECORDS
               UNTIL ARBEITER-ID = ZEROES.

       GET-NEW-ARBEITER-ID.
           PERFORM INIT-ARBEITER-RECORD.
           PERFORM ENTER-ARBEITER-ID.
           MOVE "Y" TO RECORD-FOUND.
           PERFORM FIND-NEW-ARBEITER-RECORD
               UNTIL RECORD-FOUND = "N" OR ARBEITER-ID = ZEROES.

       FIND-NEW-ARBEITER-RECORD.
           PERFORM READ-ARBEITER-RECORD.
           IF RECORD-FOUND = "Y"
               DISPLAY "RECORD ALREADY ON FILE"
               PERFORM ENTER-ARBEITER-ID.

       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-ARBEITER-RECORD.
           PERFORM GET-NEW-ARBEITER-ID.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-ABTEILUNG-ID.
           PERFORM ENTER-CHIEF-ID.
           PERFORM ENTER-FAMILIE.
           PERFORM ENTER-NAME.
           PERFORM ENTER-SALARY.
           PERFORM ENTER-EINSTELL-DATUM.

      ******************************************************************
      *    CHANGE
      ******************************************************************
       CHANGE-MODE.
           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-ARBEITER-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL ARBEITER-ID = ZEROES.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD
               UNTIL WHICH-FIELD = ZERO.
           PERFORM GET-ARBEITER-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           DISPLAY "ENTER THE NUMBER OF THE FIELD".
           DISPLAY "TO CHANGE (1-6) OR 0 TO EXIT".
           ACCEPT WHICH-FIELD.
           IF WHICH-FIELD > 6
               DISPLAY "INVALID ENTRY".

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.
           PERFORM GET-FIELD-TO-CHANGE.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-ABTEILUNG-ID.
           IF WHICH-FIELD = 2
               PERFORM ENTER-CHIEF-ID.
           IF WHICH-FIELD = 3
               PERFORM ENTER-FAMILIE.
           IF WHICH-FIELD = 4
               PERFORM ENTER-NAME.
           IF WHICH-FIELD = 5
               PERFORM ENTER-SALARY.
           IF WHICH-FIELD = 6
               PERFORM ENTER-EINSTELL-DATUM.

           PERFORM REWRITE-ARBEITER-RECORD.

      ******************************************************************
      *    INQUIRE
      ******************************************************************
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-ARBEITER-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL ARBEITER-ID = ZEROES.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-ARBEITER-RECORD.

      ******************************************************************
      *    DELETE
      ******************************************************************
       DELETE-MODE.
           MOVE "DELETE" TO THE-MODE.
           PERFORM GET-ARBEITER-RECORD.
           PERFORM DELETE-RECORDS
               UNTIL ARBEITER-ID = ZEROES.

       DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           MOVE "X" TO OK-TO-DELETE.

           PERFORM ASK-TO-DELETE
               UNTIL OK-TO-DELETE = "Y" OR "N".

           IF OK-TO-DELETE = "Y"
               PERFORM DELETE-ARBEITER-RECORD.

           PERFORM GET-ARBEITER-RECORD.

       ASK-TO-DELETE.
           DISPLAY "DELETE THIS RECORD (Y/N)?".
           ACCEPT OK-TO-DELETE.
           IF OK-TO-DELETE = "y"
               MOVE "Y" TO OK-TO-DELETE.
           IF OK-TO-DELETE = "n"
               MOVE "N" TO OK-TO-DELETE.
           IF OK-TO-DELETE  NOT = "Y" AND OK-TO-DELETE NOT = "N"
               DISPLAY "YOU MUST ENTER Y OR N".

      ******************************************************************
      *    Routines shared by all modes
      ******************************************************************
       INIT-ARBEITER-RECORD.
           MOVE SPACE TO ARBEITER-RECORD.
           MOVE ZEROES TO ARBEITER-ID.

       ENTER-ARBEITER-ID.
           DISPLAY " ".
           DISPLAY "ENTER ARBEITER ID".
           DISPLAY "TO " THE-MODE " (1-999999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT ARBEITER-ID-FIELD.
           MOVE ARBEITER-ID-FIELD TO ARBEITER-ID.

       GET-ARBEITER-RECORD.
           PERFORM INIT-ARBEITER-RECORD.
           PERFORM ENTER-ARBEITER-ID.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-ARBEITER-RECORD
               UNTIL RECORD-FOUND = "Y" OR ARBEITER-ID = ZEROES.

      ******************************************************************
      *    Routines shared Add and Change
      ******************************************************************
       FIND-ARBEITER-RECORD.
           PERFORM READ-ARBEITER-RECORD.
           IF RECORD-FOUND = "N"
               DISPLAY "RECORD NOT FOUND"
               PERFORM ENTER-ARBEITER-ID.

       ENTER-ABTEILUNG-ID.
           DISPLAY "ENTER ABTEILUNG ID (1 or 2)".
           ACCEPT ABTEILUNG-ID.

       ENTER-CHIEF-ID.
           PERFORM ACCEPT-CHIEF-ID.
           PERFORM RE-ACCEPT-CHIEF-ID
               UNTIL CHIEF-ID NOT = SPACE.

       ACCEPT-CHIEF-ID.
           DISPLAY "Enter chief's ID.".
           DISPLAY "If arbeiter doesn't have a chief enter 0."
           ACCEPT CHIEF-ID.

       RE-ACCEPT-CHIEF-ID.
           DISPLAY "Chief'd ID must be entered!".
           DISPLAY "If arbeiter doesn't have a chief enter 0."
           PERFORM ACCEPT-CHIEF-ID.

       ENTER-FAMILIE.
           DISPLAY "ENTER FAMILIE".
           ACCEPT FAMILIE.

       ENTER-NAME.
           DISPLAY "ENTER NAME".
           ACCEPT NAME.

       ENTER-SALARY.
           DISPLAY "ENTER SALARY.".
           ACCEPT SALARY.

       ENTER-EINSTELL-DATUM.
           DISPLAY "ENTER EINSTEL DATUM IN FORMAT YYYY-MM-DD".
           ACCEPT EINSTELL-DATUM.

      ******************************************************************
      *    Routines shared by Change, Inquire and DELETE
      ******************************************************************
       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-ARBEITER-ID.
           PERFORM DISPLAY-ABTEILUNG-ID.
           PERFORM DISPLAY-CHIEF-ID.
           PERFORM DISPLAY-FAMILIE.
           PERFORM DISPLAY-NAME.
           PERFORM DISPLAY-SALARY.
           PERFORM DISPLAY-EINSTELL-DATUM.
           DISPLAY " ".

       DISPLAY-ARBEITER-ID.
           DISPLAY "  ARBEITER ID: " ARBEITER-ID.

       DISPLAY-ABTEILUNG-ID.
           DISPLAY "1. ABTEILUNG ID: " ABTEILUNG-ID.

       DISPLAY-CHIEF-ID.
           DISPLAY "2. CHIEF ID: " CHIEF-ID.

       DISPLAY-FAMILIE.
           DISPLAY "3. FAMILIE: " FAMILIE.

       DISPLAY-NAME.
           DISPLAY "4. NAME: " NAME.

       DISPLAY-SALARY.
           DISPLAY "5. SALARY: " SALARY.

       DISPLAY-EINSTELL-DATUM.
           DISPLAY "6. EINSTELL DATUM: " EINSTELL-DATUM.

      ******************************************************************
      *    File I-O Routines
      ******************************************************************
       READ-ARBEITER-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ ARBEITER-FILE RECORD
               WITH LOCK
               INVALID KEY
               MOVE "N" TO RECORD-FOUND.

       WRITE-ARBEITER-RECORD.
           WRITE ARBEITER-RECORD
               INVALID KEY
               DISPLAY "RECORD ALREADY ON FILE".

       REWRITE-ARBEITER-RECORD.
           REWRITE ARBEITER-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING ARBEITER RECORD".

       DELETE-ARBEITER-RECORD.
           DELETE ARBEITER-FILE RECORD
               INVALID KEY
               DISPLAY "ERROR DELETING ARBEITER RECORD".

       END PROGRAM MAINTENANCE-ARBEITER.
