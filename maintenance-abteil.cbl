      ******************************************************************
      * Add, Change, Inquire, Delete
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINTENANCE-ABTEIL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "select-abteil.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fd-abteil.cbl".

       WORKING-STORAGE SECTION.
       77  MENU-PICK PIC 9.
           88 MENU-PICK-IS-VALID VALUES 0 THRU 4.

       77  THE-MODE           PIC X(7).
       77  OK-TO-DELETE       PIC X.
       77  RECORD-FOUND       PIC X.
       77  WHICH-FIELD        PIC 9.
       77  ABTEILUNG-ID-FIELD PIC Z(4).

       77  UPPER-ALPHA         PIC X(26) VALUE
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       77  LOWER-ALPHA         PIC X(26) VALUE
           "abcdefghijklmnopqrstuvwxyz".

       PROCEDURE DIVISION.
       PROGRAM-START.
           PERFORM OPENING-PROCEDURE.
           PERFORM MAIN-PROCESS.
           PERFORM CLOSING-PROCEDURE.

       PROGRAM-DONE.
           STOP RUN.

       OPENING-PROCEDURE.
           OPEN I-O ABTEIL-FILE.

       CLOSING-PROCEDURE.
           CLOSE ABTEIL-FILE.

       MAIN-PROCESS.
           PERFORM GET-MENU-PICK.
           PERFORM MAINTAIN-THE-FILE
               UNTIL MENU-PICK = 0.

      ******************************************************************
      *    MENU
      ******************************************************************
       GET-MENU-PICK.
           PERFORM DISPLAY-THE-MENU.
           PERFORM ACCEPT-MENU-PICK.
           PERFORM RE-ACCEPT-MENU-PICK
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
           DISPLAY "          0. EXIT".
           PERFORM SCROLL-LINE 2 TIMES.

       ACCEPT-MENU-PICK.
           DISPLAY "YOUR CHOICE (0-4)?".
           ACCEPT MENU-PICK.

       RE-ACCEPT-MENU-PICK.
           DISPLAY "INVALID SELECTION - PLEASE RE-TRY.".
           PERFORM ACCEPT-MENU-PICK.

       CLEAR-SCREEN.
           PERFORM SCROLL-LINE 25 TIMES.

       SCROLL-LINE.
           DISPLAY " ".

       MAINTAIN-THE-FILE.
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
           PERFORM GET-NEW-ABTEILUNG-ID.
           PERFORM ADD-RECORDS
               UNTIL ABTEILUNG-ID = 0.

       GET-NEW-ABTEILUNG-ID.
           PERFORM INIT-ABTEIL-RECORD.
           PERFORM ENTER-ABTEILUNG-ID.
           MOVE "Y" TO RECORD-FOUND.
           PERFORM FIND-NEW-ABTEIL-RECORD
               UNTIL RECORD-FOUND = "N" OR ABTEILUNG-ID = ZERO.

       FIND-NEW-ABTEIL-RECORD.
           PERFORM READ-ABTEIL-RECORD.
           IF RECORD-FOUND = "Y"
               DISPLAY "RECORD ALREADY ON FILE"
               PERFORM ENTER-ABTEILUNG-ID.

       ADD-RECORDS.
           PERFORM ENTER-REMAINING-FIELDS.
           PERFORM WRITE-ABTEIL-RECORD.
           PERFORM GET-NEW-ABTEILUNG-ID.

       ENTER-REMAINING-FIELDS.
           PERFORM ENTER-ABTEILUNG-NAME.
           PERFORM ENTER-PRAEMIE.

      ******************************************************************
      *    CHANGE
      ******************************************************************
       CHANGE-MODE.
           MOVE "CHANGE" TO THE-MODE.
           PERFORM GET-ABTEIL-RECORD.
           PERFORM CHANGE-RECORDS
               UNTIL ABTEILUNG-ID = 0.

       CHANGE-RECORDS.
           PERFORM GET-FIELD-TO-CHANGE.
           PERFORM CHANGE-ONE-FIELD.
           PERFORM GET-ABTEIL-RECORD.

       GET-FIELD-TO-CHANGE.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-WHICH-FIELD.

       ASK-WHICH-FIELD.
           MOVE 1 TO WHICH-FIELD.

       CHANGE-ONE-FIELD.
           PERFORM CHANGE-THIS-FIELD.

       CHANGE-THIS-FIELD.
           IF WHICH-FIELD = 1
               PERFORM ENTER-ABTEILUNG-NAME.
           IF WHICH-FIELD = 2
               PERFORM ENTER-PRAEMIE.
           PERFORM REWRITE-ABTEIL-RECORD.

      ******************************************************************
      *    INQUIRE
      ******************************************************************
       INQUIRE-MODE.
           MOVE "DISPLAY" TO THE-MODE.
           PERFORM GET-ABTEIL-RECORD.
           PERFORM INQUIRE-RECORDS
               UNTIL ABTEILUNG-ID = ZERO.

       INQUIRE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM GET-ABTEIL-RECORD.

      ******************************************************************
      *    DELETE
      ******************************************************************
       DELETE-MODE.
           MOVE "DELETE" TO THE-MODE.
           PERFORM GET-ABTEIL-RECORD.
           PERFORM DELETE-RECORDS
               UNTIL ABTEILUNG-ID = ZERO.

       DELETE-RECORDS.
           PERFORM DISPLAY-ALL-FIELDS.
           PERFORM ASK-OK-TO-DELETE.
           IF OK-TO-DELETE = "Y"
               PERFORM DELETE-ABTEIL-RECORD.
           PERFORM GET-ABTEIL-RECORD.

       ASK-OK-TO-DELETE.
           PERFORM ACCEPT-OK-TO-DELETE.
           PERFORM RE-ACCEPT-OK-TO-DELETE
               UNTIL OK-TO-DELETE = "Y" OR "N".

       ACCEPT-OK-TO-DELETE.
           DISPLAY "DELETE THIS RECORD (Y/N)?".
           ACCEPT OK-TO-DELETE.
           INSPECT OK-TO-DELETE
               CONVERTING LOWER-ALPHA
               TO         UPPER-ALPHA.

       RE-ACCEPT-OK-TO-DELETE.
           DISPLAY "YOU MUST ENTER Y OR N".
           PERFORM ACCEPT-OK-TO-DELETE.

      ******************************************************************
      *    Routines shared by all modes
      ******************************************************************
       INIT-ABTEIL-RECORD.
           MOVE SPACE TO ABTEIL-RECORD.

       ENTER-ABTEILUNG-ID.
           PERFORM ACCEPT-ABTEILUNG-ID.
           PERFORM RE-ACCEPT-ABTEILUNG-ID
               UNTIL ABTEILUNG-ID NOT = SPACE.

       ACCEPT-ABTEILUNG-ID.
           DISPLAY " ".
           DISPLAY "ENTER ABTEILUNG ID".
           DISPLAY "TO " THE-MODE " (1-9999)".
           DISPLAY "ENTER 0 TO STOP ENTRY".
           ACCEPT ABTEILUNG-ID-FIELD.
           MOVE ABTEILUNG-ID-FIELD TO ABTEILUNG-ID.

       RE-ACCEPT-ABTEILUNG-ID.
           DISPLAY "ABTEILUNG ID MUST BE ENTERED".
           PERFORM ACCEPT-ABTEILUNG-ID.

       GET-ABTEIL-RECORD.
           PERFORM INIT-ABTEIL-RECORD.
           PERFORM ENTER-ABTEILUNG-ID.
           MOVE "N" TO RECORD-FOUND.
           PERFORM FIND-ABTEIL-RECORD
               UNTIL RECORD-FOUND = "Y" OR ABTEILUNG-ID = ZERO.

      ******************************************************************
      *    Routines shared Add and Change
      ******************************************************************
       FIND-ABTEIL-RECORD.
           PERFORM READ-ABTEIL-RECORD.
           IF RECORD-FOUND = "N"
               DISPLAY "RECORD NOT FOUND"
               PERFORM ENTER-ABTEILUNG-ID.

       ENTER-ABTEILUNG-NAME.
           PERFORM ACCEPT-ABTEILUNG-NAME.
           PERFORM RE-ACCEPT-ABTEILUNG-NAME
               UNTIL ABTEILUNG-NAME NOT = SPACES.

       ACCEPT-ABTEILUNG-NAME.
           DISPLAY "ENTER ABTEILUNG NAME".
           ACCEPT ABTEILUNG-NAME.

       RE-ACCEPT-ABTEILUNG-NAME.
           DISPLAY "ABTEILUNG NAME MUST BE ENTERED".
           PERFORM ACCEPT-ABTEILUNG-NAME.

       ENTER-PRAEMIE.
           DISPLAY "ENTER PRAEMIE".
           ACCEPT PRAEMIE.

      ******************************************************************
      *    Routines shared by Change, Inquier and Delete
      ******************************************************************
       DISPLAY-ALL-FIELDS.
           DISPLAY " ".
           PERFORM DISPLAY-ABTEILUNG-ID.
           PERFORM DISPLAY-ABTEILUNG-NAME.
           PERFORM DISPLAY-PRAEMIE.
           DISPLAY " ".

       DISPLAY-ABTEILUNG-ID.
           DISPLAY "   ABTEILUNG ID: " ABTEILUNG-ID.

       DISPLAY-ABTEILUNG-NAME.
           DISPLAY "1. ABTEILUNG NAME: " ABTEILUNG-NAME.

       DISPLAY-PRAEMIE.
           DISPLAY "2. PRAEMIE: " PRAEMIE.

      ******************************************************************
      *    File I-O Routines
      ******************************************************************
       READ-ABTEIL-RECORD.
           MOVE "Y" TO RECORD-FOUND.
           READ ABTEIL-FILE RECORD
               WITH LOCK
               INVALID KEY
               MOVE "N" TO RECORD-FOUND.

       WRITE-ABTEIL-RECORD.
           WRITE ABTEIL-RECORD
               INVALID KEY
               DISPLAY "RECORD IS ALREADY ON FILE".

       REWRITE-ABTEIL-RECORD.
           REWRITE ABTEIL-RECORD
               INVALID KEY
               DISPLAY "ERROR REWRITING ABTEIL RECORD".

       DELETE-ABTEIL-RECORD.
           DELETE ABTEIL-FILE RECORD
               INVALID KEY
               DISPLAY "ERROR DELETING ABTEIL RECORD".

       END PROGRAM MAINTENANCE-ABTEIL.
