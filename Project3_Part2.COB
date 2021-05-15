      ******************************************************************
      * Author:
      * Date:Harpal, Arif and Sukru
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-STU-RECORD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INDEXED-STUDENT-FILE-OUT
           ASSIGN TO "C:\Users\harpa\Desktop\Project3\INDSTUFILE.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS IND-STUDENT-NUMBER.

       DATA DIVISION.
       FILE SECTION.

      *>INDEXED-STUDENT-FILE IS AN OUTPUT INDEXED FILE
       FD  INDEXED-STUDENT-FILE-OUT.
       01  INDEXED-FILE-RECORD.
           05  IND-STUDENT-NUMBER  PIC 9(6).
           05  IND-TUITION-OWED    PIC 9(4)V99.
           05  IND-STUDENT-NAME    PIC X(40).
           05  IND-PROGRAM-OF-STUDY PIC X(5).
           05  IND-COURSE-CODE-1   PIC X(7).
           05  IND-AVERAGE-1       PIC 9(3).
           05  IND-COURSE-CODE-2   PIC X(7).
           05  IND-AVERAGE-2       PIC 9(3).
           05  IND-COURSE-CODE-3   PIC X(7).
           05  IND-AVERAGE-3       PIC 9(3).
           05  IND-COURSE-CODE-4   PIC X(7).
           05  IND-AVERAGE-4       PIC 9(3).
           05  IND-COURSE-CODE-5   PIC X(7).
           05  IND-AVERAGE-5       PIC 9(3).

       WORKING-STORAGE SECTION.

       01  EOF-FLAG PIC X(3) VALUE "NO".
       01  SEARCH-STU-NUM PIC 9(6).
       01  TUTION-OWED PIC 9(5)V99.

       01  PROMPT-RECORD.
            05 STU-NUM PIC X(30) VALUE "PLEASE ENTER STUDENT NUMBER".
            05 STU-TUT-OWED PIC X(30) VALUE "ENTER TUTION PAYMENT".

             01  STATUS-FIELD PIC X(2).
       01  FLAGS-AND-CONTROLS.
           05 TRANS-FLAG PIC X(1).
           05 STUDENT-NUMBER-INPUT PIC 9(6).
           05 TUTION-OWNED-INPUT PIC 9(5)V99.
           05 INPUT-DATA-FLAG PIC X(4).

       SCREEN SECTION.

      * screen sction entering fields.

       01  TRANSACTION-SEREEN.
           05 VALUE "ONLINE TRANSACTION" LINE 3 COL 3.
           05 VALUE "What is the student number :" LINE 7 COL 7.
           05 STUDENT-NUMBER-TRANS LINE 7 COL 36 PIC 9(6) TO
           STUDENT-NUMBER-INPUT.
           05 VALUE "Enter tution payment: $" LINE 9 COL 7.
           05 TUTION-OWNED-TRANS LINE 9 COL 36 PIC 9(5)V99 TO
           TUTION-OWNED-INPUT.


      * screen sction entering fields.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       100-UPDATE-STUDENT-FILE.

           PERFORM 201-INITIAL-STUDENT-FILE.
           PERFORM 202-PROMPT-USER.
           PERFORM 203-READ-STUDENT-FILE.
           PERFORM 204-CLOSE-FILE.
           STOP RUN.

      * OPEN STUDENT-FILE-INT,READ THE DATA INTO INDEXED-STUDENT-FILE-IN

       201-INITIAL-STUDENT-FILE.
           PERFORM 301-OPEN-STUDENT-FILE.

       202-PROMPT-USER.
               DISPLAY TRANSACTION-SEREEN.
               ACCEPT TRANSACTION-SEREEN.
      * WRITE THE DATA INTO INDEXED FILE RECORD

       203-READ-STUDENT-FILE.

            MOVE STUDENT-NUMBER-INPUT TO IND-STUDENT-NUMBER
            READ INDEXED-STUDENT-FILE-OUT
            INVALID KEY PERFORM 301-NO-RECORD-FOUND
            NOT INVALID KEY   PERFORM 302-UPDATE-RECORD.

       301-NO-RECORD-FOUND.
           DISPLAY "SORRY RECORD NOT FOUND" LINE 15 COLUMN 26.

       302-UPDATE-RECORD.
           SUBTRACT TUTION-OWNED-INPUT FROM IND-TUITION-OWED
           REWRITE INDEXED-FILE-RECORD
           END-REWRITE
           DISPLAY "UPDATE-STUDENT RECORD" LINE 14 COLUMN 5
           DISPLAY "STUDENT NUMBER: " LINE 15 COLUMN 5
           IND-STUDENT-NUMBER LINE 15 COLUMN 26
           DISPLAY "Tution Owed: " LINE 16 COLUMN 5
           IND-TUITION-OWED LINE 16 COLUMN 26.


      * INITIAL I/O FILES, DISPLAY PROMPT
       301-OPEN-STUDENT-FILE.
           OPEN I-O  INDEXED-STUDENT-FILE-OUT.

      *CLOSE0 I/O FILES

       204-CLOSE-FILE.
           CLOSE  INDEXED-STUDENT-FILE-OUT.

      *ERROT PROMPT FOR DEBUGGING

       END PROGRAM UPDATE-STU-RECORD.
