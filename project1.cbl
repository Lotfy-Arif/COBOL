      ******************************************************************
      * Author: LOTFY_ARIF
      * Date: 28/1/2021
      * Purpose: MAKE STUDENT SHEET
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-FEES-SHEET.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL STUDENT-FEES-SHEET
           ASSIGN TO
           "D:\Algonquin\1\STUDENT-RECORD.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FEES-SHEET.
       01  STDUENT-FEES-INFO.
           05 STUDENT-NUMBER   PIC 9(6).
           05 FILLER PIC X(5) VALUES SPACES.
           05 TUITION-OWED     PIC 9(6).
           05 FILLER PIC X(5) VALUES SPACES.
           05 STUDENT-NAME     PIC A(40).
           05 FILLER PIC X(5) VALUES SPACES.
       01  USER-PROMPT.
           05 USER-INPUT PIC A(1).
       WORKING-STORAGE SECTION.
       01  INPUT-STUDENT-FEES.
           05 INPUT-STUDENT-NUMBER   PIC 9(6).
           05 FILLER PIC X(5) VALUES SPACES.
           05 INPUT-TUITION-OWED     PIC 9(6).
           05 FILLER PIC X(5) VALUES SPACES.
           05 INPUT-STUDENT-NAME     PIC A(40).
           05 FILLER PIC X(5) VALUES SPACES.
       01  WSUSER-PROMPT.
           05 WSUSER-INPUT PIC A(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       ADD-STUDENT-RECORD.
           PERFORM INTIIATE-STUDENT-RECORD.
           PERFORM TAKE-USER-INPUT.
           PERFORM CLOSE-INPUT-FILE.
       INTIIATE-STUDENT-RECORD.
           PERFORM OPEN-STUDENT-FEES-FILE.
           PERFORM PROMPT-FOR-RECORD-INPUT.
           PERFORM CONF-NEW-RECORD.
       TAKE-USER-INPUT.
           PERFORM PROMPT-USER-INPUT.
           PERFORM WRITE-INPUT.
           PERFORM ANOTHER-RECORD.
           PERFORM CONF-NEW-RECORD
           PERFORM CLOSE-INPUT-FILE.
       OPEN-STUDENT-FEES-FILE.
           OPEN OUTPUT STUDENT-FEES-SHEET.
       PROMPT-FOR-RECORD-INPUT.
           DISPLAY "Would you like to enter a new student info ? Y/N".
           ACCEPT USER-INPUT.
       CONF-NEW-RECORD.
           PERFORM TAKE-USER-INPUT
               UNTIL USER-INPUT = "N".
       PROMPT-USER-INPUT.
           DISPLAY "Enter the student number".
           ACCEPT STUDENT-NUMBER .
           DISPLAY "Enter the tuition owed by that student".
           ACCEPT TUITION-OWED .
           DISPLAY "Enter the stduent name".
           ACCEPT STUDENT-NAME.
       WRITE-INPUT.
           WRITE STDUENT-FEES-INFO.
       ANOTHER-RECORD.
           DISPLAY "Would you like to enter another student info ? Y/N".
           ACCEPT USER-INPUT.


       CLOSE-INPUT-FILE.
           CLOSE STUDENT-FEES-SHEET.
            STOP RUN.
       END PROGRAM STUDENT-FEES-SHEET.
