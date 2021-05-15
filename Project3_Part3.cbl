      *   ******************************************************************
      * Author:  Harpal Choudhary, Lofty Arif and Khalid, Gulesmin
      *    Sukru, Mustafa
      * Date: Apr 13th 2021
      * Purpose:prompts for student information and writes to file
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3.
      * AUTHOR. Harpal Choudhary, Lofty Arif and Khalid
      * Date Completed March 2nd 2021.
      *    end of identification division

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    file name used in program
           SELECT STUDENT-RECORDS-FILE
      *    External file info stored on user desktop
           ASSIGN TO "C:\Users\harpa\Desktop\Project3\STUFILE3.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      *    file name used in the progrom for program name and classes' code
           SELECT PROGRAM-RECORDS-FILE
           ASSIGN TO "C:\Users\harpa\Desktop\Project3\PROGRAM.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      *    file name used in the progrom to print out a student report
           SELECT STUDENT-REPORT-FILE
           ASSIGN TO
           "C:\Users\harpa\Desktop\Project3\STUDENT-REPORT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * name of the file used in the program.
       FD  STUDENT-RECORDS-FILE.
       01  STUDENT-TUTION-RECORD.
      * record structure of a file
           05 STUDENT-NUMBER PIC 9(6).
           05 STUDENT-TUTION-OWED PIC 9(4)V99.
           05 STUDENT-NAME PIC X(40).
           05 PROGRAM-OF-STUDY PIC X(5).
           05 COURSE-CODE-1   PIC X(7).
           05 AVERAGE-1       PIC 9(3).
           05 COURSE-CODE-2   PIC X(7).
           05 AVERAGE-2       PIC 9(3).
           05 COURSE-CODE-3   PIC X(7).
           05 AVERAGE-3       PIC 9(3).
           05 COURSE-CODE-4   PIC X(7).
           05 AVERAGE-4       PIC 9(3).
           05 COURSE-CODE-5   PIC X(7).
           05 AVERAGE-5       PIC 9(3).

       FD  PROGRAM-RECORDS-FILE.
       01  PROGRAM-RECORD.
           05 PROGRAM-CODE PIC X(5).
           05 PROGRAM-NAME PIC X(20).


       FD  STUDENT-REPORT-FILE.
       01  REPORT-HEADER PIC X(86).
       01  STUDENT-REPORT-RECORD.
           05 STUDENT-NAME-WS PIC X(40).
           05 FILLER PIC X(2) VALUE SPACES.
           05 STUDENT-AVERAGE-WS PIC 999.
           05 FILLER PIC X(4) VALUE SPACES.
           05 PROGRAM-NAME-R PIC X(20).
           05 FILLER PIC X(4) VALUE SPACES.
           05 TUITION-OWED-WS PIC Z,ZZ9.99.

       WORKING-STORAGE SECTION.

           COPY CREATE-TBL.

       01  FLAG-FIELDS.
      *to control the execution of record entering
           05 EOF-FLAG    PIC X(3) VALUE "NO".
           05 EOF-TBL-FLAG    PIC X(3) VALUE "NO".
           05 FOUND-FLAG PIC X(3) VALUE "NO".
           05 SUB PIC 9(2).
           05 PCODE PIC X(5).
           05 READ-COUNTER PIC 9(2) VALUE 0.
           05 WRITE-COUNTER PIC 9(2) VALUE 0.

       01 COLUMN-HEADER.
           05 COL-NAME PIC X(38) VALUE "NAME".
           05 FILLER PIC X(2) VALUE SPACES.
           05 COL-AVG PIC X(9) VALUE "AVERAGE".
           05 FILLER PIC X(2) VALUE SPACES.
           05 COL-PROGRAM PIC X(15) VALUE "PROGRAM".
           05 FILLER PIC X(6) VALUE SPACES.
           05 COL-TUT-OWED PIC X(11) VALUE "TUTION OWED".

       PROCEDURE DIVISION.

       PERFORM 101-STUDENT-REPORT-DATA.

       STOP RUN.

       101-STUDENT-REPORT-DATA.

           PERFORM 201-INIT-STUDENT-REPORT.
           PERFORM 202-LOAD-TABLE-DATA UNTIL EOF-TBL-FLAG = "YES".
           PERFORM 203-INT-REPORT-WRITING.
           PERFORM 204-WRITE-STUDENT-REPORT UNTIL EOF-FLAG = "YES".
           PERFORM 205-CLOSE-FILES.

       201-INIT-STUDENT-REPORT.
           PERFORM 301-OPEN-PROGRAM-FILE.
           PERFORM 302-READ-PROGRAM-FILE.

       202-LOAD-TABLE-DATA.
           PERFORM 303-LOAD-TABLE.
           PERFORM 302-READ-PROGRAM-FILE.

       203-INT-REPORT-WRITING.
           PERFORM 304-OPEN-STU-REPORT-FILE.
           PERFORM 402-PRINT-REPORT-HEADER 1 TIMES.
           PERFORM 305-READ-STUDENT-FILE.

       204-WRITE-STUDENT-REPORT.
           PERFORM 306-WRITE-STUDENT-REPORT .
           PERFORM 305-READ-STUDENT-FILE.

       301-OPEN-PROGRAM-FILE.
           OPEN INPUT PROGRAM-RECORDS-FILE.

       302-READ-PROGRAM-FILE.
           ADD 1 TO READ-COUNTER.
           READ PROGRAM-RECORDS-FILE
           AT END MOVE "YES" TO EOF-TBL-FLAG
           NOT AT END ADD 1 TO SUB.

       303-LOAD-TABLE.
           MOVE PROGRAM-RECORD TO PROGRAM-TABLE-WS(SUB).

       304-OPEN-STU-REPORT-FILE.
           OPEN INPUT STUDENT-RECORDS-FILE.
           OPEN OUTPUT STUDENT-REPORT-FILE.

       305-READ-STUDENT-FILE.
           READ STUDENT-RECORDS-FILE
           AT END MOVE "YES" TO EOF-FLAG.

       306-WRITE-STUDENT-REPORT.

           MOVE 'NO' TO FOUND-FLAG.
           PERFORM 401-SEARCH-TABLE
               VARYING SUB FROM 1 BY 1
               UNTIL SUB >20 or FOUND-FLAG = "YES".

       401-SEARCH-TABLE.
           IF (PROGRAM-CODE-WS(SUB) = PROGRAM-OF-STUDY) THEN
           MOVE "YES" TO FOUND-FLAG
           PERFORM 403-WRITE-RECORD-TO-OUTPUT
           END-IF.

       402-PRINT-REPORT-HEADER.
           WRITE REPORT-HEADER FROM COLUMN-HEADER.
           MOVE SPACES TO REPORT-HEADER.

       403-WRITE-RECORD-TO-OUTPUT.
           MOVE STUDENT-NAME TO STUDENT-NAME-WS

           CALL "C:\Users\harpa\Desktop\Project3\bin\GET-AVG"
           USING AVERAGE-1,AVERAGE-2,AVERAGE-3
           AVERAGE-4,AVERAGE-5,STUDENT-AVERAGE-WS.
           MOVE PROGRAM-NAME-WS(SUB) TO PROGRAM-NAME-R.
           MOVE STUDENT-TUTION-OWED TO TUITION-OWED-WS.
           ADD 1 TO WRITE-COUNTER.
           WRITE STUDENT-REPORT-RECORD.
           DISPLAY STUDENT-REPORT-RECORD.


       205-CLOSE-FILES.
           DISPLAY "TOTAL RECORDS READ:" READ-COUNTER.
           DISPLAY "TOTAL RECORDS WRITTEN:" WRITE-COUNTER.
           CLOSE STUDENT-RECORDS-FILE.
           CLOSE PROGRAM-RECORDS-FILE.
           CLOSE STUDENT-REPORT-FILE.

      *    end of the program
       END PROGRAM PROJECT3.
