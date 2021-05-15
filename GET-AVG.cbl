      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-AVG.
       DATA DIVISION.
       LINKAGE SECTION.

       01  AVE-1       PIC 9(3).
       01  AVE-2       PIC 9(3).
       01  AVE-3       PIC 9(3).
       01  AVE-4       PIC 9(3).
       01  AVE-5       PIC 9(3).
       01  STUD-AVE    PIC 999.

       PROCEDURE DIVISION USING AVE-1,AVE-2,AVE-3,AVE-4,AVE-5,STUD-AVE.

           ADD AVE-1 AVE-2 AVE-3 AVE-4 TO AVE-5
           DIVIDE AVE-5 BY 10 GIVING STUD-AVE ROUNDED.
       EXIT PROGRAM.
