       IDENTIFICATION DIVISION.
       PROGRAM-ID. report.
       AUTHOR. Seppe Degryse.

       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUDENT-MASTER-FILE ASSIGN TO 
                   "StudMasterSorted.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT REPORT-FILE ASSIGN TO "Fees.rpt".
       
       DATA DIVISION.
           FILE SECTION.
           FD STUDENT-MASTER-FILE.
           01 SM-RECORD.
               88 END-OF-SMF VALUE HIGH-VALUE.
               02 SM-STUDENT-NUMBER    PIC 9(7).
               02 SM-STUDENT-NAME      PIC X(30).
               02 SM-GENDER            PIC X(1).
               02 SM-COURSE-CODE       PIC X(4).
               02 SM-FEES-OWED         PIC 9(4).
               02 SM-AMOUNT-PAID       PIC 9(4)V99.

           
           FD REPORT-FILE
           REPORT IS FEES-REPORT.


           WORKING-STORAGE SECTION.
           01 OUSTANDING.
               02 WS-OUTSTANDING       PIC 9(4)V99.
               02 WS-OUTSTANDING-TOTAL PIC 9(8)V99 VALUE ZERO.


           REPORT SECTION.
           RD FEES-REPORT
               CONTROL IS FINAL
               PAGE LIMIT IS 66
               HEADING 1
               FIRST DETAIL 8
               LAST DETAIL 42
               FOOTING 52.

           01 TYPE IS PAGE HEADING.
               02 LINE 2.
                   03 COLUMN 25    PIC X(25)
                       VALUE "OUTSTANDING  FEES  REPORT".
               02 LINE 3.
                   03 COLUMN 24    PIC X(27)
                       VALUE ALL "-".
               02 LINE 6.
                   03 COLUMN 2     PIC X(12)
                       VALUE "STUDENT NAME".
                   03 COLUMN 31    PIC X(11)
                       VALUE "STUDENT NO.".
                   03 COLUMN 45    PIC X(4)
                       VALUE "FEES".
                   03 COLUMN 54    PIC X(8)
                       VALUE "AMT PAID".
                   03 COLUMN 66    PIC X(11)
                       VALUE "AMT OUTSTND".

           01 DETAIL-LINE TYPE IS DETAIL.
               02 LINE IS PLUS 1.
                   03 COLUMN 1     PIC X(30)
                       SOURCE SM-STUDENT-NAME.
                   03 COLUMN 33    PIC X(7)
                       SOURCE SM-STUDENT-NUMBER.
                   03 COLUMN 45    PIC $$,$$9
                       SOURCE SM-FEES-OWED.
                   03 COLUMN 54    PIC $$,$$9.99
                       SOURCE SM-AMOUNT-PAID.
                   03 COLUMN 66    PIC $$,$$9.99
                       SOURCE WS-OUTSTANDING.
           
           01 TOTAL-OUTSTANDING TYPE IS CONTROL FOOTING FINAL.
               02 LINE IS PLUS 2.
                   03 COLUMN 42    PIC X(20)
                       VALUE "TOTAL OUTSTANDING = ".
                   03 COLUMN 62    PIC $$,$$$,$$9.99
                       SOURCE WS-OUTSTANDING-TOTAL.


       PROCEDURE DIVISION.
           OPEN INPUT STUDENT-MASTER-FILE.
           OPEN OUTPUT REPORT-FILE.

           READ STUDENT-MASTER-FILE
               AT END SET END-OF-SMF TO TRUE
           END-READ.

           INITIATE FEES-REPORT.

           PERFORM GENERATE-FEES-REPORT UNTIL END-OF-SMF.

           TERMINATE FEES-REPORT.

           CLOSE STUDENT-MASTER-FILE, REPORT-FILE.

           STOP RUN.

 
       GENERATE-FEES-REPORT.
           SUBTRACT SM-AMOUNT-PAID FROM SM-FEES-OWED
               GIVING WS-OUTSTANDING.

           IF SM-AMOUNT-PAID IS LESS THAN SM-FEES-OWED
               ADD WS-OUTSTANDING TO WS-OUTSTANDING-TOTAL
           
               GENERATE DETAIL-LINE

           END-IF.

           READ STUDENT-MASTER-FILE
               AT END SET END-OF-SMF TO TRUE
           END-READ.
              
           
           
