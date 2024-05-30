       IDENTIFICATION DIVISION.
       PROGRAM-ID. update.
       AUTHOR. Seppe Degryse.

       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUDENT-MASTER-FILE ASSIGN TO "StudMaster.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SM-STUDENT-NUMBER
                   ALTERNATE RECORD KEY IS SM-STUDENT-NAME
                        WITH DUPLICATES
                   FILE STATUS IS SM-FILE-STATUS.

               SELECT STUDENT-MASTER-FILE-SORTED 
                   ASSIGN TO "StudMasterSorted.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT STUDENT-PAYMENTS-FILE ASSIGN TO "StudPay.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT WORK-FILE ASSIGN TO "work.tmp".

       
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

           FD STUDENT-MASTER-FILE-SORTED.
           01 SMS-RECORD.
               88 END-OF-SMSF VALUE HIGH-VALUE.
               02 SMS-STUDENT-NUMBER    PIC 9(7).
               02 SMS-STUDENT-NAME      PIC X(30).
               02 SMS-GENDER            PIC X(1).
               02 SMS-COURSE-CODE       PIC X(4).
               02 SMS-FEES-OWED         PIC 9(4).
               02 SMS-AMOUNT-PAID       PIC 9(4)V99.

           SD WORK-FILE.
           01 WF-RECORD.
               88 END-OF-WFF VALUE HIGH-VALUE.
               02 WF-STUDENT-NUMBER    PIC 9(7).
               02 WF-STUDENT-NAME      PIC X(30).
               02 WF-GENDER            PIC X(1).
               02 WF-COURSE-CODE       PIC X(4).
               02 WF-FEES-OWED         PIC 9(4).
               02 WF-AMOUNT-PAID       PIC 9(4)V99.
           
           FD STUDENT-PAYMENTS-FILE.
           01 SP-RECORD.
               88 END-OF-SPF VALUE HIGH-VALUE.
               02 SP-STUDENT-NUMBER    PIC 9(7).
               02 SP-PAYMENT           PIC 9(4)V99.

           
           WORKING-STORAGE SECTION.
           01 SM-FILE-STATUS PIC XX.
           01 WS-AMOUNT-PAID PIC 9(4)V99.

       
       PROCEDURE DIVISION.
           OPEN I-O STUDENT-MASTER-FILE.
           OPEN INPUT STUDENT-PAYMENTS-FILE.
           
           READ STUDENT-PAYMENTS-FILE
               AT END SET END-OF-SPF TO TRUE
           END-READ.

           PERFORM UPDATE-BALANCE UNTIL END-OF-SPF.    

           CLOSE STUDENT-PAYMENTS-FILE.
           CLOSE STUDENT-MASTER-FILE.

           SORT WORK-FILE ON ASCENDING SMS-STUDENT-NAME
               USING STUDENT-MASTER-FILE
               GIVING STUDENT-MASTER-FILE-SORTED.  

           STOP RUN.

       
       UPDATE-BALANCE.
           MOVE SP-STUDENT-NUMBER TO SM-STUDENT-NUMBER.
           READ STUDENT-MASTER-FILE
               INVALID KEY DISPLAY "Invalid key! FS: " SM-FILE-STATUS
           END-READ.
           IF SM-FILE-STATUS = "00"
               ADD SP-PAYMENT TO SM-AMOUNT-PAID
               REWRITE SM-RECORD
                   INVALID KEY DISPLAY 
                   "Not updated, invalid key! FS = " SM-FILE-STATUS
               END-REWRITE
           END-IF.
           READ STUDENT-PAYMENTS-FILE
               AT END SET END-OF-SPF TO TRUE
           END-READ.
