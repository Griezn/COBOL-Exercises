       IDENTIFICATION DIVISION.
       PROGRAM-ID. convert.
       AUTHOR. Seppe Degryse.

       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUDENT-IN-FILE ASSIGN TO "StudIn.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT STUDENT-MASTER-FILE ASSIGN TO "StudMaster.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SM-STUDENT-NUMBER
                   ALTERNATE RECORD KEY IS SM-STUDENT-NAME
                       WITH DUPLICATES
                   FILE STATUS IS SM-FILE-STATUS.

       
       DATA DIVISION.
           FILE SECTION.
           FD STUDENT-IN-FILE.
           01 SI-RECORD.
               88 END-OF-SIF VALUE HIGH-VALUE.
               02 SI-STUDENT-NUMBER    PIC 9(7).
               02 SI-STUDENT-NAME      PIC X(30).
               02 SI-GENDER            PIC X(1).
               02 SI-COURSE-CODE       PIC X(4).
               02 SI-FEES-OWED         PIC 9(4).
               02 SI-AMOUNT-PAID       PIC 9(4)V99.

           FD STUDENT-MASTER-FILE.
           01 SM-RECORD.
               88 END-OF-SMF VALUE HIGH-VALUE.
               02 SM-STUDENT-NUMBER    PIC 9(7).
               02 SM-STUDENT-NAME      PIC X(30).
               02 SM-GENDER            PIC X(1).
               02 SM-COURSE-CODE       PIC X(4).
               02 SM-FEES-OWED         PIC 9(4).
               02 SM-AMOUNT-PAID       PIC 9(4)V99.

           
           WORKING-STORAGE SECTION.
           01 SM-FILE-STATUS PIC XX.

       
       PROCEDURE DIVISION.
           OPEN OUTPUT STUDENT-MASTER-FILE.
           OPEN INPUT STUDENT-IN-FILE.

           READ STUDENT-IN-FILE
               AT END SET END-OF-SIF TO TRUE
           END-READ.

           PERFORM update-master-file WITH TEST BEFORE UNTIL END-OF-SIF.


           CLOSE STUDENT-IN-FILE.
           CLOSE STUDENT-MASTER-FILE.
           STOP RUN.


       UPDATE-MASTER-FILE.
           WRITE SM-RECORD FROM SI-RECORD
               INVALID KEY DISPLAY "Invalid key! FS = " SM-FILE-STATUS
           END-WRITE.
           READ STUDENT-IN-FILE
               AT END SET END-OF-SIF TO TRUE
           END-READ.
