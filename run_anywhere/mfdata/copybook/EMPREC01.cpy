      * 
      * Sample Employee Record COBOL Layout
      * 
       01 EMP-RECORD.
         05 EMP-ID                      PIC 9(5).
         05 EMP-ID-X REDEFINES EMP-ID   PIC X(5).
         05 EMP-NAME                    PIC X(25).
         05 EMP-DOB                     PIC X(10).
         05 EMP-ADDRESS OCCURS 3 TIMES.
            10 EMP-ADDR-LINE            PIC X(25).
         05 EMP-YOE-CUR                 PIC S9(4) COMP.
         05 EMP-YOE-TOTAL               PIC 9(4)V99 COMP-3.
         05 EMP-SALARY                  PIC S9(4)V99.
         05 EMP-SALARY-DIFF             PIC S9999V99 COMP-3.         
         05 EMP-DEPENDENTS-NUM          PIC S9(2).
         05 FILLER                      PIC X(17).

