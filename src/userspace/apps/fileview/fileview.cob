IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEVIEW.
      * CoreA File Viewer (COBOL)
      * Reads and displays file contents
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "test.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 FILE-RECORD PIC X(100).
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100).
       01 COUNT       PIC 9(9).
       01 FD          PIC 9(9)  VALUE 1.
       01 RESULT      PIC 9(9).
       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO BUFFER
               AT END
                   MOVE "No data" TO BUFFER
                   MOVE 8 TO COUNT
                   CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
                   CLOSE INPUT-FILE
                   CALL "sys_exit_c" USING 0
           END-READ.
           MOVE 100 TO COUNT.
           CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT.
           CLOSE INPUT-FILE.
           CALL "sys_exit_c" USING 0.
           STOP RUN.
