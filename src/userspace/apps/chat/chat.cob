IDENTIFICATION DIVISION.
       PROGRAM-ID. CHAT.
      * CoreA Chat Client (COBOL)
      * Sends and receives messages via pipe
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100) VALUE "Hello from COBOL!".
       01 RECV-BUF    PIC X(100).
       01 COUNT       PIC 9(9)  VALUE 17.
       01 FD          PIC 9(9)  VALUE 1.
       01 PIPEFD      TYPE PIPE-FD.
       01 RESULT      PIC 9(9).
       01 PID         PIC 9(9).
       PROCEDURE DIVISION.
           CALL "sys_fork_c" GIVING PID.
           IF PID = -1
               MOVE "Fork failed" TO BUFFER
               CALL "sys_write_c" USING FD BUFFER 12 GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF.
           IF PID = 0
               CALL "sys_pipe_c" USING PIPEFD GIVING RESULT
               IF RESULT = 0
                   CALL "sys_write_c" USING WRITE-FD OF PIPEFD BUFFER COUNT GIVING RESULT
                   CALL "sys_exit_c" USING 0
               ELSE
                   MOVE "Pipe failed" TO BUFFER
                   CALL "sys_write_c" USING FD BUFFER 12 GIVING RESULT
                   CALL "sys_exit_c" USING 1
               END-IF
           ELSE
               CALL "sys_pipe_c" USING PIPEFD GIVING RESULT
               IF RESULT = 0
                   CALL "sys_write_c" USING READ-FD OF PIPEFD RECV-BUF COUNT GIVING RESULT
                   CALL "sys_write_c" USING FD RECV-BUF COUNT GIVING RESULT
                   CALL "sys_exit_c" USING 0
               ELSE
                   MOVE "Pipe failed" TO BUFFER
                   CALL "sys_write_c" USING FD BUFFER 12 GIVING RESULT
                   CALL "sys_exit_c" USING 1
               END-IF
           END-IF.
           STOP RUN.
