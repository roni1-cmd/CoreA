IDENTIFICATION DIVISION.
       PROGRAM-ID. IPC-STATS.
      * CoreA IPC Stats (COBOL)
      * Measures pipe throughput
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100) VALUE "x".
       01 RECV-BUF    PIC X(100).
       01 COUNT       PIC 9(9)   VALUE 1.
       01 FD          PIC 9(9)   VALUE 1.
       01 PIPEFD      TYPE PIPE-FD.
       01 PID         PIC 9(9).
       01 RESULT      PIC 9(9).
       01 ITERATIONS  PIC 9(4)   VALUE 1000.
       01 I           PIC 9(4).
       PROCEDURE DIVISION.
      * Check kernel configuration
           CALL "system" USING "perl -e 'exit 1 unless do \"config/kernel.conf\"->{IPC_PIPE}'".
           IF RETURN-CODE NOT = 0
               MOVE "Pipe not supported" TO BUFFER
               MOVE 17 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
      * Create pipe
           CALL "sys_pipe_c" USING PIPEFD GIVING RESULT
           IF RESULT NOT = 0
               MOVE "Pipe failed" TO BUFFER
               MOVE 12 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
      * Fork to test pipe
           CALL "sys_fork_c" GIVING PID
           IF PID = -1
               MOVE "Fork failed" TO BUFFER
               MOVE 12 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
           IF PID = 0
      * Child: write to pipe
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > ITERATIONS
                   CALL "sys_write_c" USING WRITE-FD OF PIPEFD BUFFER COUNT GIVING RESULT
               END-PERFORM
               CALL "sys_exit_c" USING 0
           ELSE
      * Parent: read from pipe
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > ITERATIONS
                   CALL "sys_write_c" USING READ-FD OF PIPEFD RECV-BUF COUNT GIVING RESULT
               END-PERFORM
               MOVE "IPC Throughput: " TO BUFFER
               STRING BUFFER DELIMITED BY SIZE
                      ITERATIONS DELIMITED BY SIZE
                      " bytes" DELIMITED BY SIZE
                      INTO BUFFER
               MOVE 50 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 0
           END-IF.
           STOP RUN.
