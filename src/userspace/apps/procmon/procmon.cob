IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCMON.
      * CoreA Enhanced Process Monitor (COBOL)
      * Creates a process tree and reports status via pipes
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100).
       01 RECV-BUF    PIC X(100).
       01 COUNT       PIC 9(9).
       01 FD          PIC 9(9)  VALUE 1.
       01 PIPEFD      TYPE PIPE-FD.
       01 PID         PIC 9(9).
       01 GRAND-PID   PIC 9(9).
       01 RESULT      PIC 9(9).
       01 ITERATIONS  PIC 9(4)  VALUE 3.
       01 I           PIC 9(4).
       01 LEVEL       PIC 9(4).
       PROCEDURE DIVISION.
      * Check kernel configuration via external Perl script
           CALL "system" USING "perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS} && do \"config/kernel.conf\"->{SCHEDULER}'".
           IF RETURN-CODE NOT = 0
               MOVE "Fork not supported" TO BUFFER
               MOVE 16 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF.
      * Parent process
           MOVE 0 TO LEVEL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ITERATIONS
               CALL "sys_pipe_c" USING PIPEFD GIVING RESULT
               IF RESULT NOT = 0
                   MOVE "Pipe failed" TO BUFFER
                   MOVE 12 TO COUNT
                   CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
                   CALL "sys_exit_c" USING 1
               END-IF
               CALL "sys_fork_c" GIVING PID
               IF PID = -1
                   MOVE "Fork failed" TO BUFFER
                   MOVE 12 TO COUNT
                   CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
                   CALL "sys_exit_c" USING 1
               END-IF
               IF PID = 0
      * Child process
                   MOVE 1 TO LEVEL
                   CALL "sys_fork_c" GIVING GRAND-PID
                   IF GRAND-PID = 0
      * Grandchild process
                       MOVE 2 TO LEVEL
                       MOVE "Grandchild PID: " TO BUFFER
                       STRING BUFFER DELIMITED BY SIZE
                              GRAND-PID DELIMITED BY SIZE
                              " Level: " DELIMITED BY SIZE
                              LEVEL DELIMITED BY SIZE
                              INTO BUFFER
                       MOVE 50 TO COUNT
                       CALL "sys_write_c" USING WRITE-FD OF PIPEFD BUFFER COUNT GIVING RESULT
                       CALL "sys_exit_c" USING 0
                   END-IF
                   MOVE "Child PID: " TO BUFFER
                   STRING BUFFER DELIMITED BY SIZE
                          PID DELIMITED BY SIZE
                          " Level: " DELIMITED BY SIZE
                          LEVEL DELIMITED BY SIZE
                          INTO BUFFER
                   MOVE 50 TO COUNT
                   CALL "sys_write_c" USING WRITE-FD OF PIPEFD BUFFER COUNT GIVING RESULT
                   CALL "sys_exit_c" USING 0
               END-IF
      * Parent reads from pipe
               MOVE 100 TO COUNT
               CALL "sys_write_c" USING READ-FD OF PIPEFD RECV-BUF COUNT GIVING RESULT
               CALL "sys_write_c" USING FD RECV-BUF COUNT GIVING RESULT
           END-PERFORM.
           MOVE "Parent PID: " TO BUFFER
           STRING BUFFER DELIMITED BY SIZE
                  PID DELIMITED BY SIZE
                  " Level: " DELIMITED BY SIZE
                  LEVEL DELIMITED BY SIZE
                  INTO BUFFER
           MOVE 50 TO COUNT
           CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
           CALL "sys_exit_c" USING 0.
           STOP RUN.
