IDENTIFICATION DIVISION.
       PROGRAM-ID. TASKSCHED.
      * CoreA Task Scheduler (COBOL)
      * Spawns tasks, synchronizes with semaphore, reports via pipes
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100).
       01 RECV-BUF    PIC X(100).
       01 COUNT       PIC 9(9).
       01 FD          PIC 9(9)  VALUE 1.
       01 PIPEFD      TYPE PIPE-FD.
       01 SEM-DATA    TYPE SEMAPHORE-DATA.
       01 PID         PIC 9(9).
       01 RESULT      PIC 9(9).
       01 TASKS       PIC 9(4)  VALUE 3.
       01 I           PIC 9(4).
       01 SUM         PIC 9(9)  VALUE 0.
       PROCEDURE DIVISION.
      * Check kernel configuration
           CALL "system" USING "perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS} && do \"config/kernel.conf\"->{SCHEDULER} && do \"config/kernel.conf\"->{IPC_SEMAPHORE} && do \"config/kernel.conf\"->{IPC_PIPE}'".
           IF RETURN-CODE NOT = 0
               MOVE "Required features disabled" TO BUFFER
               MOVE 24 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF.
      * Create semaphore
           MOVE 0 TO SEM-VALUE OF SEM-DATA
           CALL "sys_semaphore_c" USING SEM-DATA GIVING RESULT
           IF RESULT NOT = 0
               MOVE "Semaphore failed" TO BUFFER
               MOVE 16 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
      * Parent process
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TASKS
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
      * Child process: compute sum
                   PERFORM VARYING SUM FROM 1 BY 1 UNTIL SUM > 100
                       COMPUTE SUM = SUM + I
                   END-PERFORM
                   MOVE "Task " TO BUFFER
                   STRING BUFFER DELIMITED BY SIZE
                          I DELIMITED BY SIZE
                          " PID: " DELIMITED BY SIZE
                          PID DELIMITED BY SIZE
                          " Sum: " DELIMITED BY SIZE
                          SUM DELIMITED BY SIZE
                          INTO BUFFER
                   MOVE 50 TO COUNT
                   CALL "sys_write_c" USING WRITE-FD OF PIPEFD BUFFER COUNT GIVING RESULT
                   CALL "sys_semaphore_c" USING SEM-DATA GIVING RESULT
                   CALL "sys_exit_c" USING 0
               END-IF
      * Parent reads from pipe
               MOVE 100 TO COUNT
               CALL "sys_write_c" USING READ-FD OF PIPEFD RECV-BUF COUNT GIVING RESULT
               CALL "sys_write_c" USING FD RECV-BUF COUNT GIVING RESULT
           END-PERFORM
      * Release semaphore
           MOVE 1 TO SEM-VALUE OF SEM-DATA
           CALL "sys_semaphore_c" USING SEM-DATA GIVING RESULT
           MOVE "Scheduler completed" TO BUFFER
           MOVE 20 TO COUNT
           CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
           CALL "sys_exit_c" USING 0.
           STOP RUN.
