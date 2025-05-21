IDENTIFICATION DIVISION.
       PROGRAM-ID. MEM-ANALYZER.
      * CoreA Memory Analyzer (COBOL)
      * Reports shared memory usage
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "syscall.cpy".
       01 BUFFER      PIC X(100).
       01 COUNT       PIC 9(9).
       01 FD          PIC 9(9)  VALUE 1.
       01 SHM-DATA    TYPE SHM-DATA.
       01 RESULT      PIC 9(9).
       01 SIZE        PIC 9(9)  VALUE 1024.
       PROCEDURE DIVISION.
      * Check kernel configuration
           CALL "system" USING "perl -e 'exit 1 unless do \"config/kernel.conf\"->{IPC_SHM}'".
           IF RETURN-CODE NOT = 0
               MOVE "SHM not supported" TO BUFFER
               MOVE 16 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
      * Allocate shared memory
           MOVE SIZE TO SHM-SIZE OF SHM-DATA
           CALL "sys_shm_c" USING SHM-DATA GIVING RESULT
           IF RESULT NOT = 0
               MOVE "SHM allocation failed" TO BUFFER
               MOVE 20 TO COUNT
               CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
               CALL "sys_exit_c" USING 1
           END-IF
      * Report usage
           MOVE "Allocated SHM: " TO BUFFER
           STRING BUFFER DELIMITED BY SIZE
                  SIZE DELIMITED BY SIZE
                  " bytes" DELIMITED BY SIZE
                  INTO BUFFER
           MOVE 50 TO COUNT
           CALL "sys_write_c" USING FD BUFFER COUNT GIVING RESULT
           CALL "sys_exit_c" USING 0.
           STOP RUN.
