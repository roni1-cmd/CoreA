IDENTIFICATION DIVISION.
       PROGRAM-ID. CPU-BENCH.
      * CoreA CPU Benchmark (COBOL)
      * Measures CPU performance with arithmetic loop
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ITERATIONS PIC 9(7) VALUE 1000000.
       01 I PIC 9(7).
       01 RESULT PIC 9(18) VALUE 0.
       01 OUTPUT-BUF PIC X(50).
       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ITERATIONS
               COMPUTE RESULT = RESULT + (I * 2)
           END-PERFORM.
           MOVE "CPU Benchmark Result: " TO OUTPUT-BUF.
           STRING OUTPUT-BUF DELIMITED BY SIZE
                  RESULT DELIMITED BY SIZE
                  X"0A" DELIMITED BY SIZE
                  INTO OUTPUT-BUF.
           CALL "sys_write" USING 1 OUTPUT-BUF 50.
           CALL "sys_exit" USING 0.
           STOP RUN.
