* CoreA System Call Interface (COBOL)
       * Defines system call numbers and structures
       01 SYSCALL-NUMBERS.
           05 SYS-WRITE        PIC 9(4) VALUE 1.
           05 SYS-EXIT         PIC 9(4) VALUE 2.
           05 SYS-FORK         PIC 9(4) VALUE 3.
           05 SYS-PIPE         PIC 9(4) VALUE 4.
           05 SYS-SEMAPHORE    PIC 9(4) VALUE 5.
           05 SYS-MUTEX        PIC 9(4) VALUE 6.
           05 SYS-SHM          PIC 9(4) VALUE 7.

       01 PIPE-FD.
           05 READ-FD          PIC 9(9).
           05 WRITE-FD         PIC 9(9).

       01 SEMAPHORE-DATA.
           05 SEM-ID           PIC 9(9).
           05 SEM-VALUE        PIC 9(9).

       01 MUTEX-DATA.
           05 MUTEX-ID         PIC 9(9).
           05 MUTEX-LOCK       PIC 9 VALUE 0.  * 1 for lock, 0 for unlock

       01 SHM-DATA.
           05 SHM-ADDR         USAGE IS POINTER.
           05 SHM-SIZE         PIC 9(9).
