       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2024-DAY-05.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "5.input"
                  LINE SEQUENTIAL.

      *> This might not be a great problem for cobol, but I'm getting
      *> nervous that future problems will be near-impossible to parse
      *> with cobol's file reading.
      *> This problem conveniently has all two-digit numbers.
      *> Cobol is probably the hardest language in this challenge.
      *> Hopefully part 2 isn't wicked.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 LINE-RECORD.
              05 PAGE-ORDERING-RULE.
                     10 PRE PIC 99.
                     10 FILLER PIC X.
                     10 SUC PIC 99.
                     10 FILLER PIC X(70).
              05 UPDATE-RECORD REDEFINES PAGE-ORDERING-RULE.
                     10 PAGE-NUM-CONTAINER OCCURS 25 TIMES.
                            15 PAGE-NUM PIC 99.
                            15 FILLER PIC X.

       WORKING-STORAGE SECTION.
       01 WS-LINE-RECORD.
              05 WS-PAGE-ORDERING-RULE.
                     10 WS-PRE PIC 99.
                     10 FILLER PIC X.
                     10 WS-SUC PIC 99.
                     10 FILLER PIC X(70).
              05 WS-UPDATE-RECORD REDEFINES WS-PAGE-ORDERING-RULE.
                     10 WS-PAGE-NUM-CONTAINER OCCURS 25 TIMES.
                            15 WS-PAGE-NUM PIC 99.
                            15 FILLER PIC X.
       01 WS-EOF PIC A.
      *>  WS-PROCESSING-DEPS: 'Y' when processing initial lines of the
      *> format '99|99' (ordering rules).
      *> Then 'N' when processing update records.
       01 WS-PROCESSING-DEPS PIC A VALUE "Y".
       01 WS-DEPS.
              05 WS-DEP OCCURS 99 TIMES.
      *>             10 WS-DEP-PRE PIC 99.
                     10 WS-DEP-SUC PIC 99 OCCURS 99 TIMES.
      *> Lengths of WS-DEP(WS-I)
       01 WS-DEPS-CNTS.
              05 WS-DEP-CNT PIC 99 OCCURS 99 TIMES.
       01 WS-I PIC 99.
       01 WS-J PIC 99.
       01 WS-K PIC 99.
       01 WS-L PIC 99.
       01 WS-N PIC 99.
       01 WS-SUM PIC 9(10).
       01 WS-SUM-2 PIC 9(10).
       01 WS-SUM-DISP PIC Z(9)9.
       01 WS-CORRECT PIC A.

       PROCEDURE DIVISION.
           OPEN INPUT INFILE.

           PERFORM UNTIL WS-EOF = 'Y'
              READ INFILE INTO WS-LINE-RECORD
                     AT END
                            MOVE 'Y' TO WS-EOF
                     NOT AT END
                            PERFORM PROCESS-LINE
           END-PERFORM

           MOVE WS-SUM TO WS-SUM-DISP.
           DISPLAY 'Part 1: ' WS-SUM-DISP.
           MOVE WS-SUM-2 TO WS-SUM-DISP.
           DISPLAY 'Part 2: ' WS-SUM-DISP.

           CLOSE INFILE.
           STOP RUN.

       PROCESS-LINE.
      *> Check for blank line when switching from dependencies to
      *> update-records.
           IF WS-PRE IS ZERO
                  MOVE 'N' TO WS-PROCESSING-DEPS
                  EXIT
           END-IF

           IF WS-PROCESSING-DEPS = 'Y'
                  PERFORM PROCESS-DEPENDENCY
           ELSE
                  PERFORM PROCESS-UPDATE-RECORD
           END-IF.

       PROCESS-DEPENDENCY.
           ADD 1 TO WS-DEP-CNT(WS-PRE).
           MOVE WS-SUC TO WS-DEP-SUC(WS-PRE, WS-DEP-CNT(WS-PRE)).

       PROCESS-UPDATE-RECORD.
           PERFORM VARYING WS-N FROM 1 BY 1
                  UNTIL WS-PAGE-NUM(WS-N) IS ZERO
      *> No statement here on purpose, just building up WS-N
           END-PERFORM

           MOVE 'Y' TO WS-CORRECT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = WS-N
              PERFORM VARYING WS-J FROM 1 BY 1
                            UNTIL WS-J > WS-DEP-CNT(WS-PAGE-NUM(WS-I))
                     PERFORM VARYING WS-K FROM 1 BY 1
                                   UNTIL WS-K = WS-I
                            IF WS-PAGE-NUM(WS-K) =
                               WS-DEP-SUC(WS-PAGE-NUM(WS-I), WS-J)
                                   MOVE 'N' TO WS-CORRECT
                            END-IF
                     END-PERFORM
              END-PERFORM
           END-PERFORM

           IF WS-CORRECT = 'Y'
              COMPUTE WS-I = WS-N / 2
              ADD WS-PAGE-NUM(WS-I) TO WS-SUM
           ELSE
              PERFORM TOPSORT
              COMPUTE WS-I = WS-N / 2
              ADD WS-PAGE-NUM(WS-I) TO WS-SUM-2
           END-IF.


      *> Ordering incorrect. Do topological sort.
       TOPSORT.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = WS-N
              PERFORM FIND-NODE-W-NO-PREREQS
      *> Swap WS-PAGE-NUM's elements at WS-I and WS-J
              MOVE WS-PAGE-NUM(WS-I) TO WS-K
              MOVE WS-PAGE-NUM(WS-J) TO WS-PAGE-NUM(WS-I)
              MOVE WS-K TO WS-PAGE-NUM(WS-J)
           END-PERFORM.

      *> Find the first element in WS-PAGE-NUM(WS-I..WS-N)
      *> that has no incoming edges (from that same subarray),
      *> and place its index in WS-J.
       FIND-NODE-W-NO-PREREQS.
           PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J = WS-N
              MOVE 'Y' TO WS-CORRECT
              PERFORM VARYING WS-K FROM WS-I BY 1 UNTIL WS-K = WS-N
                     PERFORM VARYING WS-L FROM 1 BY 1
                                   UNTIL WS-L >
                                         WS-DEP-CNT(WS-PAGE-NUM(WS-K))
                            IF WS-DEP-SUC(WS-PAGE-NUM(WS-K), WS-L) =
                               WS-PAGE-NUM(WS-J)
                                   MOVE 'N' TO WS-CORRECT
                            END-IF
                     END-PERFORM
              END-PERFORM

              IF WS-CORRECT = 'Y'
                 EXIT PERFORM
              END-IF
           END-PERFORM.

           IF WS-J = WS-N
              DISPLAY 'Found a cycle. Problem is impossible.'
              STOP RUN
           END-IF.
