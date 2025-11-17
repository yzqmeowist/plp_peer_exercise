       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT-GENERATOR.

      *=========================
      *Compilation:
      *cobc -x report_generator.cbl
      *
      *Execution:
      *macOS:   ./report_generator
      *Windows: report_generator.exe
      *=========================

      *-------------------------
      *The ENVIRONMENT DIVISION is used to specify the physical
      *environment for the program. In our case, we use the INPUT-OUTPUT
      *SECTION and FILE-CONTROL in order to define the files we want
      *to read and write.
      *-------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *This tells COBOL which file to look for when referencing
      *AccountsFile (defined in the DATA DIVISION, under FILE SECTION).
       SELECT AccountsFile ASSIGN TO "accounts.dat"
      *        This tells COBOL that the file should be read
      *        line by line.
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
      * load transactions.dat
       SELECT TransactionsFile ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
      * create report.dat
       SELECT ReportFile ASSIGN TO "report.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *-------------------------
      *The DATA DIVISION defines all data that is going to be used
      *by the program (e.g. variables, constants, and file records).
      *In our case, the FILE SECTION will be used to define input/output
      *file records, while the WORKING-STORAGE SECTION will be used for
      *other variables.
      *-------------------------
       DATA DIVISION.
       FILE SECTION.
      *The FILE DESCRIPTION (FD) for the AccountsFile.
       FD AccountsFile
      *    A line (RECORD) contains 78 characters.
           RECORD CONTAINS 78 CHARACTERS.
       01 AccountRecord.
      *    The PICTURE clause (PIC) defines the type of the variable
      *    PIC 9(4):       A 4-digit integer.
      *    PIC 9(5)V99:    A 5-digit integer with a 2-digit decimal
      *                    value.
      *    PIC X(10):      A 10-character alphanumeric string.
           02 AccountId            PIC 9(4).
      *    Filler represents the space between each value.
           02 Filler               PIC X.
           02 AccountFirstName     PIC X(20).
           02 Filler               PIC X.
           02 AccountSecondName    PIC X(20).
           02 Filler               PIC X.
           02 AccountMoney         PIC X(15).
           02 Filler               PIC X.
           02 AccountLoan          PIC X(15).

      * use X(80) as the actions and amounts keep a variable length
       FD TransactionsFile
           RECORD CONTAINS 80 CHARACTERS.
           01 TransactionLine      PIC X(80).

       FD ReportFile
           RECORD CONTAINS 80 CHARACTERS.
           01 ReportLine           PIC X(200).

       WORKING-STORAGE SECTION.
      *Here, we define a TABLE, which is meant to hold the account
      *information for all accounts contained in "accounts.dat" (you
      *need to populate the table first by reading the file (see example
      *in the PROCEDURE DIVISION)).
       01 DATA-TABLE.
           05 DATA-Entry OCCURS 100 TIMES
               INDEXED BY TABLE-IDX.
               10 DT-ID            PIC X(4).
               10 DT-FIRST-NAME    PIC X(20).
               10 DT-SECOND-NAME   PIC X(20).
               10 DT-MONEY         PIC 9(15)V99.
               10 DT-LOAN          PIC 9(15)V99.

      *End-of-File flag
       01 EOF-DATA-FLAG           PIC X VALUE 'F'.
       01 EOF-TRANSACTIONS-FLAG   PIC X VALUE 'F'.

       01 WS-TRANSACTION-DETAILS.
           05 WS-TT-ID         PIC X(4).
           05 WS-TT-TYPE       PIC X(13).
           05 WS-TT-AMOUNT     PIC X(7).

       01 WS-CALCULATION-FIELDS.
           05 WS-AMOUNT-NUM    PIC 9(5)V99.

       01 WS-OLD-VALUES.
           05 WS-OLD-MONEY     PIC 9(15)V99.
           05 WS-OLD-LOAN      PIC 9(15)V99.

      * use Z to suppress leading zeros in display
       01 WS-DISPLAY.
           05 WS-DISPLAY-OLD-MONEY      PIC Z(4)9.99.
           05 WS-DISPLAY-NEW-MONEY      PIC Z(4)9.99.
           05 WS-DISPLAY-OLD-LOAN       PIC Z(4)9.99.
           05 WS-DISPLAY-NEW-LOAN       PIC Z(4)9.99.

       01 WS-REPORT-LINE          PIC X(200).
      *-------------------------
      *The PROCEDURE DIVISION holds the actual logic and instructions
      *for the program. The data we defined in the DATA DIVISION is
      *processed here.
      *-------------------------
       PROCEDURE DIVISION.
      *This is a paragraph. The first paragraph is always executed
      *automatically by COBOL. You can call upon other paragraphs you
      *define further below by using the PERFORM statement.
       Main-Logic.

           PERFORM Read-Accounts-File

           OPEN OUTPUT ReportFile
           PERFORM Process-Transactions
           CLOSE ReportFile

      *    If the STOP RUN statement wasn't here, the program would
      *    continue executing the next paragraph, until it reaches the
      *    end of the file, or a STOP RUN statement.
           STOP RUN.

      * read accounts from accounts.dat to data table
       Read-Accounts-File.
           OPEN INPUT AccountsFile
           MOVE 1 TO TABLE-IDX
           MOVE 'F' TO EOF-DATA-FLAG
           PERFORM UNTIL EOF-DATA-FLAG = 'T'
               READ AccountsFile INTO AccountRecord
                 AT END
                    MOVE 'T' TO EOF-DATA-FLAG
                 NOT AT END
                    MOVE AccountId        TO DT-ID(TABLE-IDX)
                    MOVE AccountFirstName TO DT-FIRST-NAME(TABLE-IDX)
                    MOVE AccountSecondName TO DT-SECOND-NAME(TABLE-IDX)
                    COMPUTE DT-MONEY(TABLE-IDX) =
                        FUNCTION NUMVAL(AccountMoney)
                    COMPUTE DT-LOAN(TABLE-IDX) =
                        FUNCTION NUMVAL(AccountLoan)
                    SET TABLE-IDX UP BY 1
               END-READ
           END-PERFORM
           CLOSE AccountsFile.

      * process transactions from transactions.dat
       Process-Transactions.
           OPEN INPUT TransactionsFile
           MOVE 'F' TO EOF-TRANSACTIONS-FLAG
           PERFORM UNTIL EOF-TRANSACTIONS-FLAG = 'T'
               READ TransactionsFile INTO TransactionLine
                 AT END
                    MOVE 'T' TO EOF-TRANSACTIONS-FLAG
                 NOT AT END
                    INITIALIZE WS-TRANSACTION-DETAILS
                    INITIALIZE WS-CALCULATION-FIELDS
      * extract transaction details
                    UNSTRING TransactionLine
                        DELIMITED BY ALL SPACES
                        INTO WS-TT-ID
                             WS-TT-TYPE
                             WS-TT-AMOUNT
                    END-UNSTRING
                    COMPUTE WS-AMOUNT-NUM =
                        FUNCTION NUMVAL(WS-TT-AMOUNT)
      * find account in data table
                    SET TABLE-IDX TO 1
                    SEARCH DATA-Entry
                      AT END
                        DISPLAY "Account ID " WS-TT-ID " not found."
                      WHEN DT-ID(TABLE-IDX) = WS-TT-ID
                          MOVE DT-MONEY(TABLE-IDX) TO WS-OLD-MONEY
                          MOVE DT-LOAN(TABLE-IDX) TO WS-OLD-LOAN
      * process transaction according to its type                    
                          EVALUATE WS-TT-TYPE
                            WHEN "ADD_AMOUNT"
                              ADD WS-AMOUNT-NUM 
                                  TO DT-MONEY(TABLE-IDX)
                            WHEN "REMOVE_AMOUNT"
                              SUBTRACT WS-AMOUNT-NUM 
                                  FROM DT-MONEY(TABLE-IDX)
                            WHEN "ADD_LOAN"
                              ADD WS-AMOUNT-NUM 
                                  TO DT-LOAN(TABLE-IDX)
                              ADD WS-AMOUNT-NUM 
                                  TO DT-MONEY(TABLE-IDX)
                            WHEN "REMOVE_LOAN"
                              SUBTRACT WS-AMOUNT-NUM 
                                  FROM DT-LOAN(TABLE-IDX)
                              SUBTRACT WS-AMOUNT-NUM 
                                  FROM DT-MONEY(TABLE-IDX)
                            WHEN OTHER
                              DISPLAY "Unknown transaction type" 
                          END-EVALUATE
                          PERFORM Write-Report
                    END-SEARCH
               END-READ
           END-PERFORM
           CLOSE TransactionsFile.

      * output with the required format
       Write-Report.
           MOVE WS-OLD-MONEY TO WS-DISPLAY-OLD-MONEY
           MOVE DT-MONEY(TABLE-IDX) TO WS-DISPLAY-NEW-MONEY
           MOVE WS-OLD-LOAN TO WS-DISPLAY-OLD-LOAN
           MOVE DT-LOAN(TABLE-IDX) TO WS-DISPLAY-NEW-LOAN

           INITIALIZE WS-REPORT-LINE
           STRING DT-ID(TABLE-IDX) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  DT-FIRST-NAME(TABLE-IDX) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  DT-SECOND-NAME(TABLE-IDX) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-DISPLAY-OLD-MONEY DELIMITED BY SIZE
                  " --> " DELIMITED BY SIZE
                  WS-DISPLAY-NEW-MONEY DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-DISPLAY-OLD-LOAN DELIMITED BY SIZE
                  " --> " DELIMITED BY SIZE
                  WS-DISPLAY-NEW-LOAN DELIMITED BY SIZE
                  INTO WS-REPORT-LINE
           END-STRING
           WRITE ReportLine FROM WS-REPORT-LINE.
