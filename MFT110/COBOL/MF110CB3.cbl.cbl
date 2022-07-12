       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MF110CB3.
       AUTHOR. HARIKASADI
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CLAIMNBR      PIC S9(9).
       01 WS-SQLCODE       PIC -9(03).
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
              INCLUDE CLAIMS
           END-EXEC.
       LINKAGE SECTION.
          COPY MF110BMS.
       PROCEDURE DIVISION USING MF110BMSI
                                MF110BMSO.
       C000-MAIN-PARA.
           EVALUATE SelectOptionfI
           WHEN '1'
               PERFORM 100-CLAIM-INQUIRY
           WHEN '2'
               PERFORM 200-CLAIM-ADD
           END-EVALUATE
           GOBACK.
       100-CLAIM-INQUIRY.
           MOVE INPUTI(4:7) TO WS-CLAIMNBR
           DISPLAY 'WS-CLAIMNBR : ' WS-CLAIMNBR
           MOVE WS-CLAIMNBR TO CLAIMNUMBER OF CLAIMS
            EXEC SQL
              SELECT CLAIMDATE,
                     PAID,
                     VALUE1,
                     CAUSE,
                     OBSERVATIONS
              INTO  :CLAIMS.CLAIMDATE,
                    :CLAIMS.PAID,
                    :CLAIMS.VALUE1,
                    :CLAIMS.CAUSE,
                    :CLAIMS.OBSERVATIONS
              FROM MFTR33.CLAIMS
              WHERE CLAIMNUMBER = :CLAIMS.CLAIMNUMBER
            END-EXEC.
           EVALUATE SQLCODE
            WHEN 0
              MOVE FUNCTION DISPLAY-OF(CLAIMDATE)  TO INPUT3O
              MOVE FUNCTION DISPLAY-OF(CAUSE) TO INPUT6O
             MOVE FUNCTION DISPLAY-OF(OBSERVATIONS) TO INPUT8O
              MOVE PAID of CLAIMS TO  INPUT4O
              MOVE VALUE1 OF CLAIMS TO INPUT5O
            WHEN 100
              MOVE 'CLAIM NOT FOUND' TO MESSAGEO
            WHEN OTHER
              MOVE SQLCODE       TO WS-SQLCODE
              STRING 'SQL ERROR IN FETCH - RC : ' WS-SQLCODE
              DELIMITED BY SIZE INTO MESSAGEO
              END-STRING
           END-EVALUATE.
       200-CLAIM-ADD.
              MOVE INPUTI(4:7) TO WS-CLAIMNBR
              MOVE WS-CLAIMNBR TO CLAIMNUMBER OF CLAIMS
              MOVE INPUT3I TO CLAIMDATE
              MOVE INPUT4I TO PAID         OF CLAIMS
              MOVE INPUT5I TO VALUE1       OF CLAIMS
              MOVE INPUT6I TO CAUSE        OF CLAIMS
              MOVE INPUT8I TO OBSERVATIONS OF CLAIMS
                EXEC SQL
                   INSERT INTO MFTR33.CLAIMS
                          (CLAIMNUMBER,
                           CLAIMDATE,
                           PAID,
                           VALUE1,
                           CAUSE,
                           OBSERVATIONS)
                   VALUES (:CLAIMS.CLAIMNUMBER,
                           :CLAIMS.CLAIMDATE,
                           :CLAIMS.PAID,
                           :CLAIMS.VALUE1,
                           :CLAIMS.CAUSE,
                           :CLAIMS.OBSERVATIONS)
                END-EXEC.
           EVALUATE SQLCODE
             WHEN 0
                 MOVE 'CLAIM ADDED' TO MESSAGEO
             WHEN -803
                 MOVE 'DUPLICATE CLAIM' TO MESSAGEO
             WHEN OTHER
                 MOVE SQLCODE       TO WS-SQLCODE
                 STRING 'SQL ERROR IN FETCH - RC : ' WS-SQLCODE
                 DELIMITED BY SIZE INTO MESSAGEO
                 END-STRING
                END-EVALUATE.
