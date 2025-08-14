       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA-CNPJS-LOTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-ENT ASSIGN TO "CNPJS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQ-SAI ASSIGN TO "CNPJS.REP"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-ENT.
       01 REG-ENTRADA           PIC X(30).

       FD ARQ-SAI.
       01 REG-SAIDA             PIC X(80).

       WORKING-STORAGE SECTION.

       01 WS-CNPJ               PIC X(14).
       01 WS-BASE               PIC X(12).
       01 WS-DV                 PIC 9(2).
       01 WS-DV1                PIC 9.
       01 WS-DV2                PIC 9.

       01 WS-PESOS1.
          88 WS-PESOS           PIC 99 OCCURS 12 TIMES VALUE
             5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2.

       01 WS-PESOS2.
          88 WS-PESOS           PIC 99 OCCURS 13 TIMES VALUE
             6, 5, 4, 3, 2, 9, 9, 7, 6, 5, 4, 3. 2.

       01 WS-SOMA               PIC 9(5) VALUE 0.
       01 WS-RESTO              PIC 99 VALUE 0.
       01 WS-I                  PIC 99 VALUE 0.
       01 WS-CHAR               PIC X.
       01 WS-ASCII              PIC 999 VALUE 0.
       01 WS-VALOR              PIC 99 VALUE 0.
       01 WS-STATUS             PIC X(10).

       PROCEDURE DIVISION.
       0001-INICIO.
           OPEN INPUT ARQ-ENT
                OUTPUT ARQ-SAI

           PERFORM UNTIL EOF-ENT
               READ ARQ-ENT
                   AT END MOVE "S" TO EOF-ENT
               NOT AT END
                   PERFORM 1000-PROCESSA-CNPJ
               END-READ
           END-PERFORM

           CLOSE ARQ-ENT ARQ-SAI
           STOP RUN.

       1000-PROCESSA-CNPJ.
      * Remove espaços
           MOVE FUNCTION TRIM(REG-ENTRADA) TO WS-CNPJ
      * Remove pontuação
           INSPECT WS-CNPJ REPLACING ALL "." BY ""
           INSPECT WS-CNPJ REPLACING ALL "/" BY ""
           INSPECT WS-CNPJ REPLACING ALL "-" BY ""

           IF FUNCTION LENGTH(WS-CNPJ) NOT = 14
               MOVE "INVALIDO" TO WS-STATUS
               PERFORM 9000-GRAVA-SAIDA
               EXIT PARAGRAPH
           END-IF

           MOVE WS-CNPJ(1:12) TO WS-BASE
           MOVE WS-CNPJ(13:2) TO WS-DV

      * CALCULA DV1
           MOVE 0 TO WS-SOMA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE WS-BASE(WS-I:1) TO WS-CHAR
               MOVE FUNCTION ORD(WS-CHAR) TO WS-ASCII
               SUBTRACT 48 FROM WS-ASCII GIVING WS-VALOR
               COMPUTE WS-SOMA = WS-SOMA + (WS-VALOR * WS-PESOS1(WS-I))
           END-PERFORM

           COMPUTE WS-RESTO = WS-SOMA MOD 11
           IF WS-RESTO < 2
               MOVE 0 TO WS-DV1
           ELSE
               COMPUTE WS-DV1 = 11 - WS-RESTO
           END-IF

      * CALCULA DV2
           MOVE 0 TO WS-SOMA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               IF WS-I < 13
                   MOVE WS-BASE(WS-I:1) TO WS-CHAR
               ELSE
                   MOVE FUNCTION NUMVAL(WS-DV1) TO WS-VALOR
                   ADD 48 TO WS-VALOR
                   MOVE FUNCTION CHAR(WS-VALOR) TO WS-CHAR
               END-IF

               MOVE FUNCTION ORD(WS-CHAR) TO WS-ASCII
               SUBTRACT 48 FROM WS-ASCII GIVING WS-VALOR

               COMPUTE WS-SOMA = WS-SOMA + (WS-VALOR * WS-PESOS2(WS-I))
           END-PERFORM

           COMPUTE WS-RESTO = WS-SOMA MOD 11
           IF WS-RESTO < 2
               MOVE 0 TO WS-DV2
           ELSE
               COMPUTE WS-DV2 = 11 - WS-RESTO
           END-IF

      * VALIDAÇÃO FINAL
           IF WS-DV = (WS-DV1 * 10 + WS-DV2)
               MOVE "VALIDO" TO WS-STATUS
           ELSE
               MOVE "INVALIDO" TO WS-STATUS
           END-IF

           PERFORM 9000-GRAVA-SAIDA.

       9000-GRAVA-SAIDA.
           STRING
               FUNCTION TRIM(REG-ENTRADA) DELIMITED BY SIZE
               "  =>  " DELIMITED BY SIZE
               WS-STATUS DELIMITED BY SIZE
               " (DV Calc: " DELIMITED BY SIZE
               WS-DV1 DELIMITED BY SIZE
               WS-DV2 DELIMITED BY SIZE
               ")" DELIMITED BY SIZE
               INTO REG-SAIDA
           END-STRING
           WRITE REG-SAIDA.

       WORKING-STORAGE SECTION.
       77 EOF-ENT PIC X VALUE "N".
