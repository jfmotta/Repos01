       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-DV-CNPJ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Entrada: 12 primeiros caracteres (sem DV)
       01  CNPJ-BASE         PIC X(12) VALUE "12A3456B0001".

      * Conversão de caracteres para número
       01  CNPJ-NUM          PIC 99 OCCURS 12 TIMES.

      * Pesos
       01  PESOS-DV1         PIC 99 VALUE ZEROS OCCURS 12 TIMES.
       01  PESOS-DV2         PIC 99 VALUE ZEROS OCCURS 13 TIMES.

      * Variáveis de cálculo
       01  SOMA              PIC 9(5) VALUE ZEROS.
       01  RESTO             PIC 99 VALUE ZEROS.
       01  DV1               PIC 9 VALUE ZEROS.
       01  DV2               PIC 9 VALUE ZEROS.
       01  I                 PIC 99 VALUE ZEROS.
       01  CHAR-VAL          PIC X VALUE SPACE.
       01  ASCII-VAL         PIC 999 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      * Define pesos para DV1
           MOVE 05 TO PESOS-DV1 (1)
           MOVE 04 TO PESOS-DV1 (2)
           MOVE 03 TO PESOS-DV1 (3)
           MOVE 02 TO PESOS-DV1 (4)
           MOVE 09 TO PESOS-DV1 (5)
           MOVE 08 TO PESOS-DV1 (6)
           MOVE 07 TO PESOS-DV1 (7)
           MOVE 06 TO PESOS-DV1 (8)
           MOVE 05 TO PESOS-DV1 (9)
           MOVE 04 TO PESOS-DV1 (10)
           MOVE 03 TO PESOS-DV1 (11)
           MOVE 02 TO PESOS-DV1 (12)

    * * Define pesos para DV2
           MOVE 06 TO PESOS-DV2 (1)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
               MOVE PESOS-DV1 (I) TO PESOS-DV2 (I + 1)
           END-PERFORM

      *     * Converte caracteres para números
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
               MOVE CNPJ-BASE (I:1) TO CHAR-VAL
               MOVE FUNCTION ORD (CHAR-VAL) TO ASCII-VAL
               IF CHAR-VAL IS NUMERIC
                   MOVE FUNCTION NUMVAL (CHAR-VAL) TO CNPJ-NUM (I)
               ELSE
                   COMPUTE CNPJ-NUM (I) = ASCII-VAL - 48
               END-IF
           END-PERFORM

      *     * Calcula DV1
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
               COMPUTE SOMA = SOMA + (CNPJ-NUM (I) * PESOS-DV1 (I))
           END-PERFORM
           COMPUTE RESTO = FUNCTION MOD (SOMA 11)
           IF RESTO < 2
               MOVE 0 TO DV1
           ELSE
               COMPUTE DV1 = 11 - RESTO
           END-IF

      *     * Calcula DV2
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
               COMPUTE SOMA = SOMA + (CNPJ-NUM (I) * PESOS-DV2 (I))
           END-PERFORM
           COMPUTE SOMA = SOMA + (DV1 * PESOS-DV2 (13))
           COMPUTE RESTO = FUNCTION MOD (SOMA 11)
           IF RESTO < 2
               MOVE 0 TO DV2
           ELSE
               COMPUTE DV2 = 11 - RESTO
           END-IF

           DISPLAY "CNPJ Base : " CNPJ-BASE
           DISPLAY "DV Calculado: " DV1 DV2

           STOP RUN.
