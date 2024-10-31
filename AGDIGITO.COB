      ****************************************************************
      * PROGRAM: CCAGENCI                                            *
      *                                                              *
      * DESCRIPTION: CALCULAR O DIGITO VERIFICADOR DO CODIGO DA      *
      *              AGENGIA BANCARIA DO BANCO FUTURE SCHOOL         *
      *                                                              *
      * AUTHOR: FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)             *
      *                                                              *
      * ENVIRONMENT: MAINFRAME IBM ZOS                               *
      *                                                              *
      * ENTRY PARAMETERS                                             *
      *   LKS-NUMERO-AGENCIA    NUMERO DA AGENCIA                    *
      *                                                              *
      * OUTPUT                                                       *
      *   WS-DIGITO-VERIFICADOR DIGITO VERIFICADOR DA AGENCIA        *
      *                                                              *
      * MODIFICATIONS                                                *
      *                                                              *
      *   DATE        AUTHOR          DESCRIPTION OF MODIFICATION    *
      *   ----------- --------------- ----------------------------   *
      *   2024-08-27  FABIO MARQUES   VERSAO INICIAL                 *
      *                                                              *
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCAGENCI.
       AUTHOR. FABIO MARQUES.
           INSTALLATION. MAINFRAME Z/OS.
           DATE-WRITTEN. 2024-08-27.
           DATE-COMPILED. 2024-08-27.
           SECURITY. CONFIDENTIAL.
       REMARKS. CALCULO DO DIGITO VERIFICADOR DA AGENCIA.
      *
      ************************* DATA DIVISION ************************
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01 WS-DV                 PIC 9     VALUE ZEROS.
       01 WS-DIGITO-VERIFICADOR PIC X     VALUE ZEROS.
       01 WS-SOMA               PIC 9(05) VALUE ZEROS.
       01 WS-RES                PIC 9(05) VALUE ZEROS.
       01 WS-RESTO              PIC 99    VALUE ZEROS.
      *
       01 WS-PESOS.
           05 WS-PESO-1         PIC 9  VALUE 5.
           05 WS-PESO-2         PIC 9  VALUE 4.
           05 WS-PESO-3         PIC 9  VALUE 3.
           05 WS-PESO-4         PIC 9  VALUE 2.
      *
       01 WS-DIGITOS-AGENCIA.
           05  WS-DIGITO-1      PIC 9.
           05  WS-DIGITO-2      PIC 9.
           05  WS-DIGITO-3      PIC 9.
           05  WS-DIGITO-4      PIC 9.
      *
       LINKAGE SECTION.
       01 LKS-PARM-IBM.
           05 LKS-PARAM-TAM     PIC S9(04) COMP.
           05 LKS-PARAM-GROUP.
               10 LKS-NUMERO    PIC 9(04).
      *
      ********************** PROCEDURE DIVISION *********************
      *
       PROCEDURE DIVISION USING LKS-PARM-IBM.
      *
      ** VALIDACAO
      *
       VALIDATION-LOGIC.
      *
           IF LKS-PARAM-TAM NOT = 4
               MOVE 100 TO RETURN-CODE
               PERFORM ROT-ABEND
           END-IF.
      *
           IF LKS-NUMERO NOT NUMERIC
               MOVE 200 TO RETURN-CODE
              PERFORM ROT-ABEND
           END-IF.
      *
      ** LOGICA PRINCIPAL
      *
       MAIN-LOGIC.
           MOVE LKS-NUMERO TO WS-DIGITOS-AGENCIA.
      *
           COMPUTE WS-SOMA = (   (WS-DIGITO-1 * WS-PESO-1)
                               + (WS-DIGITO-2 * WS-PESO-2)
                               + (WS-DIGITO-3 * WS-PESO-3)
                               + (WS-DIGITO-4 * WS-PESO-4) ).
      *
           DIVIDE WS-SOMA BY 11 GIVING WS-RES REMAINDER WS-RESTO.
      *
           IF WS-RESTO = 0 OR WS-RESTO = 1
               MOVE '0' TO WS-DIGITO-VERIFICADOR
           ELSE
               IF WS-RESTO = 10
                   MOVE 'X' TO WS-DIGITO-VERIFICADOR
               ELSE
                   COMPUTE WS-DV = 11 - WS-RESTO
                   MOVE WS-DV TO WS-DIGITO-VERIFICADOR
               END-IF
           END-IF.
      *
           DISPLAY "O DIGITO VERIFICADOR E: " WS-DIGITO-VERIFICADOR.
      *
           STOP RUN.
      *
      ** ABEND
      *
       ROT-ABEND.
      *
           DISPLAY '*******************************************'.
           DISPLAY '* NUMERO DE AGENCIA INVALIDO              *'.
           DISPLAY '* ' LKS-NUMERO.
           DISPLAY '*******************************************'.
           DISPLAY '* RETURN-CODE ' RETURN-CODE.
           DISPLAY '*******************************************'.
      *
           STOP RUN.
      *
       END PROGRAM CCAGENCI.                                          
