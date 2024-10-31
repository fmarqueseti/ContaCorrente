      ****************************************************************
      * PROGRAM: CCDIGITO                                            *
      *                                                              *
      * DESCRIPTION: CALCULAR O DIGITO VERIFICADOR DO CODIGO DA      *
      *              CONTA BANCARIA DO BANCO FUTURE SCHOOL           *
      *                                                              *
      * AUTHOR: FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)             *
      *                                                              *
      * ENVIRONMENT: MAINFRAME IBM ZOS                               *
      *                                                              *
      * ENTRY PARAMETERS                                             *
      *   LKS-NUMERO-CONTA      NUMERO DA CONTA CORRENTE             *
      *                                                              *
      * OUTPUT                                                       *
      *   WS-DIGITO-VERIFICADOR DIGITO VERIFICADOR DA CONTA CORRENTE *
      *                                                              *
      * MODIFICATIONS                                                *
      *                                                              *
      *   DATE        AUTHOR          DESCRIPTION OF MODIFICATION    *
      *   ----------- --------------- ----------------------------   *
      *   2024-08-24  FABIO MARQUES   VERSAO INICIAL                 *
      *   2024-08-30  FABIO MARQUES   AJUSTE DOS PESOS DOS DIGITOS   *
      *                                                              *
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCDIGITO.
       AUTHOR. FABIO MARQUES.
           INSTALLATION. MAINFRAME Z/OS.
           DATE-WRITTEN. 2024-08-24.
           DATE-COMPILED. 2024-08-30.
           SECURITY. CONFIDENTIAL.
       REMARKS. CALCULO DO DIGITO VERIFICADOR DA CONTA CORRENTE.
      *
      ************************* DATA DIVISION ************************
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01 WS-DIGITO-VERIFICADOR PIC 9     VALUE ZEROS.
       01 WS-SOMA               PIC 9(05) VALUE ZEROS.
       01 WS-RES                PIC 9(05) VALUE ZEROS.
       01 WS-RESTO              PIC 9     VALUE ZEROS.
      *
       01 WS-PESOS.
           05 WS-PESO-1         PIC 99 VALUE 10.
           05 WS-PESO-2         PIC 9  VALUE 9.
           05 WS-PESO-3         PIC 9  VALUE 8.
           05 WS-PESO-4         PIC 9  VALUE 7.
           05 WS-PESO-5         PIC 9  VALUE 6.
           05 WS-PESO-6         PIC 9  VALUE 5.
           05 WS-PESO-7         PIC 9  VALUE 4.
           05 WS-PESO-8         PIC 9  VALUE 3.
           05 WS-PESO-9         PIC 9  VALUE 2.
      *
       01 WS-DIGITOS-CONTA.
           05  WS-DIGITO-1      PIC 9.
           05  WS-DIGITO-2      PIC 9.
           05  WS-DIGITO-3      PIC 9.
           05  WS-DIGITO-4      PIC 9.
           05  WS-DIGITO-5      PIC 9.
           05  WS-DIGITO-6      PIC 9.
           05  WS-DIGITO-7      PIC 9.
           05  WS-DIGITO-8      PIC 9.
           05  WS-DIGITO-9      PIC 9.
      *
       LINKAGE SECTION.
       01 LKS-PARM-IBM.
           05 LKS-PARAM-TAM     PIC S9(04) COMP.
           05 LKS-PARAM-GROUP.
               10 LKS-NUMERO    PIC 9(09).
      *
      ********************** PROCEDURE DIVISION *********************
      *
       PROCEDURE DIVISION USING LKS-PARM-IBM.
      *
      ** VALIDACAO
      *
       VALIDATION-LOGIC.
      *
           IF LKS-PARAM-TAM NOT = 9
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
           MOVE LKS-NUMERO TO WS-DIGITOS-CONTA.
      *
           COMPUTE WS-SOMA = (   (WS-DIGITO-1 * WS-PESO-1)
                               + (WS-DIGITO-2 * WS-PESO-2)
                               + (WS-DIGITO-3 * WS-PESO-3)
                               + (WS-DIGITO-4 * WS-PESO-4)
                               + (WS-DIGITO-5 * WS-PESO-5)
                               + (WS-DIGITO-6 * WS-PESO-6)
                               + (WS-DIGITO-7 * WS-PESO-7)
                               + (WS-DIGITO-8 * WS-PESO-8)
                               + (WS-DIGITO-9 * WS-PESO-9) ).
      *
           DIVIDE WS-SOMA BY 11 GIVING WS-RES REMAINDER WS-RESTO
      *
           IF WS-RESTO = 0 OR WS-RESTO = 1
               MOVE 0 TO WS-DIGITO-VERIFICADOR
           ELSE
               COMPUTE WS-DIGITO-VERIFICADOR = 11 - WS-RESTO
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
           DISPLAY '* NUMERO DE CONTA INVALIDO                *'.
           DISPLAY '* ' LKS-NUMERO.
           DISPLAY '*******************************************'.
           DISPLAY '* RETURN-CODE ' RETURN-CODE.
           DISPLAY '*******************************************'.
      *
           STOP RUN.
      *
       END PROGRAM CCDIGITO.                                          
