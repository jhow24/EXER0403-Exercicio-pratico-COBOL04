      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0403.
       AUTHOR.     JOHNATHAN.
      *================================================================*
      *              C A P G E M I N I - S I S T E M A S               *
      *================================================================*
      *    PROGRAMA....: EXER0403
      *    PROGRAMADOR.: JOHNATHAN
      *    ANALISTA....: ARI BORGES                                *
      *    DATA........: 24/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO CSV COM DADOS DE NOME,
      *                    DATA ATULZ,QTD E ENDERECOS
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT04103
      *      ARQSAI01                                  SAI04103
      *      INFO_PSSOA                                CADUB069
      *      ENDER_PSSOA                               CADUB018
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT ARQENT01 ASSIGN      TO UT-S-ARQENT01
                      FILE STATUS      IS WRK-FS-ARQENT01.
.
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 10                 *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(10).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 117               *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(117).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0403 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC  X(008) VALUE 'EXER0403'.
       77 WRK-MASK-QTREG          PIC  ZZ.ZZ9.
       77 WRK-TABELA              PIC  X(010) VALUE SPACES.
       77 WRK-SQLCODE             PIC  -99999.
      *
       01 WRK-ACUMULADORES.
           03 ACU-LIDOS-ARQENT01  PIC  9(005) VALUE ZEROS.
           03 ACU-DESPREZADOS     PIC  9(005) VALUE ZEROS.
           03 ACU-ENDERECOS       PIC  S9(005) COMP-3 VALUE ZEROS.
           03 ACU-GRAVA-ARQSAI01  PIC  9(005) VALUE ZEROS.
      *
       01 WRK-CABEC.
           05 WRL-CABEC-ARQSAI01  PIC  X(037) VALUE
              'COD;NOME;DATA ATULZ;QTDE;ENDERECOS'.
      *
       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-CN-ARQENT01      VALUE 'ENT01113'.
          88 WRK-CN-ARQSAI01      VALUE 'SAI01113'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CN-OPEN          VALUE 'OPEN '.
          88 WRK-CN-CLOSE         VALUE 'CLOSE'.
          88 WRK-CN-READ          VALUE 'READ '.
          88 WRK-CN-WRITE         VALUE 'WRITE'.            
           
      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK               VALUE '00'.
             88 WRK-FS-ENT01-FIM              VALUE '10'.

      *
          05 WRK-FS-ARQSAI01         PIC  X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK                  VALUE '00'.

          05 WRK-FS-DISPLAY          PIC  X(002) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT04103.
           COPY SAI04103.

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
              'AREA PARA DB2'.
      *----------------------------------------------------------------*
           EXEC SQL 
              INCLUDE SQLCA
           END-EXEC.

      *    DB2PRD.INFO_PSSOA                                           *
           EXEC SQL
              INCLUDE CADUB069
           END-EXEC.

      *    DB2PRD.ENDER_PSSOA
           EXEC SQL
              INCLUDE CADUB018
           END-EXEC.

      *----------------------------------------------------------------*
       01 FILLER                PIC X(050) VALUE
              'ENT0403 - FIM DA AREA DE WORKING'.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *    
           CALL 'CKRS1000'
           CALL 'CKRS1050'

           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ENT01-FIM
      *
           PERFORM 9900-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *    
           SET WRK-CN-OPEN                    TO TRUE
           SET WRK-CN-ARQSAI01                TO TRUE

           OPEN INPUT ARQENT01
           IF NOT WRK-FS-ENT01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF
      * 
           SET WRK-CN-ARQSAI01                TO TRUE
           SET WRK-CN-OPEN                    TO TRUE

           OPEN OUTPUT ARQSAI01
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           PERFORM 3800-LER-ARQENT01
           
           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: VAZIO                               *'
              DISPLAY '* ARQUIVO: ENT04103                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01 '              *'
              DISPLAY '* 'WRK-PROGRAMA'  CANCELADO                    *'
              DISPLAY '************************************************'
              PERFORM 9900-FINALIZAR 
           END-IF 
           
           SET WRK-CN-WRITE                   TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC.

           IF NOT WRK-FS-SAI01-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF      

           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      * CONTROLE DE PROCESSAMENTO ATE O FIM DO ARQUIVO DE ENTRADA      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
           PERFORM 3100-SELECIONAR-CLIENTE
           
           IF SQLCODE EQUAL +0
              PERFORM 3900-GRAVAR-ARQSAI01
           END-IF

           PERFORM 3800-LER-ARQENT01
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ACESSA TABELA CLIENTES                                      *
      *----------------------------------------------------------------*
       3100-SELECIONAR-CLIENTE SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           MOVE ARQENT01-COD-CLI           TO CCLUB OF CADUB069

           EXEC SQL
              SELECT CSGL_UF, 
                     DINCL_REG
                   INTO :CADUB069.CSGL-UF,
                        :CADUB069.DINCL-REG
                   FROM  DB2PRD.INFO_PSSOA
                   WHERE CCLUB = :CADUB069.CCLUB
           END-EXEC

           EVALUATE SQLCODE
              WHEN ZEROS
                   MOVE ARQENT01-COD-CLI
                                      TO ARQSAI01-COD-CLI
                   MOVE CSGL-UF    OF CADUB069  TO ARQSAI01-COD-UF
                   MOVE DINCL-REG  OF CADUB069  TO ARQSAI01-DAT-INCL
                   MOVE HULT-ATULZ OF CADUB069  TO ARQSAI01-DATA-ATLZ
              WHEN +100
                   DISPLAY ARQENT01-COD-CLI ' - CLIENTE INXISTENTE'
                   ADD 1              TO ACU-DESPREZADOS
              WHEN OTHER 
                   MOVE 'INFO PESSOA' TO WRK-TABELA
                   MOVE SQLCODE       TO WRK-SQLCODE
                   DISPLAY '*******************************'
                   DISPLAY '*       ERRO ACESSO DB2       *'
                   DISPLAY '* TABELA : ' WRK-TABELA
                                                  '        *'
                   DISPLAY '* SQLCODE: ' WRK-SQLCODE
                                             '             *'
                   DISPLAY '* ' WRK-PROGRAMA
                                      ' CANCELADO          *'
                   DISPLAY '*******************************'

                   PERFORM 9900-FINALIZAR
           END-EVALUATE 
           
          
              PERFORM 3200-CONTA-ENDERECOS
           
           .
      *----------------------------------------------------------------*
       3100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    CONTAGEM DE ENDERECOS
      *----------------------------------------------------------------*
       3200-CONTA-ENDERECOS SECTION.
      *----------------------------------------------------------------*
           
           EVALUATE SQLCODE
                    WHEN +0
                       EXEC SQL
                          SELECT COUNT (*)
                                 INTO  ACU-ENDERECOS
                                 FROM  DB2PRD.ENDER_PSSOA
                                 WHERE CCLUB = :CADUB069.CCLUB
                        END-EXEC 
                       MOVE ACU-ENDERECOS  TO ARQSAI01-QTD-END
                    WHEN OTHER
                       DISPLAY 'ERRO NA OPERACAO'
           END-EVALUATE

           EVALUATE SQLCODE
              WHEN ZEROS
                   MOVE ACU-ENDERECOS  TO ARQSAI01-QTD-END
              WHEN OTHER 
                    MOVE 'ENDER_PSSOA'  TO WRK-TABELA
                    MOVE SQLCODE       TO WRK-SQLCODE
                    DISPLAY '*******************************'
                    DISPLAY '*       ERRO ACESSO DB2       *'
                    DISPLAY '* TABELA : ' WRK-TABELA
                                                  '        *'
                    DISPLAY '* SQLCODE: ' WRK-SQLCODE
                                             '             *'
                    DISPLAY '* ' WRK-PROGRAMA
                                      ' CANCELADO          *'
                    DISPLAY '*******************************'

                    PERFORM 9900-FINALIZAR
           END-EVALUATE

           .
      *----------------------------------------------------------------*
       3200-99-FIM.
           EXIT.

      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-ARQENT01 SECTION.
      *----------------------------------------------------------------*
           INITIALIZE ARQENT01-REGISTRO

           SET WRK-CN-READ                TO TRUE
           SET WRK-CN-ARQENT01            TO TRUE

           READ ARQENT01 INTO ARQENT01-REGISTRO.
      *
           IF  (WRK-FS-ARQENT01  EQUAL '00')
           OR  (WRK-FS-ARQENT01 EQUAL '10')
               IF WRK-FS-ARQENT01 EQUAL '00'
                 ADD 1 TO ACU-LIDOS-ARQENT01 
               ELSE 
                 NEXT SENTENCE  
           ELSE
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF.

      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3900-GRAVAR-ARQSAI01 SECTION.
      *----------------------------------------------------------------*
           

           SET WRK-CN-WRITE           TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           ADD 1                      TO ACU-GRAVA-ARQSAI01

           INITIALIZE                  ARQSAI01-REGISTRO 
           .
           
      *----------------------------------------------------------------*
       3900-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
       9100-ERROS-ARQUIVOS SECTION .
      *----------------------------------------------------------------*
           DISPLAY '*****************************'
           DISPLAY '*     ERRO COM ARQUIVOS     *'
           DISPLAY '* COMANDO: ' WRK-COMANDO
                                   '            *'
           DISPLAY '* ARQUIVO: ' WRK-ARQUIVO
                                      '         *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' CANCELADO        *'
           DISPLAY '*****************************'

           PERFORM 9900-FINALIZAR.
      *----------------------------------------------------------------*
       9100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
       9900-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           SET WRK-CN-CLOSE TO TRUE.

           CLOSE ARQENT01.
           IF NOT WRK-FS-ENT01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           CLOSE ARQSAI01.
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           DISPLAY '***************************************************'
           MOVE ACU-LIDOS-ARQENT01     TO WRK-mask-QTREG
           DISPLAY '* QTDE REGISTROS LIDOS   : ' WRK-MASK-QTREG
                                                    '                 *'
           MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK-QTREG
           DISPLAY '* QTDE REGISTROS GRAVADOS: ' WRK-MASK-QTREG
                                                    '                 *'
           MOVE ACU-DESPREZADOS        TO WRK-MASK-QTREG
           DISPLAY '* QTDE DESPREZADOS       : ' WRK-MASK-QTREG
                                                    '                 *'
           DISPLAY '*                                                 *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' FIM NORMAL                             *'
           DISPLAY '***************************************************'

             STOP RUN.
           
          END PROGRAM EXER0403.
      *----------------------------------------------------------------*