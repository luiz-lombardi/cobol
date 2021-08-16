      *----------------------------------------------------------------*
      *                                                                *
      *     SISTEMA      -->        S.E.F.                             *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *     PROGRAMA     -->        SEF001.CBL                         *
      *                                                                *
      *     OBJETIVO     -->        MENU DE FUNCOES ESCRITA FISCAL     *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *     ALTERACAO    -->        10/12/2000     POR: LUIZ           *
      *----------------------------------------------------------------*
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.         SEF001.
       AUTHOR.             AFFM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPARAM ASSIGN TO "CADPARM.SDI"
                  ORGANIZATION    IS RELATIVE
                  ACCESS  MODE       DYNAMIC
                  RELATIVE KEY       WK-PARAMCOD
                  FILE STATUS     IS WK-STATUS.
      *
           SELECT COMFUN  ASSIGN TO WK-COMFUN
                  ORGANIZATION   IS INDEXED
                  ACCESS MODE    IS DYNAMIC
                  LOCK MODE      IS AUTOMATIC
                  FILE STATUS       WK-STATUS
                  RECORD    KEY  IS CMFU-CHAVE.
      *
           SELECT DIREITO ASSIGN TO "DIREITO.SDI"
                  ORGANIZATION   IS INDEXED
                  ACCESS MODE    IS DYNAMIC
                  LOCK MODE      IS AUTOMATIC
                  FILE STATUS    IS WK-STATUS
                  RECORD    KEY  IS DIR-CHAVE.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ARQPARAM
           LABEL RECORDS ARE STANDARD.
       01  PARAM-LOJA.
           COPY    \FONTES\SDIC\SDIC4F01.CPY.
      *
       FD  COMFUN
           LABEL RECORD IS STANDARD.
       01  CMFU-REG.
           COPY    \FONTES\SDIC\SDICFF04.CPY.
      *
       FD  DIREITO
           LABEL RECORDS ARE STANDARD.
       01  DIR-REG.
           COPY    \FONTES\SDIC\SDICDIR.CPY.
      *
       WORKING-STORAGE SECTION.
       01 WK-CHAIN-DT.
          05        WK-CHAIN-DD     PIC 9(02).
          05        WK-CHAIN-MM     PIC 9(02).
          05        WK-CHAIN-SC     PIC 9(02).
          05        WK-CHAIN-AA     PIC 9(02).
       01 WK-CH-X.
          03 WK-CH-DT     PIC 9(08)  VALUE ZEROS.
          03 FILLER  REDEFINES  WK-CH-DT.
             05        WK-CH-SC     PIC 9(02).
             05        WK-CH-AA     PIC 9(02).
             05        WK-CH-MM     PIC 9(02).
             05        WK-CH-DD     PIC 9(02).
          03           FILLER          PIC X(13)  VALUE SPACES.
       01  WK-TELAOP               PIC X(01)     VALUE ZEROS.
       01  WK-CHAPA                PIC 9(05)     VALUE ZEROS.
       01  WK-SENHA                PIC 9(06)     VALUE ZEROS.
       01  WK-SUPERV               PIC 9(05)     VALUE ZEROS.
       01  WK-WORK.
           03  WK-PARAMCOD         PIC 9(05)     VALUE ZEROS.
           03  WK-SPACE            PIC X(78)     VALUE SPACES.
           03  WK-RESP             PIC X(01)     VALUE SPACES.
           03  WK-STATUS           PIC X(02)     VALUE SPACES.
           03  WK-SCAPE            PIC 9(02)     VALUE ZEROS.
           03  WK-LIN              PIC 9(02)     VALUE ZEROS.
           03  WK-COL1             PIC 9(02)     VALUE ZEROS.
           03  WK-COL2             PIC 9(02)     VALUE ZEROS.
           03  WK-CPO1             PIC X(80)     VALUE SPACES.
           03  WK-CPO2             PIC X(78)     VALUE SPACES.
           03  WK-AUX              PIC 9(02).
           03  WK-DTCAB            PIC 99/99/9999.
           03  WK-OPCAO            PIC 9(02)     VALUE ZEROS.
           03  WK-DATA             PIC 9(08)     VALUE ZEROS.
           03  FILLER              REDEFINES     WK-DATA.
               05  WK-DD           PIC 9(02).
               05  WK-MM           PIC 9(02).
               05  WK-SC           PIC 9(02).
               05  WK-AA           PIC 9(02).
      *
           03      WK-COMFUN.
              05   FILLER          PIC X(06) VALUE "COMFUN".
              05   WK-COMFUNLJ     PIC 9(02) VALUE ZEROS.
              05   FILLER          PIC X(04) VALUE ".SDI".
      *
       PROCEDURE DIVISION USING WK-CHAIN-DT WK-TELAOP WK-CHAPA
                                WK-SENHA    WK-SUPERV.
       P001.
           IF  WK-CHAIN-DT  NOT NUMERIC OR WK-CHAIN-DT = ZEROS
               MOVE  "0"  TO  WK-TELAOP
           END-IF.
           PERFORM P010 THRU P019.
           PERFORM P020 THRU P029.
           PERFORM P030 THRU P039.
       P001-FIM.
           MOVE    ALL  "Ä" TO WK-CPO2.
           DISPLAY (22, 02) WK-CPO2.
           DISPLAY (23, 02) WK-SPACE.
           IF      WK-TELAOP  =  "S"
                   DISPLAY (01, 01) ERASE
                   STOP    RUN
           ELSE
                   MOVE  ZEROS  TO  WK-TELAOP
                   EXIT  PROGRAM.
      *
       P010.
           MOVE ALL "Í" TO WK-CPO1.
           DISPLAY (01, 01) ERASE.
           DISPLAY (02, 02) WK-SPACE WITH
                                     BACKGROUND-COLOR IS 5
                                     FOREGROUND-COLOR IS 15
           DISPLAY (01, 01) WK-CPO1 WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (24, 01) WK-CPO1 WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           MOVE 01 TO WK-LIN.
           MOVE 80 TO WK-COL2.
           MOVE 01 TO WK-COL1.
       P011.
           DISPLAY (WK-LIN, WK-COL1) "º" WITH
                                    BACKGROUND-COLOR IS 1
                                    FOREGROUND-COLOR IS 15
           DISPLAY (WK-LIN, WK-COL2) "º" WITH
                                    BACKGROUND-COLOR IS 1
                                    FOREGROUND-COLOR IS 15
           ADD  1 TO WK-LIN.
           IF   WK-LIN NOT = 22
                GO TO P011.
LUIZ       DISPLAY (WK-LIN, 01) "Ç" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (WK-LIN, 80) "¶" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           ADD  1 TO WK-LIN.
           DISPLAY (WK-LIN, WK-COL1) "º" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (WK-LIN, WK-COL2) "º" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (01, 01) "É" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (24, 01) "È" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (01, 80) "»" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (02, 01) "º" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (02, 80) "º" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (03, 01) "Ç" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (03, 80) "¶" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (24, 80) "¼" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           MOVE ALL "Ä" TO WK-CPO2.
           DISPLAY (03, 02) WK-CPO2 WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (22, 02) WK-CPO2 WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
      *
           MOVE FUNCTION CURRENT-DATE TO WK-CH-X.
           MOVE WK-CH-DD TO WK-CHAIN-DD WK-DD.
           MOVE WK-CH-MM TO WK-CHAIN-MM WK-MM.
           MOVE WK-CH-SC TO WK-CHAIN-SC WK-SC.
           MOVE WK-CH-AA TO WK-CHAIN-AA WK-AA.
           MOVE WK-DATA  TO WK-DTCAB
                            WK-CHAIN-DT.
         DISPLAY (02, 03) "TendTudo - " WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
         DISPLAY (02, 14) WK-DTCAB WITH
                                   BACKGROUND-COLOR IS 5
                                   FOREGROUND-COLOR IS 15
         MOVE ALL "Ä" TO WK-CPO2.
         DISPLAY (22, 02) WK-CPO2 WITH
                                  BACKGROUND-COLOR IS 5
                                  FOREGROUND-COLOR IS 15.
         DISPLAY (22, 03) "Mensagens" WITH
                                      BACKGROUND-COLOR IS 5
                                      FOREGROUND-COLOR IS 15.
       P019.
           EXIT.
      *
      *---- ABERTURA DE ARQUIVOS
      *
       P020.
           DISPLAY (23, 02) WK-SPACE.
           DISPLAY (23, 03) "AGUARDE: INICIALIZANDO SISTEMA".
      *
           OPEN     INPUT     ARQPARAM.
           IF       WK-STATUS NOT  =   ZEROS
                    DISPLAY (23, 02) WK-SPACE
                    DISPLAY   (23, 03) "ERRO ABERTURA PARAMETROS  ST = "
                    WK-STATUS " -   TECLE <ENTER> "
                    ACCEPT (23, 75) WK-RESP
                    GO TO  P001-FIM.
           MOVE     1      TO   WK-PARAMCOD.
           READ    ARQPARAM    INVALID  KEY
                   DISPLAY (23, 02) WK-SPACE
                   DISPLAY (23, 03) "ERRO LEITURA PARAMETRO ST = "
                                    WK-STATUS "  -  TECLE <ENTER> "
                   ACCEPT (23, 75) WK-RESP
                   CLOSE  ARQPARAM
                   GO TO  P001-FIM.
           MOVE    PARAM-NR     TO  WK-COMFUNLJ.
           CLOSE ARQPARAM.
      *
       P020-FUNC.
           OPEN     INPUT     COMFUN.
           IF       WK-STATUS NOT  =   ZEROS
                    DISPLAY (23, 02) WK-SPACE
                    DISPLAY (23, 03) "ERRO ABERTURA COMFUN  ST = "
                                     WK-STATUS " -   TECLE <ENTER> "
                    ACCEPT (23, 75) WK-RESP
                    GO TO  P001-FIM.
      *
           INITIALIZE  CMFU-REG.
      *
           IF       WK-CHAPA  NUMERIC AND WK-CHAPA NOT = ZEROS
                    MOVE   WK-CHAPA  TO  CMFU-CHAPA
                    MOVE   WK-SENHA  TO  CMFU-SENHA
                    GO TO  P029.
      *
      *    CALCULO DA SENHA DE SUPERVISAO
      *
           COMPUTE WK-SUPERV = 11 * WK-DD * WK-MM * WK-COMFUNLJ.
      *
       P666-1.
           MOVE  ZEROS TO WK-LIN
           ADD 4       TO WK-LIN. 
       P666.
           DISPLAY (WK-LIN, 02) WK-SPACE WITH
                                    BACKGROUND-COLOR IS 1
                                    FOREGROUND-COLOR IS 15.
           ADD  1 TO WK-LIN.
           IF   WK-LIN < 22
                GO TO P666.
       P999.
           EXIT.

       P020-CHAPA.
           DISPLAY (05, 26) "***   ESCRITA FISCAL    ***".

           DISPLAY (23, 02) WK-SPACE WITH
                                     BACKGROUND-COLOR IS 5
                                     FOREGROUND-COLOR IS 15.
           DISPLAY (23, 02) "DIGITE (CHAPA/SENHA) -> <     /      >".
      *
           ACCEPT WK-SCAPE FROM ESCAPE KEY.
           IF   WK-SCAPE = 08
                STOP RUN.

           ACCEPT  (23, 27) CMFU-CHAPA WITH SECURE.
           IF      CMFU-CHAPA = ZEROS OR 99999 OR 99998 OR 99997
                   GO  TO  P020-CHAPA.
       P020-SENHA.
           ACCEPT  (23, 33) CMFU-SENHA WITH SECURE.
      *
           IF      CMFU-CHAPA      =    1234      AND
                   CMFU-SENHA      =    WK-SUPERV
                   CLOSE   COMFUN
                   GO TO   P029.
      *
           READ    COMFUN.
           IF WK-STATUS = "23"
              DISPLAY (23, 02) WK-SPACE
              DISPLAY (23, 02) "CHAPA/SENHA NAO ENCONTRADA - TECLE ALGO"
              ACCEPT  (23, 75) WK-RESP
              GO TO P020-CHAPA.
           IF WK-STATUS NOT = ZEROS
              DISPLAY (23, 02) WK-SPACE
              DISPLAY (23, 02) "ERRO LEITURA COMFUN - FS " WK-STATUS
                               " TECLE ALGO"
              ACCEPT  (23, 75) WK-RESP
              GO  TO  P001-FIM.
           CLOSE  COMFUN.
      *
       P020-DIREITO.
           OPEN     INPUT     DIREITO.
           IF       WK-STATUS NOT  =   ZEROS
                    DISPLAY (23, 02) WK-SPACE
                    DISPLAY (23, 03) "ERRO ABERTURA DIREITO.SDI ST = "
                                     WK-STATUS " -   TECLE <ENTER> "
                    ACCEPT (23, 75) WK-RESP
                    GO TO  P001-FIM.
           MOVE   CMFU-CHAPA  TO  DIR-CHAPA.
           MOVE   1           TO  DIR-SISTEMA.
           MOVE   7           TO  DIR-NIVEL1.
           MOVE   ZEROS       TO  DIR-NIVEL2.
           MOVE   ZEROS       TO  DIR-NIVEL3.
           MOVE   ZEROS       TO  DIR-NIVEL4.
           READ   DIREITO.
           IF     WK-STATUS NOT = "00"
                  DISPLAY  (23, 02) WK-SPACE
                  DISPLAY  (23, 03) "DIREITO NAO CADASTRADO. TECLE ALGO"
                  ACCEPT   (23, 78) WK-RESP
                  CLOSE    DIREITO
                  GO  TO   P001-FIM.
           IF     DIR-DIREITO  NOT = "S"
                  DISPLAY  (23, 02)  WK-SPACE
                  DISPLAY  (23, 03)  "ACESSO NAO PERMITIDO. TECLE ALGO"
                  ACCEPT   (23, 78)  WK-RESP
                  CLOSE    DIREITO
                  GO  TO   P001-FIM.
           CLOSE    DIREITO.
       P029.
           EXIT.
      *
      *---- MENU PRINCIPAL
      *
       P030.
           PERFORM P900 THRU P909.
           DISPLAY (05, 25) "01 - Inclusao do Movimento Diario".
           DISPLAY (07, 25) "02 - Manutencao de Mapas de PDV".
           DISPLAY (09, 25) "03 - Manutencao de Notas Fiscais".
           DISPLAY (11, 25) "04 - Relatorios".
           DISPLAY (13, 25) "05 - Geracao de arquivo SINTEGRA".
           DISPLAY (15, 25) "06 - Manutencao de Fornecedores".
           DISPLAY (17, 25) "07 - Manutencao de Clientes".
           DISPLAY (19, 25) "08 - Importa Clientes (Dados do Oracle)".
           DISPLAY (21, 50) "Opcao: <  >".
       P031.
           DISPLAY (23, 02) WK-SPACE.
           DISPLAY (23, 03) "F7 : MENU ANTERIOR".
      *
           ACCEPT (21, 58) WK-OPCAO.
           ACCEPT WK-SCAPE FROM ESCAPE KEY.
           IF   WK-SCAPE = 08
                GO TO P039.
           IF   WK-OPCAO = ZEROS OR > 8
                GO TO P031.
      *
           IF      CMFU-CHAPA      =    1234      AND
                   CMFU-SENHA      =    WK-SUPERV
                   GO TO    P032.
      *
      *    MOVE     CMFU-CHAPA   TO    DIR-CHAPA.
      *    MOVE     1            TO    DIR-SISTEMA.
      *    MOVE     7            TO    DIR-NIVEL1.
      *    MOVE     WK-OPCAO     TO    DIR-NIVEL2.
      *    MOVE     ZEROS        TO    DIR-NIVEL3.
      *    MOVE     ZEROS        TO    DIR-NIVEL4.
      *    PERFORM  LERDIR       THRU  LERDIR-FIM.
      *    IF       DIR-DIREITO  NOT = "S"
      *             GO  TO   P031.
      *
       P032.
           IF   WK-OPCAO = 1  AND  WK-COMFUNLJ NOT = 20
                CALL   "SEF010.EXE" USING WK-CHAIN-DT
                CANCEL "SEF010.EXE"
           ELSE
           IF   WK-OPCAO = 2  AND  WK-COMFUNLJ NOT = 20
                PERFORM P900 THRU P909
                DISPLAY (23, 02) WK-SPACE
                DISPLAY (23, 03) "F7 : MENU ANTERIOR"
                DISPLAY (05, 26) "***   ESCRITA FISCAL    ***"
                DISPLAY (07, 25) 
                "01 - Alteracao de informacoes de Mapas ja' existentes"
                DISPLAY (09, 25)
                "02 - Mudanca de numero de Mapas de PDV"
                DISPLAY (21, 50) "Opcao: <  >"
                ACCEPT (21, 58) WK-OPCAO
                ACCEPT WK-SCAPE FROM ESCAPE KEY
                IF   WK-SCAPE = 08
                     GO TO P039
                END-IF
             IF WK-OPCAO = 1
                CALL   "SEF012.EXE" USING WK-CHAIN-DT
                CANCEL "SEF012.EXE"
             ELSE
              IF WK-OPCAO = 2
                 CALL   "SEF014.EXE" USING WK-CHAIN-DT
                 CANCEL "SEF014.EXE"
              ELSE
                 MOVE  2   TO   WK-OPCAO
                 GO        TO   P032
              END-IF
             END-IF
           ELSE
           IF   WK-OPCAO = 3
                CALL   "SEF011.EXE" USING WK-CHAIN-DT
                CANCEL "SEF011.EXE"
           ELSE
           IF   WK-OPCAO = 4
                PERFORM P100 THRU P109
           ELSE
           IF   WK-OPCAO = 5
                CALL   "SEF147.EXE" USING WK-CHAIN-DT
                CANCEL "SEF147.EXE"
           ELSE
           IF   WK-OPCAO = 6
                CALL   "SEF013.EXE" USING WK-CHAIN-DT
                CANCEL "SEF013.EXE"
           ELSE
           IF   WK-OPCAO = 7
                CALL   "SEF017.EXE" USING WK-CHAIN-DT
                CANCEL "SEF017.EXE"
           ELSE
           IF   WK-OPCAO = 8
                CALL   "SEF777.EXE" USING WK-CHAIN-DT
                CANCEL "SEF777.EXE".
           GO  TO  P030.
       P039.
           EXIT.
      *
      *---- MENU DE RELATORIOS
      *
       P100.
           PERFORM P900  THRU  P909.
           DISPLAY (05, 26) "***   ESCRITA FISCAL    ***".
           DISPLAY (07, 15) "01 - Relatorio de Mapas de PDV".
           DISPLAY (08, 15) "02 - Relatorio de Entradas".
           DISPLAY (09, 15) "03 - Relatorio de Saidas".
           DISPLAY (10, 15) "04 - Relatorio de Apuracao de ICMS".
           DISPLAY (11, 15) "05 - Relatorio de Operacoes por UF".
           DISPLAY (12, 15) "06 - Relatorio de Codigos de Emitentes".
           DISPLAY (13, 15) "07 - Relatorio de Totais por CFO".
           DISPLAY (14, 15) "08 - Relatorio de Op. Interestaduais "
                            "(Trimestral)".
           DISPLAY (15, 15) "09 - Relatorio de Movimentacao de Notas".
           DISPLAY (16, 15) "10 - Demonstrativo p/ confeccao da DIPI".
           DISPLAY (17, 15) "11 - Resumo de Saidas Interestaduais c/ "
                            "Imposto Retido por ST".
           DISPLAY (21, 50) "Opcao: <  >".
       P101.
           DISPLAY (23, 02) WK-SPACE.
           DISPLAY (23, 03) "F7 : MENU ANTERIOR".
      *    
           ACCEPT (21, 58) WK-OPCAO.
           ACCEPT WK-SCAPE FROM ESCAPE KEY.
           IF   WK-SCAPE = 08
                GO TO P109.
           IF   WK-OPCAO = ZEROS OR > 11
                GO TO P101.
      *
      *     IF      CMFU-CHAPA      =    1234      AND
      *             CMFU-SENHA      =    WK-SUPERV
      *             GO TO    P102.
      *
      *     MOVE     WK-OPCAO     TO    DIR-NIVEL3.
      *     MOVE     ZEROS        TO    DIR-NIVEL4.
      *     PERFORM  LERDIR       THRU  LERDIR-FIM.
      *     IF       DIR-DIREITO  NOT = "S"
      *              GO  TO   P101.
      *
       P102.
           IF   WK-OPCAO = 1  AND  WK-COMFUNLJ NOT = 20
                CALL   "SEF148.EXE" USING WK-CHAIN-DT
                CANCEL "SEF148.EXE"
           ELSE
           IF   WK-OPCAO = 2
                CALL   "SEF143.EXE" USING WK-CHAIN-DT
                CANCEL "SEF143.EXE"
           ELSE
           IF   WK-OPCAO = 3
                CALL   "SEF144.EXE" USING WK-CHAIN-DT
                CANCEL "SEF144.EXE"
           ELSE
           IF   WK-OPCAO = 4
                CALL   "SEF041.EXE" USING WK-CHAIN-DT
                CANCEL "SEF041.EXE"
           ELSE
           IF   WK-OPCAO = 5
                CALL   "SEF048.EXE" USING WK-CHAIN-DT
                CANCEL "SEF048.EXE"
           ELSE
           IF   WK-OPCAO = 6
                CALL   "SEF145.EXE" USING WK-CHAIN-DT
                CANCEL "SEF145.EXE"
           ELSE
           IF   WK-OPCAO = 7
                CALL   "SEF146.EXE" USING WK-CHAIN-DT
                CANCEL "SEF146.EXE"
           ELSE
           IF   WK-OPCAO = 8
                CALL   "SEF047.EXE" USING WK-CHAIN-DT
                CANCEL "SEF047.EXE"
           ELSE
           IF   WK-OPCAO = 9
                CALL   "SEF149.EXE" USING WK-CHAIN-DT
                CANCEL "SEF149.EXE"
           ELSE
           IF   WK-OPCAO = 10  AND  PARAM-LJDP = "D"
                CALL   "SEF150.EXE" USING WK-CHAIN-DT
                CANCEL "SEF150.EXE"
           ELSE
           IF   WK-OPCAO = 11  AND  PARAM-LJDP = "D"
                CALL   "SEF151.EXE" USING WK-CHAIN-DT
                CANCEL "SEF151.EXE".
           GO  TO  P100.
       P109.
           EXIT.
      *
      *---- ROTINA PARA LIMPAR A TELA
      *
       P900.
           MOVE 4 TO WK-LIN.
           DISPLAY (02, 02) WK-SPACE WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15.
           DISPLAY (02, 03) "TendTudo - " WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15
           DISPLAY (02, 14) WK-DTCAB WITH
                                   BACKGROUND-COLOR IS 5
                                   FOREGROUND-COLOR IS 15
           DISPLAY (02, 64) " SEF001 - V01.5" WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15.
           DISPLAY (23, 02) WK-SPACE WITH
                                    BACKGROUND-COLOR IS 5
                                    FOREGROUND-COLOR IS 15.
       P901.
           DISPLAY (WK-LIN, 02) WK-SPACE WITH
                                    BACKGROUND-COLOR IS 1
                                    FOREGROUND-COLOR IS 15.
           ADD  1 TO WK-LIN.
           IF   WK-LIN < 22
                GO TO P901.
       P909.
           EXIT.
      *
      *---- ROTINA PARA LER ARQUIVO DE DIREITOS
      *
       LERDIR.
           MOVE   "N"   TO  DIR-DIREITO.
           OPEN   INPUT   DIREITO.
           IF     WK-STATUS  NOT = ZEROS
                  DISPLAY  (23, 02)  WK-SPACE
                  DISPLAY  (23, 03)  "ERRO ABERTURA DIREITO.SDI FS = "
                                     WK-STATUS
                  ACCEPT   (23, 78)  WK-RESP
                  GO  TO   LERDIR-FIM.
           READ   DIREITO.
           IF     WK-STATUS NOT = "00"
                  DISPLAY  (23, 02) WK-SPACE
                  DISPLAY  (23, 03) "DIREITO NAO CADASTRADO. TECLE ALGO"
                  ACCEPT   (23, 78) WK-RESP
                  GO  TO   LERDIR-FIM.
           IF     DIR-DIREITO  NOT = "S"
                  DISPLAY  (23, 02)  WK-SPACE
                  DISPLAY  (23, 03)  "ACESSO NAO PERMITIDO. TECLE ALGO"
                  ACCEPT   (23, 78)  WK-RESP.
       LERDIR-FIM.
           CLOSE  DIREITO.
           EXIT.
      *
      *---- FIM DO PROGRAMA
      *
