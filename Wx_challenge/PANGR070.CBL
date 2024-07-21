       PROCESS             GRAPHIC                                              
       IDENTIFICATION      DIVISION.                                            
      *           
      *     蝔撘蝔  :  PANGR070                                           
      *           :  脩怠港誨亙格平蝮曄蝯梯銵                       
      *     Ｗ蝔  :  PANGR070D                                          
      *     雿輻冽獢  :  PPTBA08P(I)                                        
      *     蝔撘鈭箏  :  瞏甇蝢                                             
      *     潭交  :  86/12/14                                           
      *     瘜冽鈭  :  蝔撘嚗銝餅蝔 & DDS RECORD FORMAT NAME            
      *                  & PRTF-NAME 隞 CHANGE STRING 靽格嫣                
      *     靽格寞交  :    /  /                                             
      *     靽格孵批捆  :                                                     
      *           
       PROGRAM-ID.             PANGR070.                                        
      *           
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER.        IBM-AS400.                                       
       OBJECT-COMPUTER.        IBM-AS400.                                       
       SPECIAL-NAMES.   Sysin IS Sysin-AREA.                          
      *                                                                         
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT DSPF ASSIGN   TO WORKSTATION-PANGR070D-SI                     
                  ORGANIZATION  IS TRANSACTION                                  
                  ACCESS  MODE  IS DYNAMIC                                      
                  RELATIVE KEY  IS RELATIVE-KEY-VALUE                           
                  CONTROL-AREA  IS WK-CONTROL-AREA                              
                  FILE STATUS   IS DSPF-FILE-STATUS.                            
      *                                                                         
      *           
      *   閮剖拙撖砍漲 PRTF , 喳梯”撖阡撖砍漲 <= PRTF 撖砍漲                 
      *           
           SELECT PRTF ASSIGN   TO PRINTER-PANG66120                            
                  FILE STATUS   IS PRTF-FILE-STATUS.                            
      *                                                                         
++++++     SELECT PPTBA08P ASSIGN TO DATABASE-PPTBA08P                          
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS PPTBA08P-FILE-STATUS.                       
      *                                                                         
           SELECT SORTF ASSIGN TO DISK-SORTF.                                   
      *           
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
      *                                                                         
       FD  DSPF LABEL RECORD STANDARD.                                          
       01  DSPF-REC                            PIC X(1920).                     
      *                                                                         
      *           
      *    SELECT CLAUSE 閮剖                                             
      *           
       FD  PRTF                                                                 
           LABEL RECORD ARE STANDARD.                                           
       01  PRTF-REC                            PIC X(120).                      
      *                                                                         
++++++ FD  PPTBA08P LABEL RECORD STANDARD.                                      
  .    01  PPTBA08P-REC.                                                        
++++++     COPY DDS-ALL-FORMATS OF PPTBA08P.                                    
      *                                                                         
       SD  SORTF.                                                               
       01  SORTF-REC.                                                           
           05  SORTF-KEY.                                                       
      * 砍桐璆剖∩誨                                                        
               10  SORTF-010           PIC X(05).                               
      * 鈭斗交                                                                
               10  SORTF-020           PIC 9(08).                               
      * 隞   1. 隞亙    2. 鋡思誨                                         
               10  SORTF-030           PIC X(01).                               
      *  璆剖∪                                                             
           05  SORTF-100               PIC X(012).                              
      *  啣桐臭誨                                                         
           05  SORTF-210               PIC X(004).                              
      *  砍詨蝔                                                             
           05  SORTF-220               PIC X(010).                              
      *  啣桐璆剖∩誨                                                       
           05  SORTF-230               PIC X(005).                              
      *  璆剖∪                                                             
           05  SORTF-240               PIC X(012).                              
      *  隞交平蝮                                                               
           05  SORTF-310               PIC S9(12).                              
      *                                                                     
           05  SORTF-320               PIC S9(12).                              
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
           COPY    SELDA               OF      SEBOOK.                          
           COPY PANGSTDWRK OF PAULCBL.                                          
      *                                                                         
       01  DSPF-DDS-FORMAT.                                                     
           COPY DDS-HEADING-O          OF PANGR070D.                            
           COPY DDS-RANGEAREA-I        OF PANGR070D.                            
           COPY DDS-RANGEAREA-O        OF PANGR070D.                            
           COPY DDS-MSGAREA-O          OF PANGR070D.                            
           COPY DDS-ALL-FORMATS-INDIC  OF PANGR070D.                            
      *                                                                         
       01  FILE-STATUS.                                                         
           05  DSPF-FILE-STATUS                PIC X(002).                      
               88  DSPF-NORMAL                     VALUE "00" "02".             
               88  DSPF-AT-END                     VALUE "10".                  
               88  DSPF-NOT-FOUND                  VALUE "23".                  
               88  DSPF-RECORD-LOCK                VALUE "9D".                  
      *                                                                         
           05  PRTF-FILE-STATUS                PIC X(002).                      
               88  PRTF-NORMAL                     VALUE "00".                  
      *                                                                         
++++++     05  PPTBA08P-FILE-STATUS             PIC X(02).                      
  .            88  PPTBA08P-NORMAL                  VALUE "00" "02".            
  .            88  PPTBA08P-AT-END                  VALUE "10".                 
  .            88  PPTBA08P-NOT-FOUND               VALUE "23".                 
++++++         88  PPTBA08P-RECORD-LOCK             VALUE "9D".                 
      *                                                                         
++++++     05  PPRLT0L2-FILE-STATUS             PIC X(02).                      
  .            88  PPRLT0L2-NORMAL                  VALUE "00" "02".            
  .            88  PPRLT0L2-AT-END                  VALUE "10".                 
  .            88  PPRLT0L2-NOT-FOUND               VALUE "23".                 
++++++         88  PPRLT0L2-RECORD-LOCK             VALUE "9D".                 
      *                                                                         
++++++     05  SORTF-FILE-STATUS               PIC X(02).                       
  .            88  SORTF-NORMAL                    VALUE "00".                  
  .            88  SORTF-AT-END                    VALUE "10".                  
      *                                                                         
       01  PRIVATE-WORK-AREA.                                                   
      *           
      *   閮剖瘨梯”  U S R D T A  批捆                               
      *           
           05  OVRPRTF-USRDTA-CONTENT.                                          
               10  FILLER              PIC X(044) VALUE ,                       
                   "OVRPRTF FILE(PANG66120) USRDTA(' 脩怎 ')".              
               10  FILLER              PIC X(056) VALUE SPACES.                 
           05  DELETE-OVRPRTF.                                                  
               10  FILLER              PIC X(011) VALUE "DLTOVR *ALL".          
               10  FILLER              PIC X(089) VALUE SPACES.                 
           05  DELETE-OVRDBF.                                                   
               10  FILLER              PIC X(027) VALUE                         
                   "DLTOVR FILE(*ALL) LVL(*JOB)".                               
               10  FILLER              PIC X(073) VALUE SPACES.                 
           05  CALL-QCMDEXC-LENGTH     PIC 9(010)V9(005) COMP-3                 
                                                         VALUE 100.             
      *           
      *   怠                                                            
      *           
           05  BREAK-KEY.                                                       
               10  BREAK-SORTF-010             PIC X(05) VALUE SPACES.          
           05  WK-COUNT                        PIC 9(12) VALUE ZEROS.           
           05  S0.                                                              
               10  S1                          PIC S9(12) VALUE ZEROS.          
      *                                                                         
       01  CALL-SECHKDATE-ARGUMENT.                                             
           05  SECHKDATE-DATE                    PIC X(08).                     
           05  SECHKDATE-MSG-ID                  PIC X(07).                     
      *           
      *   梯”澆閮剖 - HEADING AREA                                     
      *           
       01  WP-HEAD-AREA.                                                        
           05  WP-HEAD01.                                                       
               10  FILLER              PIC X(012) VALUE " 霅隞嚗 ".         
      *        10  WP-BROKER-ID        PIC X(003) VALUE SPACES.                 
      *        10  FILLER              PIC X(001) VALUE "-".                    
               10  WP-BRANCH-CODE      PIC X(004) VALUE SPACES.                 
               10  WP-BRANCH-NAME      PIC X(010) VALUE SPACES.                 
      *  銝  FILLER  瑕漲  = ((' 撖血祝 ' - 24) / 2) - 26                       
               10  FILLER              PIC X(017) VALUE SPACES.                 
               10  WP-BROKER-NAME      PIC X(024) VALUE SPACES.                 
      *  銝  FILLER  瑕漲  = ((' 撖血祝 ' - 24) / 2) - 21                       
               10  FILLER              PIC X(022) VALUE SPACES.                 
               10  FILLER              PIC X(012) VALUE " 唳交嚗 ".         
               10  WP-SYS-DATE         PIC XX/XX/XX.                            
               10  FILLER              PIC X(001) VALUE SPACES.                 
           05  WP-HEAD02.                                                       
               10  FILLER              PIC X(012) VALUE " 蝔撘隞嚗 ".         
               10  WP-PROGRAM-CODE     PIC X(010) VALUE SPACES.                 
      *  銝  FILLER  瑕漲  = (' 梯”撖阡撖砍漲 ' / 2 ) - 38                     
               10  FILLER              PIC X(017) VALUE SPACES.                 
               10  WP-PROGRAM-NAME     PIC X(032) VALUE SPACES.                 
      *  銝  FILLER  瑕漲  = (' 梯”撖阡撖砍漲 ' / 2 ) - 37                     
               10  FILLER              PIC X(018) VALUE SPACES.                 
               10  FILLER              PIC X(012) VALUE " 唳嚗 ".         
               10  WP-SYS-TIME-H       PIC 9(002).                              
               10  FILLER              PIC X(001) VALUE ":".                    
               10  WP-SYS-TIME-M       PIC 9(002).                              
               10  FILLER              PIC X(001) VALUE ":".                    
               10  WP-SYS-TIME-S       PIC 9(002).                              
               10  FILLER              PIC X(001) VALUE SPACES.                 
      *         唳隞嗡銵券 , 臬閮剖                                     
      *         蝮賢祝摨阡  =  梯”撖阡撖砍漲                                       
           05  WP-HEAD03.                                                       
               10  FILLER              PIC X(012) VALUE SPACES.                 
               10  FILLER              PIC X(086) VALUE SPACES.                 
               10  FILLER              PIC X(004) VALUE " 蝚 ".                 
               10  WP-PAGE-CNT         PIC Z(004) VALUE ZEROS.                  
               10  FILLER              PIC X(004) VALUE "  ".                 
           05  WP-HEAD04.                                                       
               10  FILLER              PIC X(020) VALUE                         
                                       "       璆       ".                  
               10  FILLER              PIC X(010) VALUE                         
                                       " 鈭斗交 ".                            
               10  FILLER              PIC X(010) VALUE                         
                                       " 隞亥牧 ".                            
               10  FILLER              PIC X(038) VALUE                         
                " 啣啣臭誨蝔        璆       ".                       
               10  FILLER              PIC X(016) VALUE                         
                                       " 隞亙桀喟絞璆剔蜀 ".                      
               10  FILLER              PIC X(016) VALUE                         
                                       " 貊潭 ".                      
           05  WP-HEAD05.                                                       
               10  FILLER              PIC X(020) VALUE                         
                                       " ================== ".                  
               10  FILLER              PIC X(010) VALUE                         
                                       " ======== ".                            
               10  FILLER              PIC X(010) VALUE                         
                                       " ======== ".                            
               10  FILLER              PIC X(038) VALUE                         
                " ==================================== ".                       
               10  FILLER              PIC X(016) VALUE                         
                                       " ============== ".                      
               10  FILLER              PIC X(016) VALUE                         
                                       " ============== ".                      
      *           
      *   梯”澆閮剖 - BODY AREA                                        
      *           
       01  WP-BODY-AREA.                                                        
           05  WP-BODY01.                                                       
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-010           PIC X(005) VALUE SPACES.              
               10  FILLER                 PIC X(002) VALUE SPACES.              
               10  WP-SORTF-100           PIC X(012) VALUE SPACES.              
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-020           PIC Z(008) VALUE ZEROS.               
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-030           PIC X(010) VALUE ZEROS.               
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-210           PIC X(004) VALUE SPACES.              
               10  FILLER                 PIC X(002) VALUE SPACES.              
               10  WP-SORTF-220           PIC X(010) VALUE SPACES.              
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-230           PIC X(005) VALUE SPACES.              
               10  FILLER                 PIC X(003) VALUE SPACES.              
               10  WP-SORTF-240           PIC X(012) VALUE SPACES.              
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-SORTF-310           PIC ZZ,ZZZ,ZZZ,ZZZ                    
                                              VALUE ZEROS.                      
               10  FILLER                 PIC X(002) VALUE SPACES.              
               10  WP-SORTF-320           PIC --,---,---,---                    
                                              VALUE ZEROS.                      
      *           
      *   梯”澆閮剖 - TOTAL AREA                                       
      *           
       01  WP-TOTAL-ARAE.                                                       
           05  WP-SUB-TOTAL01.                                                  
               10  FILLER                 PIC X(084) VALUE SPACES.              
               10  FILLER                 PIC X(010) VALUE                      
                   " 撠閮 ".                                                
               10  FILLER                 PIC X(001) VALUE SPACES.              
               10  WP-S1                  PIC --,---,---,---                    
                                              VALUE ZEROS.                      
           05  WP-FINAL-TOTAL01.                                                
               10  FILLER                 PIC X(010) VALUE                      
                   "     閮 ".                                                
               10  FILLER                 PIC X(125) VALUE SPACES.              
      *           
      *   梯”澆閮剖 - BREAK KEY LINE TYPE                              
      *           
       01  WP-BREAK-LINE.                                                       
           05  WP-SUB-BREAK-LINE.                                               
               10  FILLER                 PIC X(120) VALUE ALL "-".             
           05  WP-FINAL-BOTTOM-LINE.                                            
               10  FILLER                 PIC X(120) VALUE ALL "=".             
      *           
      *   梯”澆閮剖 - FOOT AREA                                        
      *           
       01  WP-FOOT-AREA.                                                        
           05  WP-FOOT01.                                                       
               10  FILLER                 PIC X(021) VALUE SPACES.              
               10  FILLER                 PIC X(008) VALUE " 蝬嚗 ".          
               10  FILLER                 PIC X(025) VALUE SPACES.              
               10  FILLER                 PIC X(008) VALUE " 閬賂 ".          
               10  FILLER                 PIC X(025) VALUE SPACES.              
               10  FILLER                 PIC X(008) VALUE " 鋆質”嚗 ".          
               10  FILLER                 PIC X(025) VALUE SPACES.              
      *                                                                         
       01  CALL-SEREDSA0B-ARGUMENT.                                             
           05  SEREDSA0B-STATUS                  PIC X(02).                     
  .        88  SESA0P-NORMAL                     VALUE "00".                    
           COPY  DDS-ALL-FORMATS OF SESA0P.                                     
      *                                                                         
       01  CALL-PANGCL170-ARGUMENT.                                             
           05  PANGCL170-FILE-NAME            PIC X(10).                        
           05  PANGCL170-LIBRARY-NAME         PIC X(10).                        
           05  PANGCL170-TOFILE-NAME          PIC X(10).                        
      *                                                                         
           COPY PANGSTDLNK OF PAULCBL.                                          
      *           
       PROCEDURE DIVISION USING LINK-ARGUMENT.                                  
           COPY PANGSTDDCL OF PAULCBL.                                          
      *                                                                         
       MAIN-RTN.                                                                
           PERFORM 1000-INIT-RTN.                                               
           IF  PROCESS-NORMAL                                                   
               PERFORM 2000-PROCESS-RTN UNTIL END-PROGRAM                       
           END-IF.                                                              
      *                                                                         
       EXIT-PROGRAM.                                                            
           INITIALIZE CALL-SEREDSA0B-ARGUMENT.                                  
           MOVE "CL" TO SEREDSA0B-STATUS.                                       
           CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD.               
                                                                                
           MOVE 100 TO CALL-QCMDEXC-LENGTH.                                     
           CALL "QCMDEXC" USING DELETE-OVRPRTF CALL-QCMDEXC-LENGTH.             
           CALL "QCMDEXC" USING DELETE-OVRDBF CALL-QCMDEXC-LENGTH.              
++++++     CLOSE PPTBA08P.                                                      
           GOBACK.                                                              
      *                                                                         
       1000-INIT-RTN.                                                           
           ACCEPT LDA-REC FROM Sysin-AREA.                                 
                                                                                
           INITIALIZE CALL-SEREDSA0B-ARGUMENT.                                  
           MOVE "CL" TO SEREDSA0B-STATUS.                                       
           CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD.               
                                                                                
           INITIALIZE CALL-PANGCL170-ARGUMENT.                                  
           MOVE LDA-CURLIB999 TO PANGCL170-LIBRARY-NAME.                        
           MOVE "SESA0L    "  TO PANGCL170-FILE-NAME                            
                                 PANGCL170-TOFILE-NAME.                         
           CALL "PANGCL170" USING CALL-PANGCL170-ARGUMENT.                      
                                                                                
           MOVE "Y" TO PROCESS-SW.                                              
           IF  PROCESS-NORMAL                                                   
               OPEN I-O DSPF                                                    
               IF  NOT DSPF-NORMAL                                              
                   MOVE "E" TO PROCESS-SW                                       
                   MOVE "FIO0010"        TO LINK-MSG-CODE                       
                   MOVE "DSPF      "     TO LINK-MSG-FILE-NAME                  
                   MOVE DSPF-FILE-STATUS TO LINK-MSG-FILE-STATUS                
               END-IF                                                           
           END-IF.                                                              
++++++     IF  PROCESS-NORMAL                                                   
  .            OPEN INPUT PPTBA08P                                              
  .            IF  NOT PPTBA08P-NORMAL                                          
  .                MOVE "E" TO PROCESS-SW                                       
  .                MOVE "FIO0010"           TO LINK-MSG-CODE                    
  .                MOVE "PPTBA08P   "        TO LINK-MSG-FILE-NAME              
  .                MOVE PPTBA08P-FILE-STATUS TO LINK-MSG-FILE-STATUS            
  .            END-IF                                                           
++++++     END-IF.                                                              
           IF  PROCESS-NORMAL                                                   
               INITIALIZE PRIVATE-WORK-AREA CALL-PANGCL040-ARGUMENT             
                          DSPF-DDS-FORMAT                                       
      *           
      *   閮剖詨桀蝔梧蝔撘隞嚗隤芣嚗撌乩啣銋璅蝷                      
      *           
               MOVE LINK-TITLE-TEXT          TO TITLE1 OF HEADING-O             
               MOVE LINK-PROGRAM-NAME        TO TITLE2 OF HEADING-O             
               MOVE LINK-PROGRAM-CODE        TO PGMCDE OF HEADING-O             
               MOVE LINK-WORK-ENVIRONMENT    TO WRKENV OF HEADING-O             
      *           
      *   閮剖梯” BODY AREA 憭扯                                     
      *            PAGE COUNTER                                         
      *            BODY AREA LINE COUNTER                               
      *            OVERRIDE PRTF  USRDTA 批捆                             
      *           
               MOVE 54  TO BODY-AREA-MAX-LINE                                   
               MOVE  1  TO PAGE-CNT                                             
               MOVE  90 TO BODY-AREA-LINE-CNT                                   
               MOVE 100 TO CALL-QCMDEXC-LENGTH                                  
               CALL "QCMDEXC" USING OVRPRTF-USRDTA-CONTENT                      
                                    CALL-QCMDEXC-LENGTH                         
               CALL "PANGSUB500" USING CALL-PANGSUB500-ARGUMENT                 
           END-IF.                                                              
      *                                                                         
       2000-PROCESS-RTN.                                                        
           MOVE "P" TO PROCESS-MODE-SW.                                         
           MOVE "N" TO PROCESS-SW.                                              
      *           
      *     PRINT-MODE 銝 DISPLAY FILE I/O LOOP                         
      *         1. END-PROGRAM       蝯蝔撘                               
      *         2. PROCESS-NORMAL    蝜潛                               
      *           
           PERFORM  UNTIL END-PROGRAM       OR ,                                
                          PROCESS-NORMAL                                        
             PERFORM 4000-DISPLAY-ACCEPT-SCREEN                                 
             PERFORM 5000-CHECK-FUNCTION-KEY                                    
           END-PERFORM.                                                         
      *           
      *    START PF                                                     
      *           
           IF  NOT END-PROGRAM                                                  
               MOVE "Y" TO PROCESS-SW                                           
           END-IF.                                                              
      *                                                                         
       4000-DISPLAY-ACCEPT-SCREEN.                                              
      *           
      *   瑕閮航牧                                                      
      *           
           IF  PANGCL040-MESSAGE-CODE-O NOT = SPACES                            
               CALL "PANGCL040" USING CALL-PANGCL040-ARGUMENT                   
               CANCEL "PANGCL040"                                               
               MOVE PANGCL040-MESSAGE-TEXT-I TO MESSAGES OF MSGAREA-O           
               MOVE INDIC-ON  TO IN78 OF MSGAREA-O-INDIC                        
               INITIALIZE CALL-PANGCL040-ARGUMENT                               
                          LINK-MSG-CODE                                         
                          LINK-MSG-FILE-NAME                                    
                          LINK-MSG-FILE-STATUS                                  
           ELSE                                                                 
               MOVE SPACES                    TO MESSAGES OF MSGAREA-O          
               MOVE INDIC-OFF TO IN78 OF MSGAREA-O-INDIC                        
           END-IF.                                                              
      *           
      *   閮剖賣蝷                                                      
      *           
           MOVE PRINT-MODE-TEXT    TO WRKMOD OF HEADING-O.                      
      *           
      *   憿舐內 HEADING FORMAT PROMPT                                        
      *           
           WRITE DSPF-REC FROM   HEADING-O                                      
                          FORMAT "HEADING".                                     
      *           
      *   憿舐內閮臬                                                        
      *           
           WRITE DSPF-REC FROM       MSGAREA-O                                  
                          FORMAT     "MSGAREA"                                  
                          INDICATORS MSGAREA-O-INDIC.                           
      *           
      *   憿舐內 PRINT-MODE 銝剔 COLUMN HEADING                               
      *                       SUBFILE                                       
      *                       賡萄                                      
      *   霈 PRINT-MODE 銝剔 SUBFILE                                      
      *           
           WRITE DSPF-REC FROM   RANGEAREA-O                                    
                          FORMAT "RANGEAREA"                                    
                          INDICATORS RANGEAREA-O-INDIC.                         
           WRITE DSPF-REC FORMAT "SIMPLEMODE".                                  
           READ DSPF INTO RANGEAREA-I                                           
                          FORMAT "RANGEAREA".                                   
           INITIALIZE CALL-PANGCL040-ARGUMENT.                                  
      *                                                                         
       5000-CHECK-FUNCTION-KEY.                                                 
      *           
      *   PRINT MODE 銝   SUBFILE                                           
      *             SEND-KEY (USE NULL RECORD TO ADD IF ADD-MODE)           
      *           
           IF  ENTER-KEY                                                        
               PERFORM 5500-CHECK-ENTER-DATA                                    
               IF  ERROR-COUNT = 0                                              
                   WRITE DSPF-REC FORMAT "PROMPTAREA"                           
                   PERFORM 6000-EXECUTE-PRINT-LOOP                              
               END-IF                                                           
           END-IF.                                                              
      *           
      *   蝔撘蝯嚗銝銝詨桀                                          
      *           
           IF  PF3                                                              
               MOVE "E" TO PROCESS-SW                                           
           END-IF.                                                              
      *           
      *    REFRESH Ｚ                                             
      *           
           IF  PF5                                                              
               INITIALIZE RANGEAREA-O                                           
           END-IF.                                                              
      *           
      *   蝪賢                                                              
      *           
           IF  PF24                                                             
               MOVE FUNCTION-KEY TO LINK-RETURN-PF-KEY                          
               MOVE "E" TO PROCESS-SW                                           
           END-IF.                                                              
      *                                                                         
       5500-CHECK-ENTER-DATA.                                                   
           MOVE 0 TO ERROR-COUNT.                                               
           MOVE CORR RANGEAREA-I TO RANGEAREA-O.                                
           COMPUTE START-YYYYMMDD = YYYYMM IN RANGEAREA-I * 100 + 1.            
           COMPUTE END-YYYYMMDD = YYYYMM IN RANGEAREA-I * 100 + 31.             
           INITIALIZE SECHKDATE-MSG-ID .                                        
           MOVE START-YYYYMMDD TO SECHKDATE-DATE.                               
           CALL "SECHKDATE" USING SECHKDATE-DATE SECHKDATE-MSG-ID.              
           IF  SECHKDATE-MSG-ID NOT = SPACES                                    
.              MOVE INDIC-ON TO IN36 OF RANGEAREA-O-INDIC                       
.              MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O                       
.              ADD 1 TO ERROR-COUNT                                             
           END-IF.                                                              
           IF  ERROR-COUNT > 1                                                  
               MOVE "EDT0010" TO PANGCL040-MESSAGE-CODE-O                       
           END-IF.                                                              
      *                                                                         
       6000-EXECUTE-PRINT-LOOP.                                                 
           SORT SORTF ASCENDING KEY SORTF-KEY                                   
                      INPUT PROCEDURE 6000-SORT-INPUT-PROC                      
                      OUTPUT PROCEDURE 6000-SORT-OUTPUT-PROC.                   
      *                                                                         
       6000-SORT-INPUT-PROC.                                                    
           INITIALIZE PPTBA08P-REC.                                             
           MOVE START-YYYYMMDD TO TBA08000 IN PPTBA08P-REC.                     
           PERFORM 9000-START-PPTBA08P-RTN.                                     
           PERFORM UNTIL NOT PPTBA08P-NORMAL                      OR ,          
                         TBA08000 IN PPTBA08P-REC > END-YYYYMMDD                
             PERFORM 9100-READ-PPTBA08P-NEXT-RTN                                
             IF  PPTBA08P-NORMAL                            AND ,               
                 TBA08000 IN PPTBA08P-REC >= START-YYYYMMDD AND ,               
                 TBA08000 IN PPTBA08P-REC <= END-YYYYMMDD                       
                 IF  TBA08110 IN PPTBA08P-REC = LINK-BRANCH-CODE                
                     INITIALIZE SORTF-REC                                       
                     MOVE TBA08120 IN PPTBA08P-REC TO SORTF-010                 
                     MOVE TBA08000 IN PPTBA08P-REC TO SORTF-020                 
                     MOVE "1"                      TO SORTF-030                 
                     INITIALIZE CALL-SEREDSA0B-ARGUMENT                         
                     MOVE TBA08110 IN PPTBA08P-REC TO SA0010                    
                     MOVE TBA08120 IN PPTBA08P-REC TO SA0020                    
                     CALL "SEREDSA0B" USING SEREDSA0B-STATUS                    
                                            SESA0P-RECORD                       
                     MOVE SA0030                   TO SORTF-100                 
                     MOVE TBA08010 IN PPTBA08P-REC TO SORTF-210                 
                     INITIALIZE CALL-PANGSUB430-ARGUMENT                        
                     MOVE TBA08010 IN PPTBA08P-REC TO                           
                                      PANGSUB430-BRANCH-CODE                    
                     CALL "PANGSUB430" USING CALL-PANGSUB430-ARGUMENT           
                     CANCEL "PANGSUB430"                                        
                     MOVE PANGSUB430-BRANCH-NAME   TO SORTF-220                 
                     MOVE TBA08020 IN PPTBA08P-REC TO SORTF-230                 
                     INITIALIZE CALL-SEREDSA0B-ARGUMENT                         
                     MOVE TBA08010 IN PPTBA08P-REC TO SA0010                    
                     MOVE TBA08020 IN PPTBA08P-REC TO SA0020                    
                     CALL "SEREDSA0B" USING SEREDSA0B-STATUS                    
                                            SESA0P-RECORD                       
  +                  MOVE SA0030                   TO SORTF-240                 
                     MOVE TBA08210 IN PPTBA08P-REC TO SORTF-310                 
                     MOVE TBA08220 IN PPTBA08P-REC TO SORTF-320                 
                     RELEASE SORTF-REC                                          
                 END-IF                                                         
                 IF  TBA08010 IN PPTBA08P-REC = LINK-BRANCH-CODE                
                     INITIALIZE SORTF-REC                                       
                     MOVE TBA08020 IN PPTBA08P-REC TO SORTF-010                 
                     MOVE TBA08000 IN PPTBA08P-REC TO SORTF-020                 
                     MOVE "2"                      TO SORTF-030                 
                     INITIALIZE CALL-SEREDSA0B-ARGUMENT                         
                     MOVE TBA08010 IN PPTBA08P-REC TO SA0010                    
                     MOVE TBA08020 IN PPTBA08P-REC TO SA0020                    
                     CALL "SEREDSA0B" USING SEREDSA0B-STATUS                    
                                            SESA0P-RECORD                       
                     MOVE SA0030                   TO SORTF-100                 
                     MOVE TBA08110 IN PPTBA08P-REC TO SORTF-210                 
                     INITIALIZE CALL-PANGSUB430-ARGUMENT                        
                     MOVE TBA08110 IN PPTBA08P-REC TO                           
                                      PANGSUB430-BRANCH-CODE                    
                     CALL "PANGSUB430" USING CALL-PANGSUB430-ARGUMENT           
                     CANCEL "PANGSUB430"                                        
                     MOVE PANGSUB430-BRANCH-NAME   TO SORTF-220                 
                     MOVE TBA08120 IN PPTBA08P-REC TO SORTF-230                 
                     INITIALIZE CALL-SEREDSA0B-ARGUMENT                         
                     MOVE TBA08110 IN PPTBA08P-REC TO SA0010                    
                     MOVE TBA08120 IN PPTBA08P-REC TO SA0020                    
                     CALL "SEREDSA0B" USING SEREDSA0B-STATUS                    
                                            SESA0P-RECORD                       
  +                  MOVE SA0030                   TO SORTF-240                 
                     MOVE TBA08210 IN PPTBA08P-REC TO SORTF-310                 
                     MOVE TBA08220 IN PPTBA08P-REC TO SORTF-320                 
                     COMPUTE SORTF-320 = SORTF-320 * -1                         
                     RELEASE SORTF-REC                                          
                 END-IF                                                         
             END-IF                                                             
           END-PERFORM.                                                         
      *                                                                         
      *           
      *   脣亙啗艘                                                      
      *           
       6000-SORT-OUTPUT-PROC.                                                   
           INITIALIZE BREAK-KEY.                                                
      *    ACCEPT SYSTEM-DATE FROM DATE.                                        
           CALL "PANGSUB500" USING CALL-PANGSUB500-ARGUMENT.                    
           ACCEPT SYSTEM-TIME FROM TIME.                                        
           MOVE "N" TO OPEN-OUTPUT-PRTF-SW.                                     
           SET SORTF-NORMAL TO TRUE.                                            
           PERFORM UNTIL NOT SORTF-NORMAL                                       
             RETURN SORTF AT END SET SORTF-AT-END TO TRUE                       
             END-RETURN                                                         
             IF  SORTF-NORMAL                                                   
                 IF  NOT OPEN-OUTPUT-PRTF-OK                                    
                     OPEN OUTPUT PRTF                                           
                     INITIALIZE PRTF-REC                                        
                     MOVE "Y" TO OPEN-OUTPUT-PRTF-SW                            
                 END-IF                                                         
                 IF  SORTF-010 NOT = BREAK-SORTF-010    AND ,                   
                     BREAK-SORTF-010 NOT = SPACES                               
                     IF  WK-COUNT > 1                                           
                         PERFORM 6000-PRINT-SUB-TOTAL-LINE                      
                     END-IF                                                     
                     INITIALIZE WK-COUNT S0                                     
                 END-IF                                                         
                 PERFORM 6000-PRINT-BODY-AREA-LINE                              
                 ADD 1 TO WK-COUNT                                              
                 MOVE SORTF-010 TO BREAK-SORTF-010                              
             END-IF                                                             
           END-PERFORM.                                                         
           IF  WK-COUNT > 1                                                     
               PERFORM 6000-PRINT-SUB-TOTAL-LINE                                
           END-IF                                                               
           IF  OPEN-OUTPUT-PRTF-OK                                              
               PERFORM 6000-PRINT-FINAL-TOTAL-LINE                              
               CLOSE PRTF                                                       
               MOVE "NTF0260" TO PANGCL040-MESSAGE-CODE-O                       
           ELSE                                                                 
               MOVE "NTF0250" TO PANGCL040-MESSAGE-CODE-O                       
           END-IF.                                                              
           MOVE 1  TO PAGE-CNT.                                                 
           MOVE 90 TO BODY-AREA-LINE-CNT.                                       
      *                                                                         
      *           
      *   啗”                                                          
      *           
       6000-PRINT-HEAD-AREA.                                                    
      * 敶扳餈啣                                                              
      *    MOVE   TBLRATE IN RANGEAREA-I     TO WP-TBLRATE.                     
      * 箏餈啣                                                              
           MOVE PANGSUB500-TW-SYSTEM-DATE TO WP-SYS-DATE.                       
           MOVE   SYSTEM-TIME-HH       TO WP-SYS-TIME-H.                        
           MOVE   SYSTEM-TIME-MM       TO WP-SYS-TIME-M.                        
           MOVE   SYSTEM-TIME-SS       TO WP-SYS-TIME-S.                        
      *    MOVE   LINK-BROKER-ID       TO WP-BROKER-ID.                         
           MOVE   LINK-BRANCH-CODE     TO WP-BRANCH-CODE.                       
           MOVE   LINK-BRANCH-NAME     TO WP-BRANCH-NAME.                       
           MOVE   LINK-BROKER-NAME     TO WP-BROKER-NAME.                       
           MOVE   LINK-PROGRAM-CODE    TO WP-PROGRAM-CODE.                      
           MOVE   LINK-PROGRAM-NAME    TO WP-PROGRAM-NAME.                      
           MOVE   PAGE-CNT             TO WP-PAGE-CNT.                          
           IF  PAGE-CNT > 1                                                     
               WRITE PRTF-REC FROM WP-HEAD01 AFTER PAGE                         
           ELSE                                                                 
               WRITE PRTF-REC FROM WP-HEAD01 AFTER 0                            
           END-IF.                                                              
           WRITE PRTF-REC FROM WP-HEAD02 AFTER 1.                               
           WRITE PRTF-REC FROM WP-HEAD03 AFTER 1.                               
           WRITE PRTF-REC FROM WP-HEAD04 AFTER 1.                               
           WRITE PRTF-REC FROM WP-HEAD05 AFTER 1.                               
           MOVE   0   TO  BODY-AREA-LINE-CNT.                                   
           ADD    1   TO  PAGE-CNT.                                             
      *                                                                --*      
      *           
      *   唳蝝                                                          
      *           
       6000-PRINT-BODY-AREA-LINE.                                               
           IF  BODY-AREA-LINE-CNT  NOT <  BODY-AREA-MAX-LINE                    
               PERFORM 6000-PRINT-HEAD-AREA                                     
           END-IF.                                                              
           IF  BODY-AREA-LINE-CNT = ZEROS       OR ,                            
               SORTF-010 NOT = BREAK-SORTF-010                                  
               MOVE SORTF-010 TO WP-SORTF-010                                   
               MOVE SORTF-100 TO WP-SORTF-100                                   
           END-IF.                                                              
           MOVE SORTF-020             TO WP-SORTF-020.                          
           IF  SORTF-030 = "1"                                                  
               MOVE " 隞亙 "        TO WP-SORTF-030                           
           ELSE                                                                 
               MOVE " 鋡思誨 "        TO WP-SORTF-030                           
           END-IF.                                                              
           MOVE SORTF-210             TO WP-SORTF-210.                          
           MOVE SORTF-220             TO WP-SORTF-220.                          
           MOVE SORTF-230             TO WP-SORTF-230.                          
           MOVE SORTF-240             TO WP-SORTF-240.                          
           MOVE SORTF-310             TO WP-SORTF-310.                          
           MOVE SORTF-320             TO WP-SORTF-320.                          
           ADD SORTF-320 TO S1.                                                 
           WRITE PRTF-REC  FROM  WP-BODY01 AFTER 1.                             
           ADD 1 TO BODY-AREA-LINE-CNT.                                         
           INITIALIZE WP-BODY01 PRTF-REC.                                       
      *                                                                         
      *           
      *   啣閮嚗隞乩誨銵冽嗥 BREAK KEY 嚗                                 
      *           
       6000-PRINT-SUB-TOTAL-LINE.                                               
           IF  BODY-AREA-LINE-CNT  NOT <  BODY-AREA-MAX-LINE                    
               PERFORM   6000-PRINT-HEAD-AREA                                   
           END-IF.                                                              
           MOVE S1 TO WP-S1.                                                    
           WRITE  PRTF-REC  FROM WP-SUB-BREAK-LINE  AFTER 1.                    
           WRITE  PRTF-REC  FROM WP-SUB-TOTAL01     AFTER 1.                    
           ADD 1 TO BODY-AREA-LINE-CNT.                                         
           INITIALIZE WP-SUB-TOTAL01 PRTF-REC S0.                               
      *                                                                         
      *           
      *   啣閮                                                          
      *           
       6000-PRINT-FINAL-TOTAL-LINE.                                             
      *    MOVE WK-COUNT-TOTAL TO WP-COMMENT-COUNT-TOTAL.                       
      *    COMPUTE WP-COMMENT-LOW-TOTAL  = WK-LOW-TOTAL                         
      *                                  / WK-COUNT-TOTAL.                      
      *    COMPUTE WP-COMMENT-HIGH-TOTAL = WK-HIGH-TOTAL                        
      *                                  / WK-COUNT-TOTAL.                      
           WRITE  PRTF-REC  FROM WP-FINAL-BOTTOM-LINE  AFTER 1.                 
      *    WRITE  PRTF-REC  FROM WP-FINAL-COMMENT      AFTER 1.                 
      *    WRITE  PRTF-REC  FROM WP-FINAL-TOTAL01      AFTER 1.                 
      *    WRITE  PRTF-REC  FROM WP-FOOT01             AFTER 3.                 
           INITIALIZE WP-FINAL-TOTAL01 PRTF-REC.                                
      *                                                                         
      *---------------------------------------------------------------*         
      *                                                                         
       9000-START-PPTBA08P-RTN.                                                 
           START PPTBA08P KEY IS >= EXTERNALLY-DESCRIBED-KEY.                   
           IF  NOT PPTBA08P-NORMAL AND ,                                        
               NOT PPTBA08P-NOT-FOUND                                           
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0030"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPTBA08P   "        TO PANGCL040-FILE-NAME-O               
                                           LINK-MSG-FILE-NAME                   
               MOVE PPTBA08P-FILE-STATUS TO PANGCL040-FILE-STATUS-O             
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9100-READ-PPTBA08P-NEXT-RTN.                                             
           READ PPTBA08P NEXT.                                                  
           IF  NOT PPTBA08P-NORMAL AND ,                                        
               NOT PPTBA08P-AT-END                                              
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0040"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPTBA08P   "        TO PANGCL040-FILE-NAME-O               
                                           LINK-MSG-FILE-NAME                   
               MOVE PPTBA08P-FILE-STATUS TO PANGCL040-FILE-STATUS-O             
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
