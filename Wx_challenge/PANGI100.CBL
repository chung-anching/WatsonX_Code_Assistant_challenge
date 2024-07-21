       PROCESS             GRAPHIC                                              
       IDENTIFICATION      DIVISION.                                            
      *           
      *       :  PANGI100                                           
      *           :                               
      *       :  PANGI100D                                          
      *       :  PPRLT0P(IO)    PPBRN1P(I)   PPRLT0L1               
      *       :                                               
      *       :  86/12/14                                           
      *       :   & DDS RECORD FORMAT NAME            
      *                   CHANGE STRING                             
      *       :    /  /                                             
      *       :                                                     
      *           
       PROGRAM-ID.             PANGI100.                                        
      *           
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER.        IBM-AS400.                                       
       OBJECT-COMPUTER.        IBM-AS400.                                       
      *                                                                         
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT DSPF ASSIGN   TO WORKSTATION-PANGI100D-SI                     
                  ORGANIZATION  IS TRANSACTION                                  
                  ACCESS  MODE  IS DYNAMIC                                      
                  RELATIVE KEY  IS RELATIVE-KEY-VALUE                           
                  CONTROL-AREA  IS WK-CONTROL-AREA                              
                  FILE STATUS   IS DSPF-FILE-STATUS.                            
      *                                                                         
++++++     SELECT PPSYS0P ASSIGN TO DATABASE-PPSYS0P                            
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS PPSYS0P-FILE-STATUS.                        
      *                                                                         
++++++     SELECT PPRLT0L1 ASSIGN TO DATABASE-PPRLT0L1                          
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS PPRLT0L1-FILE-STATUS.                       
      *                                                                         
++++++     SELECT PPBRN1P ASSIGN TO DATABASE-PPBRN1P                            
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS PPBRN1P-FILE-STATUS.                        
      *                                                                         
++++++     SELECT PPRLT0P ASSIGN TO DATABASE-PPRLT0P                            
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS PPRLT0P-FILE-STATUS.                        
      *                                                                         
++++++     SELECT SECSAP ASSIGN TO DATABASE-SECSAP                              
  .               ORGANIZATION   IS INDEXED                                     
  .               ACCESS MODE    IS DYNAMIC                                     
  .               RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY                    
++++++            FILE STATUS    IS SECSAP-FILE-STATUS.                         
      *           
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
      *                                                                         
       FD  DSPF LABEL RECORD STANDARD.                                          
       01  DSPF-REC                            PIC X(1920).                     
      *                                                                         
++++++ FD  PPRLT0P LABEL RECORD STANDARD.                                       
  .    01  PPRLT0P-REC.                                                         
++++++     COPY DDS-ALL-FORMATS OF PPRLT0P.                                     
      *                                                                         
++++++ FD  PPSYS0P LABEL RECORD STANDARD.                                       
  .    01  PPSYS0P-REC.                                                         
++++++     COPY DDS-ALL-FORMATS OF PPSYS0P.                                     
      *                                                                         
++++++ FD  PPRLT0L1 LABEL RECORD STANDARD.                                      
  .    01  PPRLT0L1-REC.                                                        
++++++     COPY DDS-ALL-FORMATS OF PPRLT0L1.                                    
      *                                                                         
++++++ FD  PPBRN1P LABEL RECORD STANDARD.                                       
  .    01  PPBRN1P-REC.                                                         
++++++     COPY DDS-ALL-FORMATS OF PPBRN1P.                                     
      *                                                                         
++++++ FD  SECSAP LABEL RECORD STANDARD.                                        
  .    01  SECSAP-REC.                                                          
++++++     COPY DDS-ALL-FORMATS OF SECSAP.                                      
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
           COPY PANGSTDWRK OF PAULCBL.                                          
      *                                                                         
       01  DSPF-DDS-FORMAT.                                                     
           COPY DDS-HEADING-O          OF PANGI100D.                            
           COPY DDS-LISTSFL-I          OF PANGI100D.                            
           COPY DDS-LISTSFL-O          OF PANGI100D.                            
           COPY DDS-LISTCTL-I          OF PANGI100D.                            
           COPY DDS-LISTCTL-O          OF PANGI100D.                            
           COPY DDS-KEYAREA-O          OF PANGI100D.                            
           COPY DDS-DATAPAGE1-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE1-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE2-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE2-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE3-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE3-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE4-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE4-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE5-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE5-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE6-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE6-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE7-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE7-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE8-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE8-O        OF PANGI100D.                            
           COPY DDS-DATAPAGE9-I        OF PANGI100D.                            
           COPY DDS-DATAPAGE9-O        OF PANGI100D.                            
           COPY DDS-MSGAREA-O          OF PANGI100D.                            
           COPY DDS-ALL-FORMATS-INDIC  OF PANGI100D.                            
      *                                                                         
       01  FILE-STATUS.                                                         
           05  DSPF-FILE-STATUS                PIC X(002).                      
               88  DSPF-NORMAL                     VALUE "00" "02".             
               88  DSPF-AT-END                     VALUE "10".                  
               88  DSPF-NOT-FOUND                  VALUE "23".                  
               88  DSPF-RECORD-LOCK                VALUE "9D".                  
      *                                                                         
++++++     05  PPSYS0P-FILE-STATUS             PIC X(02).                       
  .            88  PPSYS0P-NORMAL                  VALUE "00" "02".             
  .            88  PPSYS0P-AT-END                  VALUE "10".                  
  .            88  PPSYS0P-NOT-FOUND               VALUE "23".                  
++++++         88  PPSYS0P-RECORD-LOCK             VALUE "9D".                  
      *                                                                         
++++++     05  PPRLT0L1-FILE-STATUS           PIC X(02).                        
  .            88  PPRLT0L1-NORMAL                VALUE "00" "02".              
  .            88  PPRLT0L1-AT-END                VALUE "10".                   
  .            88  PPRLT0L1-NOT-FOUND             VALUE "23".                   
++++++         88  PPRLT0L1-RECORD-LOCK           VALUE "9D".                   
      *                                                                         
++++++     05  PPBRN1P-FILE-STATUS             PIC X(02).                       
  .            88  PPBRN1P-NORMAL                  VALUE "00" "02".             
  .            88  PPBRN1P-AT-END                  VALUE "10".                  
  .            88  PPBRN1P-NOT-FOUND               VALUE "23".                  
++++++         88  PPBRN1P-RECORD-LOCK             VALUE "9D".                  
      *                                                                         
++++++     05  PPRLT0P-FILE-STATUS             PIC X(02).                       
  .            88  PPRLT0P-NORMAL                  VALUE "00" "02".             
  .            88  PPRLT0P-AT-END                  VALUE "10".                  
  .            88  PPRLT0P-NOT-FOUND               VALUE "23".                  
++++++         88  PPRLT0P-RECORD-LOCK             VALUE "9D".                  
      *                                                                         
++++++     05  SECSAP-FILE-STATUS             PIC X(02).                        
  .            88  SECSAP-NORMAL                  VALUE "00" "02".              
  .            88  SECSAP-AT-END                  VALUE "10".                   
  .            88  SECSAP-NOT-FOUND               VALUE "23".                   
++++++         88  SECSAP-RECORD-LOCK             VALUE "9D".                   
      *                                                                         
       01  PRIVATE-WORK-AREA.                                                   
           05  WK-LEGAL-PAGE                      PIC 9(02).                    
               88  LEGAL-PAGE                     VALUE 1 2 7.                  
      *           
      *    PF  PRIMARY KEY                                      
      *           
           05  EXTERNALLY-KEY.                                                  
++++++         10  RLT0020                   PIC X(06).                         
++++++     05  WK-BRN1100                    PIC X(05).                         
           05  A1                            PIC 999V99 VALUE ZEROS.            
           05  B1                            PIC 999V99 VALUE ZEROS.            
           05  C1                            PIC 999V99 VALUE ZEROS.            
           05  D1                            PIC 999V99 VALUE ZEROS.            
           05  E1                            PIC 999V99 VALUE ZEROS.            
           05  F1                            PIC 999V9 VALUE ZEROS.             
           05  G1                            PIC 999V9 VALUE ZEROS.             
       01  WK-OLD-RECORD.                                                       
++++++     COPY DDS-ALL-FORMATS OF PPRLT0P.                                     
           66  OLD-PAGE1-DATA RENAMES RLT0100 IN WK-OLD-RECORD                  
                              THRU    RLT0140 IN WK-OLD-RECORD.                 
           66  OLD-PAGE2-DATA RENAMES RLT0210 IN WK-OLD-RECORD                  
                              THRU    RLT0497 IN WK-OLD-RECORD.                 
           66  OLD-PAGE3-DATA RENAMES RLT0410 IN WK-OLD-RECORD                  
                              THRU    RLT0497 IN WK-OLD-RECORD.                 
           66  OLD-PAGE4-DATA RENAMES RLT0510 IN WK-OLD-RECORD                  
                              THRU    RLT0597 IN WK-OLD-RECORD.                 
           66  OLD-PAGE5-DATA RENAMES RLT0610 IN WK-OLD-RECORD                  
                              THRU    RLT0697 IN WK-OLD-RECORD.                 
           66  OLD-PAGE6-DATA RENAMES RLT0800 IN WK-OLD-RECORD                  
                              THRU    RLT0890 IN WK-OLD-RECORD.                 
           66  OLD-PAGE7-DATA RENAMES RLT0A10 IN WK-OLD-RECORD                  
                              THRU    RLT0AA6 IN WK-OLD-RECORD.                 
           66  OLD-PAGE8-DATA RENAMES RLT0B10 IN WK-OLD-RECORD                  
                              THRU    RLT0BA6 IN WK-OLD-RECORD.                 
           66  OLD-PAGE9-DATA RENAMES RLT0710 IN WK-OLD-RECORD                  
                              THRU    RLT0780 IN WK-OLD-RECORD.                 
           66  OLD-RLT0271-DATA RENAMES RLT0271 IN WK-OLD-RECORD                
                                THRU    RLT0278 IN WK-OLD-RECORD.               
           66  OLD-RLT0C81-DATA RENAMES RLT0C81 IN WK-OLD-RECORD                
                                THRU    RLT0C88 IN WK-OLD-RECORD.               
           66  OLD-RLT0C91-DATA RENAMES RLT0C91 IN WK-OLD-RECORD                
                                THRU    RLT0C98 IN WK-OLD-RECORD.               
           66  OLD-RLT0A71-DATA RENAMES RLT0A71 IN WK-OLD-RECORD                
                                THRU    RLT0A78 IN WK-OLD-RECORD.               
           66  OLD-RLT0D81-DATA RENAMES RLT0D81 IN WK-OLD-RECORD                
                                THRU    RLT0D88 IN WK-OLD-RECORD.               
           66  OLD-RLT0A81-DATA RENAMES RLT0A81 IN WK-OLD-RECORD                
                                THRU    RLT0A88 IN WK-OLD-RECORD.               
           66  OLD-RLT0B71-DATA RENAMES RLT0B71 IN WK-OLD-RECORD                
                                THRU    RLT0B78 IN WK-OLD-RECORD.               
           66  OLD-RLT0E81-DATA RENAMES RLT0E81 IN WK-OLD-RECORD                
                                THRU    RLT0E88 IN WK-OLD-RECORD.               
           66  OLD-RLT0B81-DATA RENAMES RLT0B81 IN WK-OLD-RECORD                
                                THRU    RLT0B88 IN WK-OLD-RECORD.               
       01  WK-NEW-RECORD.                                                       
++++++     COPY DDS-ALL-FORMATS OF PPRLT0P.                                     
           66  NEW-PAGE1-DATA RENAMES RLT0100 IN WK-NEW-RECORD                  
                              THRU    RLT0140 IN WK-NEW-RECORD.                 
           66  NEW-PAGE2-DATA RENAMES RLT0210 IN WK-NEW-RECORD                  
                              THRU    RLT0497 IN WK-NEW-RECORD.                 
           66  NEW-PAGE3-DATA RENAMES RLT0410 IN WK-NEW-RECORD                  
                              THRU    RLT0497 IN WK-NEW-RECORD.                 
           66  NEW-PAGE4-DATA RENAMES RLT0510 IN WK-NEW-RECORD                  
                              THRU    RLT0597 IN WK-NEW-RECORD.                 
           66  NEW-PAGE5-DATA RENAMES RLT0610 IN WK-NEW-RECORD                  
                              THRU    RLT0697 IN WK-NEW-RECORD.                 
           66  NEW-PAGE6-DATA RENAMES RLT0800 IN WK-NEW-RECORD                  
                              THRU    RLT0890 IN WK-NEW-RECORD.                 
           66  NEW-PAGE7-DATA RENAMES RLT0A10 IN WK-NEW-RECORD                  
                              THRU    RLT0AA6 IN WK-NEW-RECORD.                 
           66  NEW-PAGE8-DATA RENAMES RLT0B10 IN WK-NEW-RECORD                  
                              THRU    RLT0BA6 IN WK-NEW-RECORD.                 
           66  NEW-PAGE9-DATA RENAMES RLT0710 IN WK-NEW-RECORD                  
                              THRU    RLT0780 IN WK-NEW-RECORD.                 
           66  NEW-RLT0271-DATA RENAMES RLT0271 IN WK-NEW-RECORD                
                                THRU    RLT0278 IN WK-NEW-RECORD.               
           66  NEW-RLT0C81-DATA RENAMES RLT0C81 IN WK-NEW-RECORD                
                                THRU    RLT0C88 IN WK-NEW-RECORD.               
           66  NEW-RLT0C91-DATA RENAMES RLT0C91 IN WK-NEW-RECORD                
                                THRU    RLT0C98 IN WK-NEW-RECORD.               
           66  NEW-RLT0A71-DATA RENAMES RLT0A71 IN WK-NEW-RECORD                
                                THRU    RLT0A78 IN WK-NEW-RECORD.               
           66  NEW-RLT0D81-DATA RENAMES RLT0D81 IN WK-NEW-RECORD                
                                THRU    RLT0D88 IN WK-NEW-RECORD.               
           66  NEW-RLT0A81-DATA RENAMES RLT0A81 IN WK-NEW-RECORD                
                                THRU    RLT0A88 IN WK-NEW-RECORD.               
           66  NEW-RLT0B71-DATA RENAMES RLT0B71 IN WK-NEW-RECORD                
                                THRU    RLT0B78 IN WK-NEW-RECORD.               
           66  NEW-RLT0E81-DATA RENAMES RLT0E81 IN WK-NEW-RECORD                
                                THRU    RLT0E88 IN WK-NEW-RECORD.               
           66  NEW-RLT0B81-DATA RENAMES RLT0B81 IN WK-NEW-RECORD                
                                THRU    RLT0B88 IN WK-NEW-RECORD.               
      *           
      *  DATAQ LAYOUT                                                       
      *           
       01  DTAQ01-LAYOUT.                                                       
           05  DTAQ01.                                                          
               10  FILLER              PIC X(004) VALUE "1003".                 
               10  FILLER              PIC X(001) VALUE ";".                    
      * DTAQ01-MSGTYPE = A.      C.                           
               10  DTAQ01-MSGTYPE      PIC X(001) VALUE SPACES.                 
               10  FILLER              PIC X(001) VALUE ";".                    
      *                                                                 
               10  DTAQ01-DATE              PIC 9(007) VALUE ZEROS.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  DTAQ01-TIME              PIC 9(006) VALUE ZEROS.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  RLT0010                 PIC X(004) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0020                 PIC X(006) VALUE SPACES.             
      *      ∠                                                       
               10  DTAQ01-RLT0020-CS0025    PIC X(001) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      ⊿                                                       
               10  DTAQ01-RLT0020-CS0030    PIC X(012) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       ('1.  2.  3.  4.  9.  )         
               10  RLT0110                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      剛                                                     
               10  RLT0120                 PIC X(060) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      啣  Y/N                                                      
               10  RLT0130                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0140                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *  頦萇                                               
      *       Y/N                                                       
               10  RLT0210                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0211                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0220                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0221                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0222                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0223                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0224                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0230                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0230-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0230-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                   
               10  RLT0240                 PIC X(006) VALUE SPACES.             
      *                                                               
               10  DTAQ01-RLT0240-CS0025    PIC X(001) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  DTAQ01-RLT0240-CS0030    PIC X(012) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      鞈 S.  M.                                      
               10  RLT0250                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N  鞈 S                           
               10  RLT0255                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0261                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0262                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3 血砥                                                    
               10  RLT0263                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4 血砥                                                    
               10  RLT0264                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5 血砥                                                    
               10  RLT0265                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1                                                 
               10  RLT0271                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2                                                 
               10  RLT0272                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3                                                 
               10  RLT0273                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4                                                 
               10  RLT0274                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5                                                 
               10  RLT0275                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞蔬 Y/N                                               
               10  RLT0280                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      株正撗 Y/N                                               
               10  RLT0282                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                               
               10  RLT0290                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *         b.  Y.  N.                        
               10  RLT0292                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *         b.  E.                              
               10  RLT0310                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       y/n                                                   
               10  RLT0340                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      擗   0.                                  
               10  RLT0350                 PIC Z(01)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                         
      *       Y/N                                                       
               10  RLT0410                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0411                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0420                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0421                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0422                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0423                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0424                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0430                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0430-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0430-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0441                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0442                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1                                                       
               10  RLT0451                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2                                                       
               10  RLT0452                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N  鞈 S                           
               10  RLT0455                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞螂 Y/N                                               
               10  RLT0460                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞蔬 Y/N                                               
               10  RLT0461                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      畾 (b.  B.  M.  )                
               10  RLT0463                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭 (P.  g.  )                                    
               10  RLT0466                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0471                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0472                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *  寥２                                                 
      *       Y/N                                                       
               10  RLT0510                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0511                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0520                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0521                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0522                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0523                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0524                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0530                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0530-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0530-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0541                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0542                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蝞 %% 1                                                  
               10  RLT0551                 PIC Z(01)9.9(4) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蝞 %% 2                                                  
               10  RLT0552                 PIC Z(01)9.9(4) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N  鞈 S                           
               10  RLT0555                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞螂 Y/N                                               
               10  RLT0560                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞蔬 Y/N                                               
               10  RLT0561                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      畾 (b.  S.  )                                  
               10  RLT0563                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *     獢 Y/N                                                  
               10  RLT0564                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭 (P.  g.  )                                    
               10  RLT0566                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0571                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0572                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *  蝞                                                 
      *       Y/N                                                       
               10  RLT0610                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0611                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0620                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0621                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0622                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0623                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0624                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0630                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0630-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0630-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0641                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0642                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *      頦 %% 1                                                  
               10  RLT0651                 PIC Z(01)9.9(4) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *      頦 %% 2                                                  
               10  RLT0652                 PIC Z(01)9.9(4) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N  鞈 S                           
               10  RLT0655                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞螂 Y/N                                               
               10  RLT0660                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞蔬 Y/N                                               
               10  RLT0661                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      畾 (b.  B.  )                                  
               10  RLT0663                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                 
               10  RLT0664                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭 (P.  g.  )                                    
               10  RLT0666                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0671                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *        Y/N                                              
               10  RLT0672                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *  閮暸                                               
      *       Y/N                                                       
               10  RLT0A10                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0A11                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0A20                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0A21                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0A22                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0A23                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0A24                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0A30                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0A30-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0A30-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0A61                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0A62                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3 血砥                                                    
               10  RLT0A63                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4 血砥                                                    
               10  RLT0A64                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5 血砥                                                    
               10  RLT0A65                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1                                                     
               10  RLT0A71                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2                                                     
               10  RLT0A72                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3                                                     
               10  RLT0A73                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4                                                     
               10  RLT0A74                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5                                                     
               10  RLT0A75                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 1                                                   
               10  RLT0A81                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 2                                                   
               10  RLT0A82                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 3                                                   
               10  RLT0A83                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 4                                                   
               10  RLT0A84                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 5                                                   
               10  RLT0A85                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      剜唾 Y/N                                       
               10  RLT0A89                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞螂 Y/N                                               
               10  RLT0A90                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                       
               10  RLT0A92                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *  閮暸                                                   
      *       Y/N                                                       
               10  RLT0B10                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                           
               10  RLT0B11                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0B20                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0B21                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                                 
               10  RLT0B22                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      撖⊿賜                                                       
               10  RLT0B23                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                             
               10  RLT0B24                 PIC 9(008) VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                           
               10  RLT0B30                 PIC X(005) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *                                                               
               10  DTAQ01-RLT0B30-SA0100    PIC X(002) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *      蟡                                                     
               10  DTAQ01-RLT0B30-SA0050    PIC X(010) VALUE SPACES.            
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1 血砥                                                    
               10  RLT0B61                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2 血砥                                                    
               10  RLT0B62                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3 血砥                                                    
               10  RLT0B63                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4 血砥                                                    
               10  RLT0B64                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5 血砥                                                    
               10  RLT0B65                 PIC Z(004)9.9 VALUE ZEROS.           
               10  FILLER                  PIC X(001) VALUE ";".                
      *       1                                                     
               10  RLT0B71                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       2                                                     
               10  RLT0B72                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       3                                                     
               10  RLT0B73                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       4                                                     
               10  RLT0B74                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *       5                                                     
               10  RLT0B75                 PIC Z(05)9.9(8) VALUE ZEROS.         
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 1                                                   
               10  RLT0B81                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 2                                                   
               10  RLT0B82                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 3                                                   
               10  RLT0B83                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 4                                                   
               10  RLT0B84                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      寡縣 5                                                   
               10  RLT0B85                 PIC Z(05)9 VALUE ZEROS.              
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞螂 Y/N                                               
               10  RLT0B90                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *      潭寞蔬 Y/N                                               
               10  RLT0B91                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
      *       Y/N                                                       
               10  RLT0B92                 PIC X(001) VALUE SPACES.             
               10  FILLER                  PIC X(001) VALUE ";".                
       01  WK-DAY-RATE.                                                         
           05  HEADING1                    PIC X(12) VALUE SPACES.              
           05  RATE-1                      PIC Z.ZZZZZZZZ VALUE ZEROS.          
           05  FILLER                      PIC X(02) VALUE SPACES.              
           05  HEADING2                    PIC X(12) VALUE SPACES.              
           05  RATE-2                      PIC Z.ZZZZZZZZ VALUE ZEROS.          
           05  FILLER                      PIC X(02) VALUE SPACES.              
           05  HEADING3                    PIC X(08) VALUE SPACES.              
           05  FILLER                      PIC X(02) VALUE SPACES.              
           05  HEADING4                    PIC X(08) VALUE SPACES.              
           05  FILLER                      PIC X(04) VALUE SPACES.              
      *                                                                         
       01  CALL-PANGCAL510-ARGUMENT.                                            
           05  PANGCAL510-INPUT-FIELD.                                          
      * 1. ⊿                                               
      * 2. ∪                                             
      * 5. ∪⊿                                     
      * 6. ∪⊿ BY TX0020                           
      * B.                                              
      * S. ∪蝞                                         
      * A. 啣駁                                                         
               10  PANGCAL510-ACTION           PIC X(01).                       
                   88  PANGCAL510-ACTION-1     VALUE "1".                       
                   88  PANGCAL510-ACTION-2     VALUE "2".                       
                   88  PANGCAL510-ACTION-5     VALUE "5".                       
                   88  PANGCAL510-ACTION-6     VALUE "6".                       
                   88  PANGCAL510-ACTION-B     VALUE "B".                       
                   88  PANGCAL510-ACTION-S     VALUE "S".                       
                   88  PANGCAL510-ACTION-A     VALUE "A".                       
               10  PANGCAL510-TX0010           PIC X(04).                       
               10  PANGCAL510-TX0020           PIC X(06).                       
               10  PANGCAL510-TX0030           PIC X(06).                       
               10  PANGCAL510-ECB-SW           PIC X(01).                       
               10  PANGCAL510-TX0040           PIC 9(08).                       
               10  PANGCAL510-TX0080           PIC X(01).                       
               10  PANGCAL510-TX0100           PIC X(01).                       
               10  PANGCAL510-TX0210           PIC X(02).                       
               10  PANGCAL510-TX0220           PIC X(05).                       
               10  PANGCAL510-TX0240           PIC X(01).                       
               10  PANGCAL510-TX0350           PIC S9(08).                      
      *      Y. 鞈頛舀 CLOSE ABOUT IR1P 2P 5P                   
               10  PANGCAL510-MERGE-SW         PIC X(01).                       
      *      頦寡部                                               
               10  PANGCAL510-DEATH-BRANCH     PIC X(04).                       
           05  PANGCAL510-OUTPUT-FIELD.                                         
      *                                                               
               10  PANGCAL510-SEIR1P-FLAG-B   PIC X(001).                       
                   88  PANGCAL510-HAVE-SEIR1P-B          VALUE "Y".             
      *                                                               
               10  PANGCAL510-SEIR1P-FLAG-D  PIC X(001).                        
                   88  PANGCAL510-HAVE-SEIR1P-D          VALUE "Y".             
      *                                                           
               10  PANGCAL510-SEIR1P-FLAG-S   PIC X(001).                       
                   88  PANGCAL510-HAVE-SEIR1P-S          VALUE "Y".             
      *      頦                                                 
               10  PANGCAL510-SEIR1P-FLAG     PIC X(001).                       
                   88  PANGCAL510-HAVE-SEIR1P            VALUE "Y".             
      *      頦                                                       
               10  PANGCAL510-SEIR2P-FLAG    PIC X(001).                        
                   88  PANGCAL510-HAVE-SEIR2P            VALUE "Y".             
      *      蝞                                               
               10  PANGCAL510-SEIR5P-FLAG     PIC X(001).                       
                   88  PANGCAL510-HAVE-SEIR5P            VALUE "Y".             
      *                                                     
               10  PANGCAL510-SEIR6P-FLAG    PIC X(001).                        
                   88  PANGCAL510-HAVE-SEIR6P            VALUE "Y".             
               10  PANGCAL510-IR1110         PIC 9(08).                         
               10  PANGCAL510-IR1120         PIC 9(08).                         
               10  PANGCAL510-IR1210         PIC 9(08).                         
               10  PANGCAL510-IR1220         PIC 9(08).                         
               10  PANGCAL510-IR1230         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR1310         PIC 9(08).                         
               10  PANGCAL510-IR1320         PIC 9(08).                         
               10  PANGCAL510-IR1330         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR1410         PIC 9(08).                         
               10  PANGCAL510-IR1420         PIC 9(08).                         
               10  PANGCAL510-IR1430         PIC 9(02)V9(08).                   
      *                                                         
      *        隞 < 981119 鞈                             
      *         SEIR2P IR2050 = "2"  頩 6                 
      *         65                                              
      *     981116 鞊                                             
      *        10  PANGCAL510-IR2050         PIC X(01).                         
               10  PANGCAL510-IR2110         PIC 9(08).                         
               10  PANGCAL510-IR2120         PIC 9(08).                         
               10  PANGCAL510-IR2130         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR2140         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR5110         PIC 9(08).                         
               10  PANGCAL510-IR5120         PIC 9(08).                         
               10  PANGCAL510-IR5130         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR6130         PIC 9(02)V9(08).                   
               10  PANGCAL510-IR6140         PIC 9(02)V9(08).                   
      *                                                                         
       01  CALL-PANGCAL520-ARGUMENT.                                            
           05  PANGCAL520-INPUT-FIELD.                                          
               10  PANGCAL520-TX0010           PIC X(04).                       
               10  PANGCAL520-TX0020           PIC X(06).                       
               10  PANGCAL520-TX0040           PIC 9(08).                       
               10  PANGCAL520-CSJ030           PIC X(02).                       
      *      Y. 鞈頛舀 CLOSE ABOUT IR1P 2P 5P                   
               10  PANGCAL520-MERGE-SW         PIC X(01).                       
      *      頦寡部                                               
               10  PANGCAL520-DEATH-BRANCH     PIC X(04).                       
           05  PANGCAL520-OUTPUT-FIELD.                                         
      *                                                             
               10  PANGCAL520-SECSJ06-FLAG     PIC X(001).                      
                   88  PANGCAL520-HAVE-SECSJ06           VALUE "Y".             
      *                                                                         
       01  CALL-PANGSUB440-ARGUMENT.                                            
++++++     COPY DDS-ALL-FORMATS OF PPJRN10P.                                    
      *                                                                         
       01  CALL-SECHKDATE-ARGUMENT.                                             
           05  SECHKDATE-DATE                    PIC X(08).                     
           05  SECHKDATE-MSG-ID                  PIC X(07).                     
      *                                                                         
       01  CALL-PANGCL150-ARGUMENT.                                             
           05  PANGCL150-USER-ID                 PIC X(10).                     
           05  PANGCL150-JOB-NAME                PIC X(10).                     
      *                                                                         
       01  CALL-SEREDSA0B-ARGUMENT.                                             
           05  SEREDSA0B-STATUS                  PIC X(02).                     
  .        88  SESA0P-NORMAL                     VALUE "00".                    
           COPY  DDS-ALL-FORMATS OF SESA0P.                                     
      *                                                                         
       01  CALL-SEREDCS0B-ARGUMENT.                                             
           05  SEREDCS0B-STATUS                  PIC X(02).                     
  .        88  SECS0P-NORMAL                     VALUE "00".                    
           COPY  DDS-ALL-FORMATS OF SECS0P.                                     
      *                                                                         
       01  CALL-QSNDDTAQ01-ARGUMENT.                                            
           05  CALL-QSNDDTAQ01-LIBRARY        PIC X(010).                       
           05  CALL-QSNDDTAQ01-DTAQ01-NAME      PIC X(010).                     
           05  CALL-QSNDDTAQ01-DTAQ01-LENGTH    PIC S9(005) COMP-3.             
           05  CALL-QSNDDTAQ01-DATA           PIC X(2000).                      
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
++++++     CLOSE PPRLT0P PPSYS0P SECSAP PPRLT0L1.                               
           GOBACK.                                                              
      *                                                                         
       1000-INIT-RTN.                                                           
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
  .            OPEN I-O PPRLT0P                                                 
  .            IF  NOT PPRLT0P-NORMAL                                           
  .                MOVE "E" TO PROCESS-SW                                       
  .                MOVE "FIO0010"           TO LINK-MSG-CODE                    
  .                MOVE "PPRLT0P   "        TO LINK-MSG-FILE-NAME               
  .                MOVE PPRLT0P-FILE-STATUS TO LINK-MSG-FILE-STATUS             
  .            END-IF                                                           
++++++     END-IF.                                                              
++++++     IF  PROCESS-NORMAL                                                   
  .            OPEN INPUT PPRLT0L1                                              
  .            IF  NOT PPRLT0L1-NORMAL                                          
  .                MOVE "E" TO PROCESS-SW                                       
  .                MOVE "FIO0010"           TO LINK-MSG-CODE                    
  .                MOVE "PPRLT0L1 "        TO LINK-MSG-FILE-NAME                
  .                MOVE PPRLT0L1-FILE-STATUS TO LINK-MSG-FILE-STATUS            
  .            END-IF                                                           
++++++     END-IF.                                                              
++++++     IF  PROCESS-NORMAL                                                   
  .            OPEN INPUT PPSYS0P                                               
  .            IF  NOT PPSYS0P-NORMAL                                           
  .                MOVE "E" TO PROCESS-SW                                       
  .                MOVE "FIO0020"           TO LINK-MSG-CODE                    
  .                MOVE "PPSYS0P   "        TO LINK-MSG-FILE-NAME               
  .                MOVE PPSYS0P-FILE-STATUS TO LINK-MSG-FILE-STATUS             
  .            END-IF                                                           
++++++     END-IF.                                                              
++++++     IF  PROCESS-NORMAL                                                   
  .            OPEN INPUT SECSAP                                                
  .            IF  NOT SECSAP-NORMAL                                            
  .                MOVE "E" TO PROCESS-SW                                       
  .                MOVE "FIO0010"           TO LINK-MSG-CODE                    
  .                MOVE "SECSAP    "        TO LINK-MSG-FILE-NAME               
  .                MOVE SECSAP-FILE-STATUS TO LINK-MSG-FILE-STATUS              
  .            END-IF                                                           
++++++     END-IF.                                                              
           IF  PROCESS-NORMAL                                                   
               INITIALIZE PRIVATE-WORK-AREA CALL-PANGCL040-ARGUMENT             
                          DSPF-DDS-FORMAT PPRLT0PR IN PPRLT0P-REC               
      *           
      *    USER ID AND JOB NAME                                         
      *           
               CALL "PANGCL150" USING CALL-PANGCL150-ARGUMENT                   
               CANCEL "PANGCL150"                                               
               CALL "PANGSUB110" USING CALL-PANGSUB110-ARGUMENT                 
               CANCEL "PANGSUB110"                                              
      *           
      *   Ｗ鞈撖                      
      *           
               MOVE LINK-TITLE-TEXT          TO TITLE1 OF HEADING-O             
               MOVE LINK-PROGRAM-NAME        TO TITLE2 OF HEADING-O             
               MOVE LINK-PROGRAM-CODE        TO PGMCDE OF HEADING-O             
               MOVE LINK-WORK-ENVIRONMENT    TO WRKENV OF HEADING-O             
      *           
      *    ENTRY-MODE 甇                        
      *           
++++++         MOVE 9 TO MAXIMUM-PAGE                                           
               MOVE 1 TO PAGE-COUNT                                             
               MOVE 1 TO RCDNBRCSR OF LISTCTL-O                                 
               PERFORM 9000-START-PPRLT0P-RTN                                   
      *                                                               
               INITIALIZE PPSYS0PR                                              
               MOVE LINK-BRANCH-CODE TO SYS0010                                 
               PERFORM 9500-READ-PPSYS0P-RANDOM-RTN                             
               CALL "PANGSUB500" USING CALL-PANGSUB500-ARGUMENT                 
           END-IF.                                                              
      *                                                                         
       2000-PROCESS-RTN.                                                        
           MOVE "N" TO RECORD-LOCK-SW.                                          
           PERFORM 3000-CLEAR-SUBFILE.                                          
      *           
      *    SUBFILE  MAX-SFL-SIZE(DFT:15)              
      *           
           PERFORM  UNTIL I >= MAX-SFL-SIZE   OR ,                              
                          PPRLT0P-AT-END      OR ,                              
                          PPRLT0P-NOT-FOUND   OR ,                              
                          NOT PROCESS-NORMAL                                    
             PERFORM 9100-READ-PPRLT0P-NEXT-RTN                                 
             IF  PROCESS-NORMAL                                                 
                 IF  NOT PPRLT0P-AT-END                                         
                     ADD 1 TO I                                                 
                     MOVE CORR PPRLT0PR IN PPRLT0P-REC TO                       
                               EXTERNALLY-KEY                                   
                     MOVE EXTERNALLY-KEY      TO SUBFILE-KEY(I)                 
                     INITIALIZE LISTSFL-O                                       
                     MOVE CORR PPRLT0PR IN PPRLT0P-REC TO LISTSFL-O             
                     INITIALIZE CALL-SEREDCS0B-ARGUMENT                         
                     MOVE LINK-BRANCH-CODE      TO CS0010                       
                     MOVE RLT0020 IN LISTSFL-O  TO CS0020                       
                     MOVE "Y" TO SEREDCS0B-STATUS                               
                     CALL "SEREDCS0B" USING SEREDCS0B-STATUS                    
                                            SECS0P-RECORD                       
  +                  MOVE CS0030 TO RLT0020NAM IN LISTSFL-O                     
      *            鞎                         
      *              IF  RLT0240 IN LISTSFL-O = SPACES                          
      *                  IF  RLT0A40 IN PPRLT0P-REC NOT = SPACES                
      *                      MOVE RLT0A40 IN PPRLT0P-REC TO                     
      *                           RLT0240 IN LISTSFL-O                          
      *                      MOVE RLT0A30 IN PPRLT0P-REC TO                     
      *                           RLT0230 IN LISTSFL-O                          
      *                  ELSE                                                   
      *                      IF  RLT0B40 IN PPRLT0P-REC NOT = SPACES            
      *                          MOVE RLT0B40 IN PPRLT0P-REC TO                 
      *                               RLT0240 IN LISTSFL-O                      
      *                          MOVE RLT0B30 IN PPRLT0P-REC TO                 
      *                               RLT0230 IN LISTSFL-O                      
      *                      END-IF                                             
      *                  END-IF                                                 
      *              END-IF                                                     
                     INITIALIZE CALL-SEREDCS0B-ARGUMENT                         
                     MOVE LINK-BRANCH-CODE      TO CS0010                       
                     MOVE RLT0240 IN LISTSFL-O  TO CS0020                       
                     MOVE "Y" TO SEREDCS0B-STATUS                               
                     CALL "SEREDCS0B" USING SEREDCS0B-STATUS                    
                                            SECS0P-RECORD                       
  +                  MOVE CS0030 TO RLT0240NAM IN LISTSFL-O                     
                     INITIALIZE CALL-SEREDSA0B-ARGUMENT                         
                     MOVE LINK-BRANCH-CODE        TO SA0010                     
                     MOVE RLT0230 IN LISTSFL-O    TO SA0020                     
                     CALL "SEREDSA0B" USING SEREDSA0B-STATUS                    
                                            SESA0P-RECORD                       
  +                  MOVE SA0030 TO RLT0230 IN LISTSFL-O                        
                     IF  RLT0130 IN PPRLT0P-REC = "N"                           
                         IF  RLT0210 IN PPRLT0P-REC = "Y"  AND ,                
                             RLT0211 IN PPRLT0P-REC = "N"                       
  +                          MOVE "Y" TO SWITCH1 IN LISTSFL-O                   
  +                      END-IF                                                 
                         IF  RLT0410 IN PPRLT0P-REC = "Y"  AND ,                
                             RLT0411 IN PPRLT0P-REC = "N"                       
  +                          MOVE "Y" TO SWITCH2 IN LISTSFL-O                   
  +                      END-IF                                                 
                         IF  RLT0510 IN PPRLT0P-REC = "Y"  AND ,                
                             RLT0511 IN PPRLT0P-REC = "N"                       
  +                          MOVE "Y" TO SWITCH3 IN LISTSFL-O                   
  +                      END-IF                                                 
                         IF  RLT0610 IN PPRLT0P-REC = "Y"  AND ,                
                             RLT0611 IN PPRLT0P-REC = "N"                       
  +                          MOVE "Y" TO SWITCH4 IN LISTSFL-O                   
  +                      END-IF                                                 
                         IF  CS0500 = "4"                                       
  +                          MOVE "Y" TO SWITCH5 IN LISTSFL-O                   
  +                      END-IF                                                 
                         IF  (RLT0A10 IN PPRLT0P-REC = "Y"  AND ,               
                              RLT0A11 IN PPRLT0P-REC = "N")       OR ,          
  +                          (SWITCH1 IN LISTSFL-O = "Y"    AND ,               
                              RLT0290 IN PPRLT0P-REC = "Y")                     
  +                          MOVE "Y" TO SWITCH6 IN LISTSFL-O                   
  +                      END-IF                                                 
      *                  IF  (RLT0B10 IN PPRLT0P-REC = "Y"  AND ,               
      *                       RLT0B11 IN PPRLT0P-REC = "N")       OR ,          
  +   *                      (SWITCH1 IN LISTSFL-O = "Y"    AND ,               
      *                       RLT0291 IN PPRLT0P-REC = "Y")                     
  +   *                      MOVE "Y" TO SWITCH7 IN LISTSFL-O                   
  +   *                  END-IF                                                 
                     ELSE                                                       
  +                      MOVE "*" TO SWITCH0 IN LISTSFL-O                       
  +                  END-IF                                                     
      *           
      *   頦                                          
      *           
                     IF  I = 5  OR 10                                           
                         MOVE INDIC-ON  TO IN26 OF LISTSFL-O-INDIC              
                     ELSE                                                       
                         MOVE INDIC-OFF TO IN26 OF LISTSFL-O-INDIC              
                     END-IF                                                     
                     MOVE I TO RELATIVE-KEY-VALUE                               
                     WRITE SUBFILE DSPF-REC FROM   LISTSFL-O                    
                                            FORMAT "LISTSFL"                    
                                            INDICATORS LISTSFL-O-INDIC          
                 END-IF                                                         
             END-IF                                                             
           END-PERFORM.                                                         
      *           
      *   孵脰 SUBFILE                          
      *           
           IF  I =  0                                                           
               INITIALIZE LISTSFL-O EXTERNALLY-KEY                              
               MOVE 1 TO I RELATIVE-KEY-VALUE                                   
               MOVE EXTERNALLY-KEY TO SUBFILE-KEY(I)                            
               WRITE SUBFILE DSPF-REC FROM   LISTSFL-O                          
                                      FORMAT "LISTSFL"                          
           END-IF.                                                              
           MOVE "L" TO PROCESS-MODE-SW.                                         
           MOVE "N" TO PROCESS-SW.                                              
           MOVE "N" TO RESET-SUBFILE-SW.                                        
      *           
      *     LIST-MODE  DISPLAY FILE I/O LOOP                          
      *         1. END-PROGRAM                                      
      *         2. PROCESS-NORMAL                                   
      *         3. RESET-SUBFILE      SUBFILE                   
      *           
           PERFORM  UNTIL END-PROGRAM       OR ,                                
                          PROCESS-NORMAL    OR ,                                
                          RESET-SUBFILE                                         
             PERFORM 4000-DISPLAY-ACCEPT-SCREEN                                 
             PERFORM 5000-CHECK-FUNCTION-KEY                                    
           END-PERFORM.                                                         
      *           
      *     ENTRY-MODE  DISPLAY FILE I/O LOOP                         
      *         1. END-PROGRAM                                      
      *         2. PROCESS-NORMAL     LIST-MODE LOOP            
      *           
           IF  PROCESS-NORMAL         AND ,                                     
               NOT RESET-SUBFILE                                                
               INITIALIZE CALL-PANGCL040-ARGUMENT                               
               MOVE "Y" TO RECORD-LOCK-SW                                       
               MOVE "N" TO PROCESS-SW                                           
               MOVE 1 TO PAGE-COUNT                                             
               PERFORM  UNTIL END-PROGRAM       OR ,                            
                              PROCESS-NORMAL                                    
                 PERFORM 4000-DISPLAY-ACCEPT-SCREEN                             
                 PERFORM 5000-CHECK-FUNCTION-KEY                                
               END-PERFORM                                                      
           END-IF.                                                              
      *           
      *    START PF & RESET SUBFILE                                     
      *           
           IF  NOT END-PROGRAM                                                  
               MOVE "Y" TO PROCESS-SW                                           
               PERFORM 9000-START-PPRLT0P-RTN                                   
               IF  PPRLT0P-NOT-FOUND                                            
                   PERFORM 9400-READ-PPRLT0P-LAST-RTN                           
                   PERFORM 9000-START-PPRLT0P-RTN                               
               END-IF                                                           
           END-IF.                                                              
      *                                                                         
      *           
      *    SUBFILE                                                      
      *           
      *  SUBFILE                                                            
       3000-CLEAR-SUBFILE.                                                      
           INITIALIZE SUBFILE-TABLE I.                                          
           MOVE INDIC-ON TO IN25 OF LISTCTL-O-INDIC.                            
           WRITE DSPF-REC FORMAT     "LISTCTL"                                  
                          INDICATORS LISTCTL-O-INDIC.                           
           MOVE INDIC-OFF TO IN25 OF LISTCTL-O-INDIC.                           
      *                                                                         
       4000-DISPLAY-ACCEPT-SCREEN.                                              
      *           
      *                                                         
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
      *                                                         
      *           
           MOVE INDIC-ON TO IN79 OF HEADING-O-INDIC.                            
           EVALUATE TRUE                                                        
             WHEN LIST-MODE                                                     
                  MOVE LIST-MODE-TEXT    TO WRKMOD OF HEADING-O                 
                  MOVE INDIC-OFF TO IN79 OF HEADING-O-INDIC                     
             WHEN ADD-MODE                                                      
                  MOVE ADD-MODE-TEXT     TO WRKMOD OF HEADING-O                 
             WHEN CHANGE-MODE                                                   
                  MOVE CHANGE-MODE-TEXT  TO WRKMOD OF HEADING-O                 
           END-EVALUATE.                                                        
           EVALUATE PAGE-COUNT                                                  
++++++       WHEN 1                                                             
  .               MOVE " 1/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "                    " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 2                                                             
  .               MOVE " 2/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "    頦萇    " TO                    
                       PAGETITLE IN HEADING-O                                   
                  IF  PANGCL150-USER-ID = "S0001178"   OR ,                     
                      PANGCL150-USER-ID = "S0004021"   OR ,                     
                      PANGCL150-USER-ID = "S0000542"                            
  .                   MOVE INDIC-ON TO IN41 OF DATAPAGE2-O-INDIC                
                  END-IF                                                        
++++++       WHEN 3                                                             
  .               MOVE " 3/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "                " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 4                                                             
  .               MOVE " 4/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "     寥２     " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 5                                                             
  .               MOVE " 5/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "     蝞     " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 6                                                             
  .               MOVE " 6/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "  剝拍Ｗ  " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 7                                                             
  .               MOVE " 7/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "    閮暸    " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 8                                                             
  .               MOVE " 8/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "      閮暸      " TO                    
                       PAGETITLE IN HEADING-O                                   
++++++       WHEN 9                                                             
  .               MOVE " 9/9  " TO PAGENUMER IN HEADING-O                     
  .               MOVE "      鞊ＹＸ      " TO                    
                       PAGETITLE IN HEADING-O                                   
           END-EVALUATE.                                                        
      *           
      *    HEADING FORMAT PROMPT                                        
      *           
           WRITE DSPF-REC FROM   HEADING-O                                      
                          FORMAT "HEADING"                                      
                          INDICATORS HEADING-O-INDIC.                           
      *           
      *                                                           
      *           
           WRITE DSPF-REC FROM       MSGAREA-O                                  
                          FORMAT     "MSGAREA"                                  
                          INDICATORS MSGAREA-O-INDIC.                           
      *           
      *    LIST-MODE  COLUMN HEADING                                
      *                       SUBFILE                                       
      *                                                             
      *    LIST-MODE  SUBFILE                                       
      *           
           IF  LIST-MODE                                                        
               WRITE DSPF-REC FORMAT "COLHDGARA"                                
               WRITE DSPF-REC FROM   LISTCTL-O                                  
                              FORMAT "LISTCTL"                                  
                              INDICATORS LISTCTL-O-INDIC                        
               WRITE DSPF-REC FORMAT "LISTMODE"                                 
               READ DSPF INTO LISTCTL-I                                         
                              FORMAT "LISTCTL"                                  
           END-IF.                                                              
      *           
      *    ENTRY-MODE                                         
      *                       亦                        
      *                                                             
      *    ENTRY-MODE 亦                        
      *           
           IF  ENTRY-MODE                                                       
      *           
      *      擏擗啣餅                                 
      *           
               PERFORM 4500-GET-CODE-REFERENCE                                  
               WRITE DSPF-REC FROM       KEYAREA-O                              
                              FORMAT     "KEYAREA"                              
                              INDICATORS KEYAREA-O-INDIC                        
               EVALUATE PAGE-COUNT                                              
++++++           WHEN 1                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE1-O                         
  .                                  FORMAT "DATAPAGE1"                         
++++++                               INDICATORS DATAPAGE1-O-INDIC               
++++++           WHEN 2                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE2-O                         
  .                                  FORMAT "DATAPAGE2"                         
++++++                               INDICATORS DATAPAGE2-O-INDIC               
++++++           WHEN 3                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE3-O                         
  .                                  FORMAT "DATAPAGE3"                         
++++++                               INDICATORS DATAPAGE3-O-INDIC               
++++++           WHEN 4                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE4-O                         
  .                                  FORMAT "DATAPAGE4"                         
++++++                               INDICATORS DATAPAGE4-O-INDIC               
++++++           WHEN 5                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE5-O                         
  .                                  FORMAT "DATAPAGE5"                         
++++++                               INDICATORS DATAPAGE5-O-INDIC               
++++++           WHEN 6                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE6-O                         
  .                                  FORMAT "DATAPAGE6"                         
++++++                               INDICATORS DATAPAGE6-O-INDIC               
++++++           WHEN 7                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE7-O                         
  .                                  FORMAT "DATAPAGE7"                         
++++++                               INDICATORS DATAPAGE7-O-INDIC               
++++++           WHEN 8                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE8-O                         
  .                                  FORMAT "DATAPAGE8"                         
++++++                               INDICATORS DATAPAGE8-O-INDIC               
++++++           WHEN 9                                                         
  .                   WRITE DSPF-REC FROM   DATAPAGE9-O                         
  .                                  FORMAT "DATAPAGE9"                         
++++++                               INDICATORS DATAPAGE9-O-INDIC               
               END-EVALUATE                                                     
      *           
      *    ENTRY-MODE                                       
      *           
               IF  MAXIMUM-PAGE > 1                                             
                   MOVE INDIC-ON  TO IN77 OF ADDMODE-O-INDIC                    
                   MOVE INDIC-ON  TO IN77 OF CHANGEMODE-O-INDIC                 
               ELSE                                                             
                   MOVE INDIC-OFF TO IN77 OF ADDMODE-O-INDIC                    
                   MOVE INDIC-OFF TO IN77 OF CHANGEMODE-O-INDIC                 
               END-IF                                                           
               IF  ADD-MODE                                                     
                   WRITE DSPF-REC FORMAT "ADDMODE"                              
                                  INDICATORS ADDMODE-O-INDIC                    
               ELSE                                                             
                   WRITE DSPF-REC FORMAT "CHANGEMODE"                           
                                  INDICATORS CHANGEMODE-O-INDIC                 
               END-IF                                                           
               EVALUATE PAGE-COUNT                                              
++++++           WHEN 1                                                         
  .                   READ DSPF INTO   DATAPAGE1-I                              
++++++                          FORMAT "DATAPAGE1"                              
++++++           WHEN 2                                                         
  .                   READ DSPF INTO   DATAPAGE2-I                              
++++++                          FORMAT "DATAPAGE2"                              
++++++           WHEN 3                                                         
  .                   READ DSPF INTO   DATAPAGE3-I                              
++++++                          FORMAT "DATAPAGE3"                              
++++++           WHEN 4                                                         
  .                   READ DSPF INTO   DATAPAGE4-I                              
++++++                          FORMAT "DATAPAGE4"                              
++++++           WHEN 5                                                         
  .                   READ DSPF INTO   DATAPAGE5-I                              
++++++                          FORMAT "DATAPAGE5"                              
++++++           WHEN 6                                                         
  .                   READ DSPF INTO   DATAPAGE6-I                              
++++++                          FORMAT "DATAPAGE6"                              
++++++           WHEN 7                                                         
  .                   READ DSPF INTO   DATAPAGE7-I                              
++++++                          FORMAT "DATAPAGE7"                              
++++++           WHEN 8                                                         
  .                   READ DSPF INTO   DATAPAGE8-I                              
++++++                          FORMAT "DATAPAGE8"                              
++++++           WHEN 9                                                         
  .                   READ DSPF INTO   DATAPAGE9-I                              
++++++                          FORMAT "DATAPAGE9"                              
               END-EVALUATE                                                     
           END-IF.                                                              
           MOVE 1 TO RCDNBRCSR OF LISTCTL-O.                                    
           INITIALIZE CALL-PANGCL040-ARGUMENT.                                  
      *                                                                         
       4500-GET-CODE-REFERENCE.                                                 
      *           
      *   ENTRY MODE                                                      
      *              擏擗啣餅                         
      *           
           INITIALIZE CALL-SEREDCS0B-ARGUMENT.                                  
           MOVE LINK-BRANCH-CODE      TO CS0010.                                
           MOVE RLT0020 IN KEYAREA-O  TO CS0020.                                
           MOVE "Y" TO SEREDCS0B-STATUS.                                        
           CALL "SEREDCS0B" USING SEREDCS0B-STATUS SECS0P-RECORD.               
           MOVE CS0030 TO RLT0020NAM IN KEYAREA-O.                              
           IF  PAGE-COUNT > 1                                                   
               INITIALIZE CALL-SEREDCS0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE       TO CS0010                            
               MOVE RLT0240 IN DATAPAGE1-O TO CS0020                            
                                              RLT0240 IN KEYAREA-O              
               MOVE "Y" TO SEREDCS0B-STATUS                                     
               CALL "SEREDCS0B" USING SEREDCS0B-STATUS SECS0P-RECORD            
               MOVE CS0030 TO RLT0240NAM IN KEYAREA-O                           
               MOVE INDIC-ON  TO IN28 OF KEYAREA-O-INDIC                        
           ELSE                                                                 
               MOVE INDIC-OFF TO IN28 OF KEYAREA-O-INDIC                        
           END-IF.                                                              
                                                                                
           INITIALIZE CALL-PANGCAL510-ARGUMENT WK-DAY-RATE                      
                      CALL-PANGCAL520-ARGUMENT.                                 
           MOVE "A"                    TO PANGCAL510-ACTION.                    
           MOVE LINK-BRANCH-CODE       TO PANGCAL510-TX0010                     
                                          PANGCAL520-TX0010.                    
           MOVE RLT0020 IN KEYAREA-O   TO PANGCAL510-TX0020                     
                                          PANGCAL520-TX0020.                    
           MOVE PANGSUB500-TW-DATE     TO PANGCAL510-TX0040                     
                                          PANGCAL520-TX0040.                    
           MOVE " "                    TO PANGCAL510-TX0210.                    
           MOVE RLT0230 IN DATAPAGE2-O TO PANGCAL510-TX0220.                    
           MOVE "1"                    TO PANGCAL510-TX0240.                    
           MOVE "02"                   TO PANGCAL520-CSJ030.                    
           CALL "PANGCAL510" USING CALL-PANGCAL510-ARGUMENT.                    
           CALL "PANGCAL520" USING CALL-PANGCAL520-ARGUMENT.                    
           IF  PANGCAL520-HAVE-SECSJ06                                          
               MOVE "  " TO HEADING4                                      
      *        MOVE 0.0007125 TO RATE-2                                         
           END-IF.                                                              
           IF  PANGCAL510-HAVE-SEIR6P                                           
               MOVE "  " TO HEADING3                                      
      *        MOVE 0.0007125 TO RATE-1                                         
           END-IF.                                                              
           IF  PANGCAL510-HAVE-SEIR2P                                           
               MOVE " 怠瞍脫 " TO HEADING1                                  
               MOVE PANGCAL510-IR2130 TO RATE-1                                 
           END-IF.                                                              
           INITIALIZE CALL-PANGCAL510-ARGUMENT.                                 
           MOVE "2"                    TO PANGCAL510-ACTION.                    
           MOVE LINK-BRANCH-CODE       TO PANGCAL510-TX0010.                    
           MOVE RLT0020 IN KEYAREA-O   TO PANGCAL510-TX0020.                    
           MOVE PANGSUB500-TW-DATE     TO PANGCAL510-TX0040.                    
           MOVE "W"                    TO PANGCAL510-TX0210.                    
           MOVE RLT0230 IN DATAPAGE2-O TO PANGCAL510-TX0220.                    
           MOVE "1"                    TO PANGCAL510-TX0240.                    
           CALL "PANGCAL510" USING CALL-PANGCAL510-ARGUMENT.                    
           IF  PANGCAL510-HAVE-SEIR2P                                           
               MOVE " 瞍脫 " TO HEADING2                                  
               MOVE PANGCAL510-IR2130 TO RATE-2                                 
           END-IF.                                                              
                                                                                
  +        EVALUATE PAGE-COUNT                                                  
  +              WHEN 1                                                         
                      INITIALIZE CALL-SEREDCS0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE       TO CS0010                     
                      MOVE RLT0240 IN DATAPAGE1-O TO CS0020                     
                      MOVE "Y" TO SEREDCS0B-STATUS                              
                      CALL "SEREDCS0B" USING SEREDCS0B-STATUS                   
                                             SECS0P-RECORD                      
                      MOVE CS0030 TO RLT0240NAM IN DATAPAGE1-O                  
                      INITIALIZE SECSAP-REC CSA050NAM IN DATAPAGE1-O            
                      MOVE LINK-BRANCH-CODE      TO CSA010                      
                      MOVE RLT0020 IN KEYAREA-O  TO CSA020                      
                      PERFORM 9000-START-SECSAP-RTN                             
                      PERFORM UNTIL NOT SECSAP-NORMAL       OR ,                
                                    CSA020 NOT =                                
                                    RLT0020 IN KEYAREA-O     OR ,               
                                    CSA050NAM IN DATAPAGE1-O NOT =              
                                    SPACES                                      
                        PERFORM 9100-READ-SECSAP-NEXT-RTN                       
                        IF  SECSAP-NORMAL                  AND ,                
                            CSA020 = RLT0020 IN KEYAREA-O  AND ,                
                            CSA050 NOT = SPACES            AND ,                
                            CSA120 = 0                                          
                            MOVE CSA050 TO CSA050NAM IN DATAPAGE1-O             
                        END-IF                                                  
                      END-PERFORM                                               
  +              WHEN 2                                                         
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0230 IN DATAPAGE2-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0230NAM IN DATAPAGE2-O                  
                      MOVE WK-DAY-RATE TO DAYRATE IN DATAPAGE2-O                
  +              WHEN 3                                                         
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0430 IN DATAPAGE3-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0430NAM IN DATAPAGE3-O                  
  +              WHEN 4                                                         
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0530 IN DATAPAGE4-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0530NAM IN DATAPAGE4-O                  
  +              WHEN 5                                                         
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0630 IN DATAPAGE5-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0630NAM IN DATAPAGE5-O                  
  +   *          WHEN 6                                                         
      *               IF  RLT0870 IN PPRLT0P-REC = "C"                          
      *                   INITIALIZE CALL-SEREDCS0B-ARGUMENT                    
      *                   MOVE LINK-BRANCH-CODE       TO CS0010                 
      *                   MOVE RLT0875 IN DATAPAGE6-O TO CS0020                 
      *                   MOVE "Y" TO SEREDCS0B-STATUS                          
      *                   CALL "SEREDCS0B" USING SEREDCS0B-STATUS               
      *                                          SECS0P-RECORD                  
      *                   MOVE CS0030 TO RLT0875NAM IN DATAPAGE6-O              
      *               END-IF                                                    
      *               IF  RLT0870 IN PPRLT0P-REC = "S"                          
      *                   INITIALIZE CALL-SEREDSA0B-ARGUMENT                    
      *                   MOVE LINK-BRANCH-CODE        TO SA0010                
      *                   MOVE RLT0875 IN DATAPAGE6-O  TO SA0020                
      *                   CALL "SEREDSA0B" USING SEREDSA0B-STATUS               
      *                                          SESA0P-RECORD                  
      *                   MOVE SA0030 TO RLT0875NAM IN DATAPAGE6-O              
      *               END-IF                                                    
  +              WHEN 7                                                         
      *               INITIALIZE CALL-SEREDCS0B-ARGUMENT                        
      *               MOVE LINK-BRANCH-CODE       TO CS0010                     
      *               MOVE RLT0A40 IN DATAPAGE7-O TO CS0020                     
      *               MOVE "Y" TO SEREDCS0B-STATUS                              
      *               CALL "SEREDCS0B" USING SEREDCS0B-STATUS                   
      *                                      SECS0P-RECORD                      
      *               MOVE CS0030 TO RLT0A40NAM IN DATAPAGE7-O                  
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0A30 IN DATAPAGE7-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0A30NAM IN DATAPAGE7-O                  
                      MOVE WK-DAY-RATE TO DAYRATE IN DATAPAGE7-O                
  +              WHEN 8                                                         
      *               INITIALIZE CALL-SEREDCS0B-ARGUMENT                        
      *               MOVE LINK-BRANCH-CODE       TO CS0010                     
      *               MOVE RLT0B40 IN DATAPAGE8-O TO CS0020                     
      *               MOVE "Y" TO SEREDCS0B-STATUS                              
      *               CALL "SEREDCS0B" USING SEREDCS0B-STATUS                   
      *                                      SECS0P-RECORD                      
      *               MOVE CS0030 TO RLT0B40NAM IN DATAPAGE8-O                  
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0B30 IN DATAPAGE8-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0B30NAM IN DATAPAGE8-O                  
                 WHEN 9                                                         
                      INITIALIZE CALL-SEREDSA0B-ARGUMENT                        
                      MOVE LINK-BRANCH-CODE        TO SA0010                    
                      MOVE RLT0733 IN DATAPAGE9-O  TO SA0020                    
                      CALL "SEREDSA0B" USING SEREDSA0B-STATUS                   
                                             SESA0P-RECORD                      
                      MOVE SA0030 TO RLT0733NAM IN DATAPAGE9-O                  
           END-EVALUATE.                                                        
      *                                                                         
       5000-CHECK-FUNCTION-KEY.                                                 
      *           
      *   LIST MODE                                                       
      *               WITH SUBFILE                              *SUBFILEBile鞊糎
      *   ENTRY MODE                                                      
      *              寥                       
      *           
           IF  PAGE-UP                                                          
               IF  LIST-MODE                                                    
                   MOVE SUBFILE-KEY(1) TO EXTERNALLY-KEY                        
                   MOVE CORR EXTERNALLY-KEY TO PPRLT0PR IN PPRLT0P-REC          
                   PERFORM 9000-START-PPRLT0P-RTN                               
                   MOVE 0 TO J                                                  
                   PERFORM  UNTIL J > MAX-SFL-SIZE     OR ,                     
                                  NOT PPRLT0P-NORMAL                            
                     PERFORM 9200-READ-PPRLT0P-PRIOR-RTN                        
                     IF  PPRLT0P-NORMAL                                         
                         ADD 1 TO J                                             
                     END-IF                                                     
                   END-PERFORM                                                  
                   IF  J = 0                                                    
                       MOVE "NTF0070" TO PANGCL040-MESSAGE-CODE-O               
                       MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                  
                       MOVE PPRLT0P-FILE-STATUS TO                              
                            PANGCL040-FILE-STATUS-O                             
                   ELSE                                                         
                       MOVE "Y" TO RESET-SUBFILE-SW                             
                   END-IF                                                       
               END-IF                                                           
      *                                                                         
               IF  ENTRY-MODE                                                   
                   MOVE ZEROS TO WK-LEGAL-PAGE                                  
                   PERFORM UNTIL LEGAL-PAGE                                     
                     IF  PAGE-COUNT = 1                                         
                         MOVE MAXIMUM-PAGE TO PAGE-COUNT WK-LEGAL-PAGE          
                     ELSE                                                       
                         SUBTRACT 1 FROM PAGE-COUNT                             
                         MOVE PAGE-COUNT TO WK-LEGAL-PAGE                       
                     END-IF                                                     
                   END-PERFORM                                                  
                   SET SCREEN-INPUT-TO-OUTPUT-PAGE TO TRUE                      
                   PERFORM 8000-FIELD-MAP-MOVE                                  
               END-IF                                                           
           END-IF.                                                              
      *           
      *   LIST MODE                                                       
      *               WITH SUBFILE                                    
      *   ENTRY MODE                                                      
      *              寥                       
      *           
           IF  PAGE-DOWN                                                        
               IF  LIST-MODE                                                    
                   IF  I < MAX-SFL-SIZE                                         
                       MOVE "NTF0080" TO PANGCL040-MESSAGE-CODE-O               
                       MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                  
                       MOVE PPRLT0P-FILE-STATUS TO                              
                            PANGCL040-FILE-STATUS-O                             
                   ELSE                                                         
                       MOVE SUBFILE-KEY(I) TO EXTERNALLY-KEY                    
                       MOVE CORR EXTERNALLY-KEY TO                              
                                 PPRLT0PR IN PPRLT0P-REC                        
                       MOVE "Y" TO RESET-SUBFILE-SW                             
                   END-IF                                                       
               END-IF                                                           
               IF  ENTRY-MODE                                                   
                   MOVE ZEROS TO WK-LEGAL-PAGE                                  
                   PERFORM UNTIL LEGAL-PAGE                                     
                     IF  PAGE-COUNT = MAXIMUM-PAGE                              
                         MOVE 1 TO PAGE-COUNT WK-LEGAL-PAGE                     
                     ELSE                                                       
                         ADD 1 TO PAGE-COUNT                                    
                         MOVE PAGE-COUNT TO WK-LEGAL-PAGE                       
                     END-IF                                                     
                   END-PERFORM                                                  
                   SET SCREEN-INPUT-TO-OUTPUT-PAGE TO TRUE                      
                   PERFORM 8000-FIELD-MAP-MOVE                                  
               END-IF                                                           
           END-IF.                                                              
      *           
      *   LIST MODE                                                       
      *             SEND-KEY (USE NULL RECORD TO ADD IF ADD-MODE)           
      *             PF1 (COPY A SAMPLE RECORD TO ADD IF ADD-MODE)           
      *   ENTRY MODE                                                      
      *             SEND-KEY (TO WRITE OR REWRITE RECORD)                   
      *           
           IF  ENTER-KEY   OR ,                                                 
               PF1                                                              
               PERFORM 5500-CHECK-ENTER-DATA                                    
               IF  ERROR-COUNT = 0                                              
                   IF  LIST-MODE                                                
                       PERFORM 7000-INITIALIZE-FIELD                            
                       MOVE CORR LISTSFL-I TO PPRLT0PR IN PPRLT0P-REC           
                       PERFORM 9500-READ-PPRLT0P-RANDOM-RTN                     
                       MOVE CORR PPRLT0PR IN PPRLT0P-REC TO KEYAREA-O           
                       IF  PPRLT0P-NORMAL                                       
                           MOVE "Y" TO PROCESS-SW                               
                           MOVE "C" TO PROCESS-MODE-SW                          
                           MOVE PPRLT0P-REC TO WK-OLD-RECORD                    
                           SET FILE-TO-SCREEN-IO TO TRUE                        
                           PERFORM 8000-FIELD-MAP-MOVE                          
                       END-IF                                                   
                       IF  PPRLT0P-NOT-FOUND                                    
                           MOVE "Y" TO PROCESS-SW                               
                           MOVE "A" TO PROCESS-MODE-SW                          
                           MOVE CSRRRN OF LISTCTL-I TO J                        
                           INITIALIZE PPRLT0PR IN WK-OLD-RECORD                 
      *           
      *    PF1                                              
      *           
                           IF  PF1                                              
                               MOVE "N" TO RECORD-LOCK-SW                       
                               MOVE SUBFILE-KEY(J) TO EXTERNALLY-KEY            
                               MOVE CORR EXTERNALLY-KEY TO                      
                                         PPRLT0PR IN PPRLT0P-REC                
                               PERFORM 9500-READ-PPRLT0P-RANDOM-RTN             
                               MOVE "Y" TO RECORD-LOCK-SW                       
                               IF  PPRLT0P-NORMAL                               
                                   SET FILE-TO-SCREEN-IO TO TRUE                
                                   PERFORM 8000-FIELD-MAP-MOVE                  
                               END-IF                                           
                           END-IF                                               
                       END-IF                                                   
                   ELSE                                                         
                       MOVE "Y" TO PROCESS-SW                                   
      *           
      *   ENTRY MODE 儮 MOVE                      
      *           
                       MOVE CORR KEYAREA-O TO PPRLT0PR IN PPRLT0P-REC           
                       SET SCREEN-INPUT-TO-FILE TO TRUE                         
                       PERFORM 8000-FIELD-MAP-MOVE                              
                       MOVE PPRLT0P-REC TO WK-NEW-RECORD                        
      *                ACCEPT SYSTEM-DATE FROM DATE                             
                       CALL "PANGSUB500" USING CALL-PANGSUB500-ARGUMENT         
                       ACCEPT SYSTEM-TIME FROM TIME                             
      *   KGI-T  銋拇摹株芣鞊Ｗ砥                             
      *                PERFORM 8400-ADJUST-RLT0AB                               
                       IF  ADD-MODE                                             
                           MOVE "NTF0030" TO PANGCL040-MESSAGE-CODE-O           
                           MOVE "PPRLT0P   " TO PANGCL040-FILE-NAME-O           
                           PERFORM 9600-WRITE-PPRLT0P-RTN                       
                       ELSE                                                     
                           MOVE "NTF0040" TO PANGCL040-MESSAGE-CODE-O           
                           MOVE "PPRLT0P   " TO PANGCL040-FILE-NAME-O           
                           PERFORM 8300-SET-LAST-MODIFY-TIME                    
                           PERFORM 9700-REWRITE-PPRLT0P-RTN                     
                       END-IF                                                   
                       PERFORM 8500-WRITE-TO-JOURNAL                            
      *  104/06/30   Q                                                    
      *                PERFORM 8900-SEND-TO-DATAQ                               
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      *           
      *   蟡                                          
      *           
           IF  PF3                                                              
               MOVE "E" TO PROCESS-SW                                           
           END-IF.                                                              
      *           
      *    REFRESH                                              
      *           
           IF  PF5                                                              
               IF  LIST-MODE                                                    
                   MOVE "Y" TO RESET-SUBFILE-SW                                 
                   MOVE SUBFILE-KEY(1) TO EXTERNALLY-KEY                        
                   MOVE CORR EXTERNALLY-KEY TO PPRLT0PR IN PPRLT0P-REC          
               ELSE                                                             
                   SET FILE-TO-SCREEN-IO TO TRUE                                
                   PERFORM 8000-FIELD-MAP-MOVE                                  
               END-IF                                                           
           END-IF.                                                              
      *           
      *   ENTRY-MODE  CHANGE-MODE                         
      *           
           IF  PF7                                                              
               IF  NOT READ-PRIOR-END                                           
                   PERFORM 9200-READ-PPRLT0P-PRIOR-RTN                          
                   IF  PPRLT0P-NORMAL                                           
                       MOVE CORR PPRLT0PR IN PPRLT0P-REC TO KEYAREA-O           
                       MOVE PPRLT0P-REC TO WK-OLD-RECORD                        
                       SET FILE-TO-SCREEN-IO TO TRUE                            
                       PERFORM 8000-FIELD-MAP-MOVE                              
                       MOVE SPACES TO READ-AT-END-SW                            
                   END-IF                                                       
                   IF  PPRLT0P-AT-END                                           
                       MOVE "P" TO READ-AT-END-SW                               
                       MOVE "NTF0090" TO PANGCL040-MESSAGE-CODE-O               
                       MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                  
                       MOVE PPRLT0P-FILE-STATUS TO                              
                            PANGCL040-FILE-STATUS-O                             
                       PERFORM 9500-READ-PPRLT0P-RANDOM-RTN                     
                   END-IF                                                       
               ELSE                                                             
                   MOVE "NTF0090" TO PANGCL040-MESSAGE-CODE-O                   
                   MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                      
                   MOVE PPRLT0P-FILE-STATUS TO                                  
                        PANGCL040-FILE-STATUS-O                                 
               END-IF                                                           
           END-IF.                                                              
      *           
      *   ENTRY-MODE  CHANGE-MODE                         
      *           
           IF  PF8                                                              
               IF  NOT READ-NEXT-END                                            
                   PERFORM 9100-READ-PPRLT0P-NEXT-RTN                           
                   IF  PPRLT0P-NORMAL                                           
                       MOVE CORR PPRLT0PR IN PPRLT0P-REC TO KEYAREA-O           
                       MOVE PPRLT0P-REC TO WK-OLD-RECORD                        
                       SET FILE-TO-SCREEN-IO TO TRUE                            
                       PERFORM 8000-FIELD-MAP-MOVE                              
                       MOVE SPACES TO READ-AT-END-SW                            
                   END-IF                                                       
                   IF  PPRLT0P-AT-END                                           
                       MOVE "N" TO READ-AT-END-SW                               
                       MOVE "NTF0100" TO PANGCL040-MESSAGE-CODE-O               
                       MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                  
                       MOVE PPRLT0P-FILE-STATUS TO                              
                            PANGCL040-FILE-STATUS-O                             
                       PERFORM 9500-READ-PPRLT0P-RANDOM-RTN                     
                   END-IF                                                       
               ELSE                                                             
                   MOVE "NTF0100" TO PANGCL040-MESSAGE-CODE-O                   
                   MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                      
                   MOVE PPRLT0P-FILE-STATUS TO                                  
                        PANGCL040-FILE-STATUS-O                                 
               END-IF                                                           
           END-IF.                                                              
      *           
      *   LIST-MODE  RESEARCH A SPECIFIC RECORD                           
      *           
           IF  PF9                                                              
  .            IF  CSRRRN OF LISTCTL-I >= 1                                     
                   MOVE CSRRRN OF LISTCTL-I TO RELATIVE-KEY-VALUE               
                   READ SUBFILE DSPF INTO LISTSFL-I                             
                                     FORMAT "LISTSFL"                           
                   IF  DSPF-NORMAL                                              
                       MOVE CORR LISTSFL-I TO PPRLT0PR IN PPRLT0P-REC           
                       MOVE "Y" TO RESET-SUBFILE-SW                             
                   ELSE                                                         
                       MOVE "FIO0120" TO PANGCL040-MESSAGE-CODE-O               
                   END-IF                                                       
               ELSE                                                             
                   MOVE "NTF0130" TO PANGCL040-MESSAGE-CODE-O                   
               END-IF                                                           
           END-IF.                                                              
      *           
      *   LIST MODE                                                       
      *             蟡                                
      *   ENTRY MODE                                                      
      *              LIST-MODE                                            
      *           
           IF  PF12                                                             
               IF  LIST-MODE                                                    
                   MOVE "E" TO PROCESS-SW                                       
               END-IF                                                           
               IF  ENTRY-MODE                                                   
                   MOVE "Y" TO PROCESS-SW                                       
               END-IF                                                           
           END-IF.                                                              
      *           
      *   ENTRY MODE 擗                                       
      *           
           IF  PF16                                                             
               IF  ENTRY-MODE                                                   
                   MOVE "Y" TO PROCESS-SW                                       
                   MOVE "NTF0050" TO PANGCL040-MESSAGE-CODE-O                   
                   MOVE "PPRLT0P   " TO PANGCL040-FILE-NAME-O                   
      *            PERFORM 9800-DELETE-PPRLT0P-RTN                              
      * WRITE TO JOURNAL IF DELETE SUCCESSFULLY                                 
                   IF  PPRLT0P-NORMAL                                           
                       ACCEPT SYSTEM-DATE FROM DATE                             
                       ACCEPT SYSTEM-TIME FROM TIME                             
                       INITIALIZE  CALL-PANGSUB440-ARGUMENT                     
                       MOVE LINK-PROGRAM-CODE     TO JRNPROGRAM                 
                       MOVE LINK-BRANCH-CODE      TO JRNBRANCH                  
                       MOVE SPACES                TO TARGETDATE                 
                       MOVE SYSTEM-DATE           TO JRNDATE                    
                       MOVE SYSTEM-TIME           TO JRNTIME                    
                       MOVE PANGCL150-USER-ID     TO JRNUSER                    
                       MOVE SPACES                TO JRNREASON                  
                       MOVE "DR"                  TO JRNTYPE                    
                       MOVE WK-OLD-RECORD         TO JRNRECORD                  
      *                CALL "PANGSUB440" USING CALL-PANGSUB440-ARGUMENT         
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      *           
      *                                                         
      *           
           IF  PF19                                                             
               PERFORM 9300-READ-PPRLT0P-FIRST-RTN                              
               IF  PPRLT0P-AT-END                                               
                   MOVE "NTF0110" TO PANGCL040-MESSAGE-CODE-O                   
                   MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                      
                   MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O          
               END-IF                                                           
               IF  PPRLT0P-NORMAL                                               
                   IF  LIST-MODE                                                
                       MOVE "Y" TO RESET-SUBFILE-SW                             
                   END-IF                                                       
                   IF  CHANGE-MODE                                              
                       MOVE CORR PPRLT0PR IN PPRLT0P-REC TO KEYAREA-O           
                       MOVE PPRLT0P-REC TO WK-OLD-RECORD                        
                       SET FILE-TO-SCREEN-IO TO TRUE                            
                       PERFORM 8000-FIELD-MAP-MOVE                              
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      *           
      *                                                         
      *           
           IF  PF20                                                             
               PERFORM 9400-READ-PPRLT0P-LAST-RTN                               
               IF  PPRLT0P-AT-END                                               
                   MOVE "NTF0120" TO PANGCL040-MESSAGE-CODE-O                   
                   MOVE "PPRLT0P" TO PANGCL040-FILE-NAME-O                      
                   MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O          
               END-IF                                                           
               IF  PPRLT0P-NORMAL                                               
                   IF  LIST-MODE                                                
                       MOVE "Y" TO RESET-SUBFILE-SW                             
                   END-IF                                                       
                   IF  CHANGE-MODE                                              
                       MOVE CORR PPRLT0PR IN PPRLT0P-REC TO KEYAREA-O           
                       MOVE PPRLT0P-REC TO WK-OLD-RECORD                        
                       SET FILE-TO-SCREEN-IO TO TRUE                            
                       PERFORM 8000-FIELD-MAP-MOVE                              
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
      *           
      *                                                                 
      *           
           IF  PF24                                                             
               MOVE FUNCTION-KEY TO LINK-RETURN-PF-KEY                          
               MOVE "E" TO PROCESS-SW                                           
           END-IF.                                                              
      *                                                                         
       5500-CHECK-ENTER-DATA.                                                   
           MOVE 0 TO ERROR-COUNT.                                               
      *           
      *    LIST-MODE                                  
      *           
           IF  LIST-MODE                                                        
               IF  CSRRRN OF LISTCTL-I >= 1                                     
                   MOVE CSRRRN OF LISTCTL-I TO RELATIVE-KEY-VALUE               
                   READ SUBFILE DSPF INTO LISTSFL-I                             
                                     FORMAT "LISTSFL"                           
                   IF  NOT DSPF-NORMAL                                          
                       MOVE "FIO0120" TO PANGCL040-MESSAGE-CODE-O               
                       ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
                   IF  DSPF-NORMAL                   AND ,                      
++++++                 RLT0020 OF LISTSFL-I = SPACES                            
                       MOVE "EDT0070" TO PANGCL040-MESSAGE-CODE-O               
                       ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
                   IF  DSPF-NORMAL                      AND ,                   
++++++                 RLT0020 IN LISTSFL-I NOT = SPACES                        
                       INITIALIZE CALL-SEREDCS0B-ARGUMENT                       
                       MOVE LINK-BRANCH-CODE     TO CS0010                      
                       MOVE RLT0020 IN LISTSFL-I TO CS0020                      
                       CALL "SEREDCS0B" USING SEREDCS0B-STATUS                  
                                              SECS0P-RECORD                     
                       IF  NOT SECS0P-NORMAL                                    
                           MOVE "EDT0280" TO PANGCL040-MESSAGE-CODE-O           
                           ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   MOVE "NTF0130" TO PANGCL040-MESSAGE-CODE-O                   
                   ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *           
      *    ENTRY-MODE                                 
      *           
++++++     IF  ENTRY-MODE                                                       
               PERFORM 6000-SET-FIELD-DEFAULT-VALUE                             
                                                                                
               SET SCREEN-INPUT-TO-OUTPUT TO TRUE                               
               PERFORM 8000-FIELD-MAP-MOVE                                      
                                                                                
  .            IF  RLT0130 IN DATAPAGE1-I = "Y"                                 
  .                IF  RLT0140 IN DATAPAGE1-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0140 IN DATAPAGE1-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 1 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN36 OF DATAPAGE1-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 1 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN36 OF DATAPAGE1-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
                                                                                
  .            IF  RLT0210 IN DATAPAGE2-I = "Y"   AND ,                         
  .                RLT0211 IN DATAPAGE2-I = "N"                                 
  .                IF  RLT0220 IN DATAPAGE2-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0220 IN DATAPAGE2-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 2 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN36 OF DATAPAGE2-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
      *            ELSE                                                         
  .   *                MOVE 2 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE2-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0210 IN DATAPAGE2-I = "Y"   AND ,                         
  .                RLT0211 IN DATAPAGE2-I = "N"                                 
  .                IF  RLT0221 IN DATAPAGE2-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0221 IN DATAPAGE2-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 2 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN37 OF DATAPAGE2-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
      *            ELSE                                                         
  .   *                MOVE 2 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE2-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0211 IN DATAPAGE2-I = "Y"                                 
  .                IF  RLT0222 IN DATAPAGE2-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0222 IN DATAPAGE2-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 2 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN38 OF DATAPAGE2-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 2 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN38 OF DATAPAGE2-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
               IF  RLT0250 IN DATAPAGE2-I = "M"                OR ,             
                   (RLT0250 IN DATAPAGE2-I = "S"        AND ,                   
                    RLT0230 IN DATAPAGE2-I NOT = SPACES)                        
                   INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
                   MOVE LINK-BRANCH-CODE       TO SA0010                        
                   MOVE RLT0230 IN DATAPAGE2-I TO SA0020                        
                   CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
                                          SESA0P-RECORD                         
                   IF  NOT SESA0P-NORMAL                                        
  .                    MOVE 2 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN39 IN DATAPAGE2-O-INDIC               
  .                    MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0224 IN DATAPAGE2-I NOT = ZEROS                           
                   INITIALIZE SECHKDATE-MSG-ID                                  
                   MOVE RLT0224 IN DATAPAGE2-I TO TEMP-YYYYMMDD                 
                   MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
                   CALL "SECHKDATE" USING SECHKDATE-DATE                        
                                          SECHKDATE-MSG-ID                      
                   IF  SECHKDATE-MSG-ID NOT = SPACES  OR ,                      
  .                    RLT0224 IN DATAPAGE2-I < RLT0220 IN DATAPAGE2-I          
  .                    MOVE 2 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN40 OF DATAPAGE2-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
      *        IF  RLT0130 IN PPRLT0P-REC NOT = "Y"           AND ,             
      *            ((RLT0210 IN PPRLT0P-REC = "Y" AND ,                         
      *              RLT0211 IN PPRLT0P-REC NOT = "Y")   OR ,                   
      *             (RLT0410 IN PPRLT0P-REC = "Y" AND ,                         
      *              RLT0411 IN PPRLT0P-REC NOT = "Y")   OR ,                   
      *             (RLT0510 IN PPRLT0P-REC = "Y" AND ,                         
      *              RLT0511 IN PPRLT0P-REC NOT = "Y")   OR ,                   
      *             (RLT0610 IN PPRLT0P-REC = "Y" AND ,                         
      *              RLT0611 IN PPRLT0P-REC NOT = "Y")   OR ,                   
      *             (RLT0710 IN PPRLT0P-REC = "Y" AND ,                         
      *              RLT0711 IN PPRLT0P-REC NOT = "Y"))                         
      *        IF  RLT0250 IN DATAPAGE1-I = "S"                                 
                   INITIALIZE CALL-SEREDCS0B-ARGUMENT                           
                   MOVE LINK-BRANCH-CODE       TO CS0010                        
                   MOVE RLT0240 IN DATAPAGE1-I TO CS0020                        
                   CALL "SEREDCS0B" USING SEREDCS0B-STATUS                      
                                          SECS0P-RECORD                         
                   IF  NOT SECS0P-NORMAL                                        
  .                    MOVE 1 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN37 OF DATAPAGE1-O-INDIC               
                       MOVE "EDT0280" TO PANGCL040-MESSAGE-CODE-O               
                       ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
      *        END-IF                                                           
                                                                                
  .            IF  RLT0240 IN DATAPAGE1-I NOT = RLT0020 IN KEYAREA-O            
                   INITIALIZE PPRLT0L1-REC                                      
                   MOVE RLT0240 IN DATAPAGE1-I TO                               
                         RLT0240 IN PPRLT0L1-REC                                
                   PERFORM 9500-READ-PPRLT0L1-RANDOM-RTN                        
                   IF  NOT PPRLT0L1-NORMAL                                      
                       MOVE 1 TO PAGE-COUNT                                     
.                      MOVE INDIC-ON TO IN37 OF DATAPAGE1-O-INDIC               
                       MOVE "EDT0580" TO PANGCL040-MESSAGE-CODE-O               
                       ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
                   INITIALIZE PPRLT0L1-REC                                      
                    MOVE RLT0020 IN KEYAREA-O TO                                
                         RLT0240 IN PPRLT0L1-REC                                
                   PERFORM 9500-READ-PPRLT0L1-RANDOM-RTN                        
                   IF  PPRLT0L1-NORMAL                AND ,                     
                       RLT0250 IN PPRLT0L1-REC = "S"                            
                       MOVE 1 TO PAGE-COUNT                                     
.                      MOVE INDIC-ON TO IN37 OF DATAPAGE1-O-INDIC               
                       MOVE "EDT0570" TO PANGCL040-MESSAGE-CODE-O               
                       ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
                                                                                
  .   *        IF  RLT0420 IN DATAPAGE3-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0420 IN DATAPAGE3-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 3 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE3-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0421 IN DATAPAGE3-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0421 IN DATAPAGE3-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 3 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE3-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0422 IN DATAPAGE3-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0422 IN DATAPAGE3-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 3 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN38 OF DATAPAGE3-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
      *        IF  RLT0430 IN DATAPAGE3-I NOT = SPACES                          
      *            INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO SA0010                        
      *            MOVE RLT0430 IN DATAPAGE3-I TO SA0020                        
      *            CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
      *                                   SESA0P-RECORD                         
      *            IF  NOT SESA0P-NORMAL                                        
  .   *                MOVE 3 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN39 IN DATAPAGE3-O-INDIC               
  .   *                MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0424 IN DATAPAGE3-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0424 IN DATAPAGE3-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES   OR ,                     
  .   *                RLT0424 IN DATAPAGE3-I < RLT0420 IN DATAPAGE3-I          
  .   *                MOVE 3 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN40 OF DATAPAGE3-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
                                                                                
  .   *        IF  RLT0520 IN DATAPAGE4-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0520 IN DATAPAGE4-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 4 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE4-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0521 IN DATAPAGE4-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0521 IN DATAPAGE4-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 4 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE4-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0522 IN DATAPAGE4-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0522 IN DATAPAGE4-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 4 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN38 OF DATAPAGE4-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
      *        IF  RLT0530 IN DATAPAGE4-I NOT = SPACES                          
      *            INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO SA0010                        
      *            MOVE RLT0530 IN DATAPAGE4-I TO SA0020                        
      *            CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
      *                                   SESA0P-RECORD                         
      *            IF  NOT SESA0P-NORMAL                                        
  .   *                MOVE 4 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN39 IN DATAPAGE4-O-INDIC               
  .   *                MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0524 IN DATAPAGE4-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0524 IN DATAPAGE4-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES   OR ,                     
  .   *                RLT0524 IN DATAPAGE4-I < RLT0520 IN DATAPAGE4-I          
  .   *                MOVE 4 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN40 OF DATAPAGE4-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
                                                                                
  .   *        IF  RLT0620 IN DATAPAGE5-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0620 IN DATAPAGE5-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 5 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE5-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0621 IN DATAPAGE5-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0621 IN DATAPAGE5-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 5 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE5-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0622 IN DATAPAGE5-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0622 IN DATAPAGE5-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 5 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN38 OF DATAPAGE5-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
      *        IF  RLT0630 IN DATAPAGE5-I NOT = SPACES                          
      *            INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO SA0010                        
      *            MOVE RLT0630 IN DATAPAGE5-I TO SA0020                        
      *            CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
      *                                   SESA0P-RECORD                         
      *            IF  NOT SESA0P-NORMAL                                        
  .   *                MOVE 5 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN39 IN DATAPAGE5-O-INDIC               
  .   *                MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0624 IN DATAPAGE5-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0624 IN DATAPAGE5-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES   OR ,                     
  .   *                RLT0624 IN DATAPAGE5-I < RLT0620 IN DATAPAGE5-I          
  .   *                MOVE 5 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN40 OF DATAPAGE5-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
                                                                                
  .   *        IF  RLT0720 IN DATAPAGE9-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            COMPUTE TEMP-YYYYMMDD = RLT0720 IN DATAPAGE9-I               
      *                                  * 100 + 1                              
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 9 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE9-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0721 IN DATAPAGE9-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0721 IN DATAPAGE9-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 9 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE9-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .   *        IF  RLT0722 IN DATAPAGE9-I NOT = ZEROS                           
      *            INITIALIZE SECHKDATE-MSG-ID                                  
      *            MOVE RLT0722 IN DATAPAGE9-I TO TEMP-YYYYMMDD                 
      *            MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
      *            CALL "SECHKDATE" USING SECHKDATE-DATE                        
      *                                   SECHKDATE-MSG-ID                      
      *            IF  SECHKDATE-MSG-ID NOT = SPACES                            
  .   *                MOVE 9 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN38 OF DATAPAGE9-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
      *        IF  RLT0733 IN DATAPAGE9-I NOT = SPACES                          
      *            INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO SA0010                        
      *            MOVE RLT0733 IN DATAPAGE9-I TO SA0020                        
      *            CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
      *                                   SESA0P-RECORD                         
      *            IF  NOT SESA0P-NORMAL                                        
  .   *                MOVE 9 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN39 IN DATAPAGE9-O-INDIC               
  .   *                MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
                                                                                
  .            IF  RLT0A10 IN DATAPAGE7-I = "Y"   AND ,                         
  .                RLT0A11 IN DATAPAGE7-I = "N"                                 
  .                IF  RLT0A20 IN DATAPAGE7-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0A20 IN DATAPAGE7-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 7 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN36 OF DATAPAGE7-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
      *            ELSE                                                         
  .   *                MOVE 7 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN36 OF DATAPAGE7-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0A10 IN DATAPAGE7-I = "Y"   AND ,                         
  .                RLT0A11 IN DATAPAGE7-I = "N"                                 
  .                IF  RLT0A21 IN DATAPAGE7-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0A21 IN DATAPAGE7-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 7 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN37 OF DATAPAGE7-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
      *            ELSE                                                         
  .   *                MOVE 7 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN37 OF DATAPAGE7-O-INDIC               
  .   *                MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .   *                ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0A11 IN DATAPAGE7-I = "Y"                                 
  .                IF  RLT0A22 IN DATAPAGE7-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0A22 IN DATAPAGE7-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 7 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN38 OF DATAPAGE7-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 7 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN38 OF DATAPAGE7-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
      *        IF  RLT0A50 IN DATAPAGE7-I = "M"                OR ,             
      *            (RLT0A50 IN DATAPAGE7-I = "S"        AND ,                   
      *             RLT0A30 IN DATAPAGE7-I NOT = SPACES)                        
               IF  RLT0A30 IN DATAPAGE7-I NOT = SPACES                          
                   INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
                   MOVE LINK-BRANCH-CODE       TO SA0010                        
                   MOVE RLT0A30 IN DATAPAGE7-I TO SA0020                        
                   CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
                                          SESA0P-RECORD                         
                   IF  NOT SESA0P-NORMAL                                        
  .                    MOVE 7 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN39 IN DATAPAGE7-O-INDIC               
  .                    MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0A24 IN DATAPAGE7-I NOT = ZEROS                           
                   INITIALIZE SECHKDATE-MSG-ID                                  
                   MOVE RLT0A24 IN DATAPAGE7-I TO TEMP-YYYYMMDD                 
                   MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
                   CALL "SECHKDATE" USING SECHKDATE-DATE                        
                                          SECHKDATE-MSG-ID                      
                   IF  SECHKDATE-MSG-ID NOT = SPACES  OR ,                      
  .                    RLT0A24 IN DATAPAGE7-I < RLT0A20 IN DATAPAGE7-I          
  .                    MOVE 7 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN40 OF DATAPAGE7-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
      *        IF  RLT0130 IN PPRLT0P-REC NOT = "Y"   AND ,                     
      *            RLT0A10 IN PPRLT0P-REC = "Y"       AND ,                     
      *            RLT0A11 IN PPRLT0P-REC NOT = "Y"                             
      *            INITIALIZE CALL-SEREDCS0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO CS0010                        
      *            MOVE RLT0A40 IN DATAPAGE7-I TO CS0020                        
      *            CALL "SEREDCS0B" USING SEREDCS0B-STATUS                      
      *                                   SECS0P-RECORD                         
      *            IF  NOT SECS0P-NORMAL                                        
  .   *                MOVE 7 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN41 OF DATAPAGE7-O-INDIC               
      *                MOVE "EDT0280" TO PANGCL040-MESSAGE-CODE-O               
      *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
                                                                                
  .            IF  RLT0B10 IN DATAPAGE8-I = "Y"   AND ,                         
  .                RLT0B11 IN DATAPAGE8-I = "N"                                 
  .                IF  RLT0B20 IN DATAPAGE8-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0B20 IN DATAPAGE8-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 8 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN36 OF DATAPAGE8-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 8 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN36 OF DATAPAGE8-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0B10 IN DATAPAGE8-I = "Y"   AND ,                         
  .                RLT0B11 IN DATAPAGE8-I = "N"                                 
  .                IF  RLT0B21 IN DATAPAGE8-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0B21 IN DATAPAGE8-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 8 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN37 OF DATAPAGE8-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 8 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN37 OF DATAPAGE8-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
  .            IF  RLT0B11 IN DATAPAGE8-I = "Y"                                 
  .                IF  RLT0B22 IN DATAPAGE8-I NOT = ZEROS                       
                       INITIALIZE SECHKDATE-MSG-ID                              
                       MOVE RLT0B22 IN DATAPAGE8-I TO TEMP-YYYYMMDD             
                       MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE            
                       CALL "SECHKDATE" USING SECHKDATE-DATE                    
                                              SECHKDATE-MSG-ID                  
                       IF  SECHKDATE-MSG-ID NOT = SPACES                        
  .                        MOVE 8 TO PAGE-COUNT                                 
  .                        MOVE INDIC-ON TO IN38 OF DATAPAGE8-O-INDIC           
  .                        MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O           
  .                        ADD 1 TO ERROR-COUNT                                 
                       END-IF                                                   
                   ELSE                                                         
  .                    MOVE 8 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN38 OF DATAPAGE8-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
      *        IF  RLT0B50 IN DATAPAGE8-I = "M"                OR ,             
      *            (RLT0B50 IN DATAPAGE8-I = "S"        AND ,                   
      *             RLT0B30 IN DATAPAGE8-I NOT = SPACES)                        
               IF  RLT0B30 IN DATAPAGE8-I NOT = SPACES                          
                   INITIALIZE CALL-SEREDSA0B-ARGUMENT                           
                   MOVE LINK-BRANCH-CODE       TO SA0010                        
                   MOVE RLT0B30 IN DATAPAGE8-I TO SA0020                        
                   CALL "SEREDSA0B" USING SEREDSA0B-STATUS                      
                                          SESA0P-RECORD                         
                   IF  NOT SESA0P-NORMAL                                        
  .                    MOVE 8 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN39 IN DATAPAGE8-O-INDIC               
  .                    MOVE "EDT0300" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
      *        IF  RLT0130 IN PPRLT0P-REC NOT = "Y"   AND ,                     
      *            RLT0B10 IN PPRLT0P-REC = "Y"       AND ,                     
      *            RLT0B11 IN PPRLT0P-REC NOT = "Y"                             
      *            INITIALIZE CALL-SEREDCS0B-ARGUMENT                           
      *            MOVE LINK-BRANCH-CODE       TO CS0010                        
      *            MOVE RLT0B40 IN DATAPAGE8-I TO CS0020                        
      *            CALL "SEREDCS0B" USING SEREDCS0B-STATUS                      
      *                                   SECS0P-RECORD                         
      *            IF  NOT SECS0P-NORMAL                                        
  .   *                MOVE 8 TO PAGE-COUNT                                     
  .   *                MOVE INDIC-ON TO IN41 OF DATAPAGE8-O-INDIC               
      *                MOVE "EDT0280" TO PANGCL040-MESSAGE-CODE-O               
      *                ADD 1 TO ERROR-COUNT                                     
      *            END-IF                                                       
      *        END-IF                                                           
  .            IF  RLT0B24 IN DATAPAGE8-I NOT = ZEROS                           
                   INITIALIZE SECHKDATE-MSG-ID                                  
                   MOVE RLT0B24 IN DATAPAGE8-I TO TEMP-YYYYMMDD                 
                   MOVE TEMP-YYYYMMDD          TO SECHKDATE-DATE                
                   CALL "SECHKDATE" USING SECHKDATE-DATE                        
                                          SECHKDATE-MSG-ID                      
                   IF  SECHKDATE-MSG-ID NOT = SPACES   OR ,                     
  .                    RLT0B24 IN DATAPAGE8-I < RLT0B20 IN DATAPAGE8-I          
  .                    MOVE 8 TO PAGE-COUNT                                     
  .                    MOVE INDIC-ON TO IN40 OF DATAPAGE8-O-INDIC               
  .                    MOVE "EDT0430" TO PANGCL040-MESSAGE-CODE-O               
  .                    ADD 1 TO ERROR-COUNT                                     
                   END-IF                                                       
               END-IF                                                           
               OPEN INPUT PPBRN1P                                               
               INITIALIZE PPBRN1PR                                              
               MOVE LINK-BRANCH-CODE TO BRNID IN PPBRN1P-REC                    
               PERFORM 9500-READ-PPBRN1P-RANDOM-RTN                             
               MOVE BRN1100 TO WK-BRN1100                                       
               CLOSE PPBRN1P                                                    
               IF  FORCE IN DATAPAGE1-I NOT = WK-BRN1100  OR ,                  
                   FORCE IN DATAPAGE1-I = SPACES                                
                   INITIALIZE PPRLT0PR IN WK-NEW-RECORD                         
                   MOVE CORR DATAPAGE2-I TO PPRLT0PR IN WK-NEW-RECORD           
                   MOVE CORR DATAPAGE7-I TO PPRLT0PR IN WK-NEW-RECORD           
                   MOVE CORR DATAPAGE8-I TO PPRLT0PR IN WK-NEW-RECORD           
      *            IF  OLD-RLT0271-DATA NOT = NEW-RLT0271-DATA  OR ,            
      *                OLD-RLT0C81-DATA NOT = NEW-RLT0C81-DATA  OR ,            
      *                OLD-RLT0C91-DATA NOT = NEW-RLT0C91-DATA                  
      *                PERFORM 5700-CHECK-T-EXTRA-DATA                          
      *            END-IF                                                       
      *            IF  OLD-RLT0A71-DATA NOT = NEW-RLT0A71-DATA  OR ,            
      *                OLD-RLT0D81-DATA NOT = NEW-RLT0D81-DATA  OR ,            
      *                OLD-RLT0A81-DATA NOT = NEW-RLT0A81-DATA                  
      *                PERFORM 5700-CHECK-W-EXTRA-DATA                          
      *            END-IF                                                       
      *            IF  OLD-RLT0B71-DATA NOT = NEW-RLT0B71-DATA  OR ,            
      *                OLD-RLT0E81-DATA NOT = NEW-RLT0E81-DATA  OR ,            
      *                OLD-RLT0B81-DATA NOT = NEW-RLT0B81-DATA                  
      *                PERFORM 5700-CHECK-E-EXTRA-DATA                          
      *            END-IF                                                       
++++++         END-IF                                                           
++++++     END-IF.                                                              
           IF  ERROR-COUNT > 0       AND ,                                      
               LIST-MODE                                                        
               IF  CSRRRN OF LISTCTL-I >= 1                                     
                   MOVE CSRRRN OF LISTCTL-I TO RCDNBRCSR OF LISTCTL-O           
               ELSE                                                             
                   MOVE 1 TO RCDNBRCSR OF LISTCTL-O                             
               END-IF                                                           
           END-IF.                                                              
      *    IF  ERROR-COUNT > 1                                                  
      *        MOVE "EDT0010" TO PANGCL040-MESSAGE-CODE-O                       
      *    END-IF.                                                              
      *                                                                         
      *                                                                   
      *    頛詨瞍                                                   
      *    頛詨                                             
      *                                                     
       5700-CHECK-T-EXTRA-DATA.                                                 
           COMPUTE A1 = RLT0271 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C81 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C91 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0272 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C82 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C92 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0273 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C83 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C93 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0274 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C84 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C94 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0275 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C85 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C95 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0276 IN WK-NEW-RECORD / 100.                         
           COMPUTE B1 = RLT0C86 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0C96 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 2 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *    COMPUTE A1 = RLT0277 IN WK-NEW-RECORD / 100.                         
      *    COMPUTE B1 = RLT0C87 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0C97 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE D1 = A1 + B1                                             
      *        IF  D1 > 10                                                      
  .   *            MOVE 2 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE E1 = A1 + C1                                             
      *        IF  E1 > 12                                                      
  .   *            MOVE 2 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *    COMPUTE A1 = RLT0278 IN WK-NEW-RECORD / 100.                         
      *    COMPUTE B1 = RLT0C88 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0C98 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE D1 = A1 + B1                                             
      *        IF  D1 > 10                                                      
  .   *            MOVE 2 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE E1 = A1 + C1                                             
      *        IF  E1 > 12                                                      
  .   *            MOVE 2 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                         
      *                                                                   
       5700-CHECK-W-EXTRA-DATA.                                                 
           COMPUTE A1 = (0.001425 - RLT0A71 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D81 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A81 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = (0.001425 - RLT0A72 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D82 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A82 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = (0.001425 - RLT0A73 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D83 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A83 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = (0.001425 - RLT0A74 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D84 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A84 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = (0.001425 - RLT0A75 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D85 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A85 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = (0.001425 - RLT0A76 IN WK-NEW-RECORD) * 10000.          
           COMPUTE B1 = RLT0D86 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0A86 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE F1 = A1 + B1                                             
               IF  F1 > 10                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE G1 = A1 + C1                                             
               IF  G1 > 12                                                      
  .                MOVE 7 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *    COMPUTE A1 = (0.001425 - RLT0A77 IN WK-NEW-RECORD) * 10000.          
      *    COMPUTE B1 = RLT0D87 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0A87 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE F1 = A1 + B1                                             
      *        IF  F1 > 10                                                      
  .   *            MOVE 7 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE G1 = A1 + C1                                             
      *        IF  G1 > 12                                                      
  .   *            MOVE 7 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *    COMPUTE A1 = (0.001425 - RLT0A78 IN WK-NEW-RECORD) * 10000.          
      *    COMPUTE B1 = RLT0D88 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0A88 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE F1 = A1 + B1                                             
      *        IF  F1 > 10                                                      
  .   *            MOVE 7 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE G1 = A1 + C1                                             
      *        IF  G1 > 12                                                      
  .   *            MOVE 7 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
                                                                                
      *                                                                   
       5700-CHECK-E-EXTRA-DATA.                                                 
           COMPUTE A1 = RLT0B71 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E81 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B81 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0B72 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E82 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B82 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0B73 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E83 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B83 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0B74 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E84 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B84 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0B75 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E85 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B85 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
           COMPUTE A1 = RLT0B76 IN WK-NEW-RECORD * 1000.                        
           COMPUTE B1 = RLT0E86 IN WK-NEW-RECORD / 10.                          
           COMPUTE C1 = RLT0B86 IN WK-NEW-RECORD / 10.                          
      *                                                                 
           IF  B1 > 0                                                           
               COMPUTE D1 = A1 + B1                                             
               IF  D1 > 10                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *                                                                   
           IF  C1 > 0                                                           
               COMPUTE E1 = A1 + C1                                             
               IF  E1 > 12                                                      
  .                MOVE 8 TO PAGE-COUNT                                         
  .                MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .                ADD 1 TO ERROR-COUNT                                         
               END-IF                                                           
           END-IF.                                                              
      *    COMPUTE A1 = RLT0B77 IN WK-NEW-RECORD * 1000.                        
      *    COMPUTE B1 = RLT0E87 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0B87 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE D1 = A1 + B1                                             
      *        IF  D1 > 10                                                      
  .   *            MOVE 8 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE E1 = A1 + C1                                             
      *        IF  E1 > 12                                                      
  .   *            MOVE 8 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *    COMPUTE A1 = RLT0B78 IN WK-NEW-RECORD * 1000.                        
      *    COMPUTE B1 = RLT0E88 IN WK-NEW-RECORD / 10.                          
      *    COMPUTE C1 = RLT0B88 IN WK-NEW-RECORD / 10.                          
      *                                                                 
      *    IF  B1 > 0                                                           
      *        COMPUTE D1 = A1 + B1                                             
      *        IF  D1 > 10                                                      
  .   *            MOVE 8 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0700" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                   
      *    IF  C1 > 0                                                           
      *        COMPUTE E1 = A1 + C1                                             
      *        IF  E1 > 12                                                      
  .   *            MOVE 8 TO PAGE-COUNT                                         
  .   *            MOVE "EDT0710" TO PANGCL040-MESSAGE-CODE-O                   
  .   *            ADD 1 TO ERROR-COUNT                                         
      *        END-IF                                                           
      *    END-IF.                                                              
      *                                                                         
       6000-SET-FIELD-DEFAULT-VALUE.                                            
           MOVE INDIC-OFF TO DATAPAGE1-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE2-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE3-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE4-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE5-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE6-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE7-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE8-O-INDIC.                                 
           MOVE INDIC-OFF TO DATAPAGE9-O-INDIC.                                 
                                                                                
      *   SWITHS FLAGS                                    
           IF  (RLT0110 IN DATAPAGE1-I < "1"   OR ,                             
                RLT0110 IN DATAPAGE1-I > "4")         AND ,                     
               RLT0110 IN DATAPAGE1-I NOT = SPACES                              
               MOVE "9" TO RLT0110 IN DATAPAGE1-I                               
           END-IF.                                                              
           IF  RLT0130 IN DATAPAGE1-I NOT = "Y"                                 
               MOVE "N" TO RLT0130 IN DATAPAGE1-I                               
           END-IF.                                                              
                                                                                
           IF  RLT0240 IN DATAPAGE1-I NOT = SPACES                              
               IF  RLT0020 IN KEYAREA-O = RLT0240 IN DATAPAGE1-I                
                   MOVE "M" TO RLT0250 IN DATAPAGE1-I                           
                               RLT0250 IN DATAPAGE2-I                           
                               RLT0250 IN DATAPAGE3-I                           
                               RLT0250 IN DATAPAGE4-I                           
                               RLT0250 IN DATAPAGE5-I                           
                               RLT0250 IN DATAPAGE6-I                           
                               RLT0250 IN DATAPAGE9-I                           
               ELSE                                                             
                   MOVE "S" TO RLT0250 IN DATAPAGE1-I                           
                               RLT0250 IN DATAPAGE2-I                           
                               RLT0250 IN DATAPAGE3-I                           
                               RLT0250 IN DATAPAGE4-I                           
                               RLT0250 IN DATAPAGE5-I                           
                               RLT0250 IN DATAPAGE6-I                           
                               RLT0250 IN DATAPAGE9-I                           
               END-IF                                                           
           END-IF.                                                              
           IF  RLT0210 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE "N" TO RLT0210 IN DATAPAGE2-I                               
           END-IF.                                                              
           IF  RLT0360 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE " " TO RLT0360 IN DATAPAGE2-I                               
           END-IF.                                                              
           IF  RLT0211 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE "N" TO RLT0211 IN DATAPAGE2-I                               
           END-IF.                                                              
      *    IF  RLT0282 IN DATAPAGE2-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0282 IN DATAPAGE2-I                               
      *    END-IF.                                                              
           IF  RLT0255 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE "N" TO RLT0255 IN DATAPAGE2-I                               
           END-IF.                                                              
           IF  RLT0280 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE "N" TO RLT0280 IN DATAPAGE2-I                               
           END-IF.                                                              
      *    IF  RLT0281 IN DATAPAGE2-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0281 IN DATAPAGE2-I                               
      *    END-IF.                                                              
           IF  RLT0290 IN DATAPAGE2-I NOT = "Y"                                 
               MOVE "N" TO RLT0290 IN DATAPAGE2-I                               
           END-IF.                                                              
      *    IF  RLT0291 IN DATAPAGE2-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0291 IN DATAPAGE2-I                               
      *    END-IF.                                                              
      *    IF  RLT0310 IN DATAPAGE2-I NOT = "E"                                 
      *        MOVE " " TO RLT0310 IN DATAPAGE2-I                               
      *    END-IF.                                                              
           IF  RLT0340 IN DATAPAGE2-I NOT = "N"                                 
               MOVE "Y" TO RLT0340 IN DATAPAGE2-I                               
           END-IF.                                                              
      *    IF  RLT0362 IN DATAPAGE2-I NOT = "R"                                 
      *        MOVE " " TO RLT0362 IN DATAPAGE2-I                               
      *    END-IF.                                                              
      *    IF  RLT0300 IN DATAPAGE2-I NOT = "N"                                 
      *        MOVE "Y" TO RLT0300 IN DATAPAGE2-I                               
      *Z   END-IF.                                                              
           IF  RLT0292 IN DATAPAGE2-I NOT = "N"                                 
               MOVE "Y" TO RLT0292 IN DATAPAGE2-I                               
           END-IF.                                                              
                                                                                
           IF  RLT0410 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0410 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0411 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0411 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0455 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0455 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0463 IN DATAPAGE3-I NOT = "B"    AND ,                        
               RLT0463 IN DATAPAGE3-I NOT = "M"                                 
               MOVE " " TO RLT0463 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0460 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0460 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0461 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0461 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0466 IN DATAPAGE3-I NOT = "G"                                 
               MOVE "P" TO RLT0466 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0471 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0471 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0472 IN DATAPAGE3-I NOT = "Y"                                 
               MOVE "N" TO RLT0472 IN DATAPAGE3-I                               
           END-IF.                                                              
           IF  RLT0480 IN DATAPAGE3-I NOT = "Y"   AND ,                         
               RLT0480 IN DATAPAGE3-I NOT = "N"                                 
               MOVE " " TO RLT0480 IN DATAPAGE3-I                               
           END-IF.                                                              
                                                                                
           IF  RLT0510 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0510 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0511 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0511 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0555 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0555 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0563 IN DATAPAGE4-I NOT = "S"                                 
               MOVE " " TO RLT0563 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0564 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0564 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0560 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0560 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0561 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0561 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0566 IN DATAPAGE4-I NOT = "G"                                 
               MOVE "P" TO RLT0566 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0571 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0571 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0572 IN DATAPAGE4-I NOT = "Y"                                 
               MOVE "N" TO RLT0572 IN DATAPAGE4-I                               
           END-IF.                                                              
           IF  RLT0580 IN DATAPAGE4-I NOT = "Y"   AND ,                         
               RLT0580 IN DATAPAGE4-I NOT = "N"                                 
               MOVE " " TO RLT0580 IN DATAPAGE4-I                               
           END-IF.                                                              
                                                                                
           IF  RLT0610 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0610 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0611 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0611 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0655 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0655 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0663 IN DATAPAGE5-I NOT = "D"                                 
               MOVE " " TO RLT0663 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0664 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0664 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0660 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0660 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0661 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0661 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0666 IN DATAPAGE5-I NOT = "G"                                 
               MOVE "P" TO RLT0666 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0671 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0671 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0672 IN DATAPAGE5-I NOT = "Y"                                 
               MOVE "N" TO RLT0672 IN DATAPAGE5-I                               
           END-IF.                                                              
           IF  RLT0680 IN DATAPAGE5-I NOT = "Y"   AND ,                         
               RLT0680 IN DATAPAGE5-I NOT = "N"                                 
               MOVE " " TO RLT0680 IN DATAPAGE5-I                               
           END-IF.                                                              
                                                                                
           IF  RLT0800 IN DATAPAGE6-I NOT = "1" AND NOT = "3"                   
               MOVE "2" TO RLT0800 IN DATAPAGE6-I                               
           END-IF.                                                              
      *    IF  RLT0820 IN DATAPAGE2-I NOT = "N"                                 
      *        MOVE "Y" TO RLT0820 IN DATAPAGE2-I                               
      *    END-IF.                                                              
           IF  RLT0840 IN DATAPAGE6-I NOT = "Y"                                 
               MOVE "N" TO RLT0840 IN DATAPAGE6-I                               
           END-IF.                                                              
      *    IF  RLT0860 IN DATAPAGE6-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0860 IN DATAPAGE6-I                               
      *    END-IF.                                                              
      *    IF  RLT0870 IN DATAPAGE6-I NOT = "S" AND NOT = "C"                   
      *        MOVE " " TO RLT0870 IN DATAPAGE6-I                               
      *    END-IF.                                                              
      *    IF  RLT0880 IN DATAPAGE6-I NOT = "P" AND NOT = "M"                   
      *        AND NOT = "R"                                                    
      *        MOVE " " TO RLT0880 IN DATAPAGE6-I                               
      *    END-IF.                                                              
                                                                                
      *    IF  RLT0A40 IN DATAPAGE7-I NOT = SPACES                              
      *        IF  RLT0020 IN KEYAREA-O = RLT0A40 IN DATAPAGE7-I                
      *            MOVE "M" TO RLT0A50 IN DATAPAGE7-I                           
      *        ELSE                                                             
      *            MOVE "S" TO RLT0A50 IN DATAPAGE7-I                           
      *        END-IF                                                           
      *    END-IF.                                                              
           IF  RLT0A10 IN DATAPAGE7-I NOT = "Y"                                 
               MOVE "N" TO RLT0A10 IN DATAPAGE7-I                               
           END-IF.                                                              
           IF  RLT0361 IN DATAPAGE7-I NOT = "Y"                                 
               MOVE " " TO RLT0361 IN DATAPAGE7-I                               
           END-IF.                                                              
           IF  RLT0A11 IN DATAPAGE7-I NOT = "Y"                                 
               MOVE "N" TO RLT0A11 IN DATAPAGE7-I                               
           END-IF.                                                              
      *    IF  RLT0A55 IN DATAPAGE7-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0A55 IN DATAPAGE7-I                               
      *    END-IF.                                                              
           IF  RLT0A89 IN DATAPAGE7-I NOT = "Y"                                 
               MOVE "N" TO RLT0A89 IN DATAPAGE7-I                               
           END-IF.                                                              
           IF  RLT0A90 IN DATAPAGE7-I NOT = "Y"                                 
               MOVE "N" TO RLT0A90 IN DATAPAGE7-I                               
           END-IF.                                                              
      *    IF  RLT0A91 IN DATAPAGE7-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0A91 IN DATAPAGE7-I                               
      *    END-IF.                                                              
      *    IF  RLT0A92 IN DATAPAGE7-I NOT = "N"                                 
      *        MOVE "Y" TO RLT0A92 IN DATAPAGE7-I                               
      *    END-IF.                                                              
                                                                                
      *    IF  RLT0B40 IN DATAPAGE8-I NOT = SPACES                              
      *        IF  RLT0020 IN KEYAREA-O = RLT0B40 IN DATAPAGE8-I                
      *            MOVE "M" TO RLT0B50 IN DATAPAGE8-I                           
      *        ELSE                                                             
      *            MOVE "S" TO RLT0B50 IN DATAPAGE8-I                           
      *        END-IF                                                           
      *    END-IF.                                                              
           IF  RLT0B10 IN DATAPAGE8-I NOT = "Y"                                 
               MOVE "N" TO RLT0B10 IN DATAPAGE8-I                               
           END-IF.                                                              
           IF  RLT0B11 IN DATAPAGE8-I NOT = "Y"                                 
               MOVE "N" TO RLT0B11 IN DATAPAGE8-I                               
           END-IF.                                                              
      *    IF  RLT0B55 IN DATAPAGE8-I NOT = "Y"                                 
      *        MOVE "N" TO RLT0B55 IN DATAPAGE8-I                               
      *    END-IF.                                                              
           IF  RLT0B90 IN DATAPAGE8-I NOT = "Y"                                 
               MOVE "N" TO RLT0B90 IN DATAPAGE8-I                               
           END-IF.                                                              
           IF  RLT0B91 IN DATAPAGE8-I NOT = "Y"                                 
               MOVE "N" TO RLT0B91 IN DATAPAGE8-I                               
           END-IF.                                                              
      *    IF  RLT0B92 IN DATAPAGE8-I NOT = "N"                                 
      *        MOVE "Y" TO RLT0B92 IN DATAPAGE8-I                               
      *    END-IF.                                                              
                                                                                
           IF  RLT0710 IN DATAPAGE9-I NOT = "Y"                                 
               MOVE "N" TO RLT0710 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0711 IN DATAPAGE9-I NOT = "Y"                                 
               MOVE "N" TO RLT0711 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0750 IN DATAPAGE9-I NOT = "N"                                 
               MOVE "Y" TO RLT0750 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0755 IN DATAPAGE9-I NOT = "G"                                 
               MOVE "P" TO RLT0710 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0761 IN DATAPAGE9-I NOT = "N"                                 
               MOVE "Y" TO RLT0761 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0762 IN DATAPAGE9-I NOT = "N"                                 
               MOVE "Y" TO RLT0762 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0764 IN DATAPAGE9-I NOT = "M"                                 
               MOVE "G" TO RLT0764 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0772 IN DATAPAGE9-I NOT = "N"                                 
               MOVE "Y" TO RLT0772 IN DATAPAGE9-I                               
           END-IF.                                                              
           IF  RLT0773 IN DATAPAGE9-I NOT = "N"                                 
               MOVE "Y" TO RLT0773 IN DATAPAGE9-I                               
           END-IF.                                                              
      *  Ｙ鞎                                       
      *                  RLT0A30 - B30 - 430 - 530 - 630 鞊             
           IF  RLT0230 IN DATAPAGE2-I = SPACES        AND ,                     
               RLT0A30 IN DATAPAGE7-I NOT = SPACES                              
               MOVE RLT0A30 IN DATAPAGE7-I TO                                   
                    RLT0230 IN DATAPAGE2-I                                      
           END-IF.                                                              
           IF  RLT0230 IN DATAPAGE2-I = SPACES        AND ,                     
               RLT0B30 IN DATAPAGE8-I NOT = SPACES                              
               MOVE RLT0B30 IN DATAPAGE8-I TO                                   
                    RLT0230 IN DATAPAGE2-I                                      
           END-IF.                                                              
      *    IF  RLT0230 IN DATAPAGE2-I = SPACES        AND ,                     
      *        RLT0430 IN DATAPAGE3-I NOT = SPACES                              
      *        MOVE RLT0430 IN DATAPAGE3-I TO                                   
      *             RLT0230 IN DATAPAGE2-I                                      
      *    END-IF.                                                              
      *    IF  RLT0230 IN DATAPAGE2-I = SPACES        AND ,                     
      *        RLT0530 IN DATAPAGE4-I NOT = SPACES                              
      *        MOVE RLT0530 IN DATAPAGE4-I TO                                   
      *             RLT0230 IN DATAPAGE2-I                                      
      *    END-IF.                                                              
      *    IF  RLT0230 IN DATAPAGE2-I = SPACES        AND ,                     
      *        RLT0630 IN DATAPAGE5-I NOT = SPACES                              
      *        MOVE RLT0630 IN DATAPAGE5-I TO                                   
      *             RLT0230 IN DATAPAGE2-I                                      
      *    END-IF.                                                              
                                                                                
      *                                                                         
       7000-INITIALIZE-FIELD.                                                   
           INITIALIZE PPRLT0PR IN PPRLT0P-REC.                                  
           MOVE LINK-BRANCH-CODE TO RLT0010 IN PPRLT0P-REC.                     
      *  頦                                                             
           MOVE "9" TO RLT0110 IN PPRLT0P-REC.                                  
           MOVE "D" TO RLT0260 IN PPRLT0P-REC.                                  
           MOVE "A" TO RLT0320 IN PPRLT0P-REC                                   
           MOVE "1" TO RLT0330 IN PPRLT0P-REC.                                  
           MOVE "M" TO RLT0732 IN PPRLT0P-REC.                                  
           MOVE "P" TO RLT0755 IN PPRLT0P-REC.                                  
           MOVE "G" TO RLT0764 IN PPRLT0P-REC.                                  
           MOVE "P" TO RLT0466 IN PPRLT0P-REC                                   
                       RLT0566 IN PPRLT0P-REC                                   
                       RLT0666 IN PPRLT0P-REC.                                  
           MOVE "2" TO RLT0800 IN PPRLT0P-REC.                                  
           MOVE "R" TO RLT0A60 IN PPRLT0P-REC.                                  
           MOVE "R" TO RLT0B60 IN PPRLT0P-REC.                                  
      *    PPSYS0P                                                          
      *    IF  SYS0320 IN PPSYS0P-REC = "N"                                     
      *        MOVE "N" TO RLT0292 IN PPRLT0P-REC                               
      *    ELSE                                                                 
      *        MOVE "Y" TO RLT0292 IN PPRLT0P-REC                               
      *    END-IF.                                                              
      *    IF  SYS0300 = "N"                                                    
      *        MOVE "N" TO RLT0480 IN PPRLT0P-REC                               
      *    ELSE                                                                 
      *        MOVE "Y" TO RLT0480 IN PPRLT0P-REC                               
      *    END-IF.                                                              
      *    IF  SYS0310 = "N"                                                    
      *        MOVE "N" TO RLT0580 IN PPRLT0P-REC                               
      *                    RLT0680 IN PPRLT0P-REC                               
      *    ELSE                                                                 
      *        MOVE "Y" TO RLT0580 IN PPRLT0P-REC                               
      *                    RLT0680 IN PPRLT0P-REC                               
      *    END-IF.                                                              
      *  剛                                                         
           MOVE "Y" TO RLT0292 IN PPRLT0P-REC                                   
                       RLT0300 IN PPRLT0P-REC                                   
                       RLT0301 IN PPRLT0P-REC                                   
                       RLT0302 IN PPRLT0P-REC                                   
                       RLT0303 IN PPRLT0P-REC                                   
                       RLT0304 IN PPRLT0P-REC                                   
                       RLT0305 IN PPRLT0P-REC                                   
                       RLT0306 IN PPRLT0P-REC                                   
                       RLT0340 IN PPRLT0P-REC                                   
                       RLT0750 IN PPRLT0P-REC                                   
                       RLT0761 IN PPRLT0P-REC                                   
                       RLT0762 IN PPRLT0P-REC                                   
                       RLT0763 IN PPRLT0P-REC                                   
                       RLT0772 IN PPRLT0P-REC                                   
                       RLT0773 IN PPRLT0P-REC                                   
                       RLT0774 IN PPRLT0P-REC                                   
                       RLT0820 IN PPRLT0P-REC                                   
                       RLT0A92 IN PPRLT0P-REC                                   
                       RLT0AA0 IN PPRLT0P-REC                                   
                       RLT0AA1 IN PPRLT0P-REC                                   
                       RLT0AA2 IN PPRLT0P-REC                                   
                       RLT0AA3 IN PPRLT0P-REC                                   
                       RLT0AA4 IN PPRLT0P-REC                                   
                       RLT0AA5 IN PPRLT0P-REC                                   
                       RLT0AA6 IN PPRLT0P-REC                                   
                       RLT0B92 IN PPRLT0P-REC                                   
                       RLT0BA0 IN PPRLT0P-REC                                   
                       RLT0BA1 IN PPRLT0P-REC                                   
                       RLT0BA2 IN PPRLT0P-REC                                   
                       RLT0BA3 IN PPRLT0P-REC                                   
                       RLT0BA4 IN PPRLT0P-REC                                   
                       RLT0BA5 IN PPRLT0P-REC                                   
                       RLT0BA6 IN PPRLT0P-REC.                                  
           MOVE "N" TO RLT0255 IN PPRLT0P-REC                                   
                       RLT0130 IN PPRLT0P-REC                                   
                       RLT0210 IN PPRLT0P-REC                                   
                       RLT0211 IN PPRLT0P-REC                                   
                       RLT0280 IN PPRLT0P-REC                                   
                       RLT0281 IN PPRLT0P-REC                                   
                       RLT0282 IN PPRLT0P-REC                                   
                       RLT0290 IN PPRLT0P-REC                                   
                       RLT0291 IN PPRLT0P-REC                                   
                       RLT0410 IN PPRLT0P-REC                                   
                       RLT0411 IN PPRLT0P-REC                                   
                       RLT0455 IN PPRLT0P-REC                                   
                       RLT0460 IN PPRLT0P-REC                                   
                       RLT0461 IN PPRLT0P-REC                                   
                       RLT0462 IN PPRLT0P-REC                                   
                       RLT0471 IN PPRLT0P-REC                                   
                       RLT0472 IN PPRLT0P-REC                                   
                       RLT0473 IN PPRLT0P-REC                                   
                       RLT0510 IN PPRLT0P-REC                                   
                       RLT0511 IN PPRLT0P-REC                                   
                       RLT0555 IN PPRLT0P-REC                                   
                       RLT0560 IN PPRLT0P-REC                                   
                       RLT0561 IN PPRLT0P-REC                                   
                       RLT0562 IN PPRLT0P-REC                                   
                       RLT0564 IN PPRLT0P-REC                                   
                       RLT0571 IN PPRLT0P-REC                                   
                       RLT0572 IN PPRLT0P-REC                                   
                       RLT0573 IN PPRLT0P-REC                                   
                       RLT0610 IN PPRLT0P-REC                                   
                       RLT0611 IN PPRLT0P-REC                                   
                       RLT0655 IN PPRLT0P-REC                                   
                       RLT0660 IN PPRLT0P-REC                                   
                       RLT0661 IN PPRLT0P-REC                                   
                       RLT0662 IN PPRLT0P-REC                                   
                       RLT0664 IN PPRLT0P-REC                                   
                       RLT0671 IN PPRLT0P-REC                                   
                       RLT0672 IN PPRLT0P-REC                                   
                       RLT0673 IN PPRLT0P-REC                                   
                       RLT0710 IN PPRLT0P-REC                                   
                       RLT0711 IN PPRLT0P-REC                                   
                       RLT0840 IN PPRLT0P-REC                                   
                       RLT0860 IN PPRLT0P-REC                                   
                       RLT0A10 IN PPRLT0P-REC                                   
                       RLT0A11 IN PPRLT0P-REC                                   
                       RLT0A55 IN PPRLT0P-REC                                   
                       RLT0A89 IN PPRLT0P-REC                                   
                       RLT0A90 IN PPRLT0P-REC                                   
                       RLT0A91 IN PPRLT0P-REC                                   
                       RLT0B10 IN PPRLT0P-REC                                   
                       RLT0B11 IN PPRLT0P-REC                                   
                       RLT0B55 IN PPRLT0P-REC                                   
                       RLT0B90 IN PPRLT0P-REC                                   
                       RLT0B91 IN PPRLT0P-REC.                                  
      *           
      *   ENTRY MODE 儮 INITIALIZE                
      *           
           INITIALIZE DATAPAGE1-I DATAPAGE1-O DATAPAGE2-I DATAPAGE2-O           
                      DATAPAGE3-I DATAPAGE3-O DATAPAGE4-I DATAPAGE4-O           
                      DATAPAGE5-I DATAPAGE5-O DATAPAGE6-I DATAPAGE6-O           
                      DATAPAGE7-I DATAPAGE7-O DATAPAGE8-I DATAPAGE8-O           
                      DATAPAGE9-I DATAPAGE9-O.                                  
      *                                                                         
      *           
      *   ENTRY MODE 儮 MOVE                      
      *           
       8000-FIELD-MAP-MOVE.                                                     
           IF  FILE-TO-SCREEN-IO                                                
               MOVE CORR PPSYS0PR IN PPSYS0P-REC TO DATAPAGE3-O                 
               MOVE CORR PPSYS0PR IN PPSYS0P-REC TO DATAPAGE4-O                 
               MOVE CORR PPSYS0PR IN PPSYS0P-REC TO DATAPAGE5-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE1-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE1-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE2-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE2-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE3-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE3-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE4-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE4-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE5-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE5-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE6-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE6-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE7-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE7-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE8-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE8-O                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE9-I                 
               MOVE CORR PPRLT0PR IN PPRLT0P-REC TO DATAPAGE9-O                 
                                                                                
      *  嚗                                                       
      *        COMPUTE RLT0A71 IN DATAPAGE7-I = RLT0A71 IN DATAPAGE7-I          
      *                                       * 1000                            
      *        COMPUTE RLT0A72 IN DATAPAGE7-I = RLT0A72 IN DATAPAGE7-I          
      *                                       * 1000                            
      *        COMPUTE RLT0A73 IN DATAPAGE7-I = RLT0A73 IN DATAPAGE7-I          
      *                                       * 1000                            
      *        COMPUTE RLT0A74 IN DATAPAGE7-I = RLT0A74 IN DATAPAGE7-I          
      *                                       * 1000                            
      *        COMPUTE RLT0A75 IN DATAPAGE7-I = RLT0A75 IN DATAPAGE7-I          
      *                                       * 1000                            
      *        COMPUTE RLT0A76 IN DATAPAGE7-I = RLT0A76 IN DATAPAGE7-I          
      *                                       * 1000                            
                                                                                
      *        COMPUTE RLT0A71 IN DATAPAGE7-O = RLT0A71 IN DATAPAGE7-O          
      *                                       * 1000                            
      *        COMPUTE RLT0A72 IN DATAPAGE7-O = RLT0A72 IN DATAPAGE7-O          
      *                                       * 1000                            
      *        COMPUTE RLT0A73 IN DATAPAGE7-O = RLT0A73 IN DATAPAGE7-O          
      *                                       * 1000                            
      *        COMPUTE RLT0A74 IN DATAPAGE7-O = RLT0A74 IN DATAPAGE7-O          
      *                                       * 1000                            
      *        COMPUTE RLT0A75 IN DATAPAGE7-O = RLT0A75 IN DATAPAGE7-O          
      *                                       * 1000                            
      *        COMPUTE RLT0A76 IN DATAPAGE7-O = RLT0A76 IN DATAPAGE7-O          
      *                                       * 1000                            
           END-IF.                                                              
           IF  SCREEN-INPUT-TO-FILE                                             
      *   0.001425                                                    
      *        COMPUTE RLT0A71 IN DATAPAGE7-I = RLT0A71 IN DATAPAGE7-I          
      *                                       / 1000                            
      *        COMPUTE RLT0A72 IN DATAPAGE7-I = RLT0A72 IN DATAPAGE7-I          
      *                                       / 1000                            
      *        COMPUTE RLT0A73 IN DATAPAGE7-I = RLT0A73 IN DATAPAGE7-I          
      *                                       / 1000                            
      *        COMPUTE RLT0A74 IN DATAPAGE7-I = RLT0A74 IN DATAPAGE7-I          
      *                                       / 1000                            
      *        COMPUTE RLT0A75 IN DATAPAGE7-I = RLT0A75 IN DATAPAGE7-I          
      *                                       / 1000                            
      *        COMPUTE RLT0A76 IN DATAPAGE7-I = RLT0A76 IN DATAPAGE7-I          
      *                                       / 1000                            
                                                                                
               MOVE CORR DATAPAGE1-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE2-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE3-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE4-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE5-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE6-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE7-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE8-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE CORR DATAPAGE9-I TO PPRLT0PR IN PPRLT0P-REC                 
               MOVE RLT0240 IN PPRLT0P-REC TO RLT0A40 IN PPRLT0P-REC            
                                              RLT0B40 IN PPRLT0P-REC            
               MOVE RLT0250 IN PPRLT0P-REC TO RLT0A50 IN PPRLT0P-REC            
                                              RLT0B50 IN PPRLT0P-REC            
           END-IF.                                                              
           IF  SCREEN-INPUT-TO-OUTPUT                                           
               MOVE CORR DATAPAGE1-I TO DATAPAGE1-O                             
               MOVE CORR DATAPAGE2-I TO DATAPAGE2-O                             
               MOVE CORR DATAPAGE3-I TO DATAPAGE3-O                             
               MOVE CORR DATAPAGE4-I TO DATAPAGE4-O                             
               MOVE CORR DATAPAGE5-I TO DATAPAGE5-O                             
               MOVE CORR DATAPAGE6-I TO DATAPAGE6-O                             
               MOVE CORR DATAPAGE7-I TO DATAPAGE7-O                             
               MOVE CORR DATAPAGE8-I TO DATAPAGE8-O                             
               MOVE CORR DATAPAGE9-I TO DATAPAGE9-O                             
           END-IF.                                                              
           IF  SCREEN-INPUT-TO-OUTPUT-PAGE                                      
               EVALUATE PAGE-COUNT                                              
++++++           WHEN 1                                                         
                      MOVE CORR DATAPAGE1-I TO DATAPAGE1-O                      
++++++           WHEN 2                                                         
                      MOVE CORR DATAPAGE2-I TO DATAPAGE2-O                      
++++++           WHEN 3                                                         
                      MOVE CORR DATAPAGE3-I TO DATAPAGE3-O                      
++++++           WHEN 4                                                         
                      MOVE CORR DATAPAGE4-I TO DATAPAGE4-O                      
++++++           WHEN 5                                                         
                      MOVE CORR DATAPAGE5-I TO DATAPAGE5-O                      
++++++           WHEN 6                                                         
                      MOVE CORR DATAPAGE6-I TO DATAPAGE6-O                      
++++++           WHEN 7                                                         
                      MOVE CORR DATAPAGE7-I TO DATAPAGE7-O                      
++++++           WHEN 8                                                         
                      MOVE CORR DATAPAGE8-I TO DATAPAGE8-O                      
++++++           WHEN 9                                                         
                      MOVE CORR DATAPAGE9-I TO DATAPAGE9-O                      
              END-EVALUATE                                                      
           END-IF.                                                              
      *                                                                         
       8300-SET-LAST-MODIFY-TIME.                                               
           IF  OLD-PAGE2-DATA NOT = NEW-PAGE2-DATA       OR ,                   
               OLD-RLT0271-DATA NOT = NEW-RLT0271-DATA   OR ,                   
               OLD-RLT0C81-DATA NOT = NEW-RLT0C81-DATA   OR ,                   
               OLD-RLT0C91-DATA NOT = NEW-RLT0C91-DATA                          
               MOVE PANGSUB500-TW-DATE TO RLT0223 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE3-DATA NOT = NEW-PAGE3-DATA                              
               MOVE PANGSUB500-TW-DATE TO RLT0423 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE4-DATA NOT = NEW-PAGE4-DATA                              
               MOVE PANGSUB500-TW-DATE TO RLT0523 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE5-DATA NOT = NEW-PAGE5-DATA                              
               MOVE PANGSUB500-TW-DATE TO RLT0623 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE7-DATA NOT = NEW-PAGE7-DATA       OR ,                   
               OLD-RLT0A71-DATA NOT = NEW-RLT0A71-DATA   OR ,                   
               OLD-RLT0D81-DATA NOT = NEW-RLT0D81-DATA   OR ,                   
               OLD-RLT0A81-DATA NOT = NEW-RLT0A81-DATA                          
               MOVE PANGSUB500-TW-DATE TO RLT0A23 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE8-DATA NOT = NEW-PAGE8-DATA       OR ,                   
               OLD-RLT0B71-DATA NOT = NEW-RLT0B71-DATA   OR ,                   
               OLD-RLT0E81-DATA NOT = NEW-RLT0E81-DATA   OR ,                   
               OLD-RLT0B81-DATA NOT = NEW-RLT0B81-DATA                          
               MOVE PANGSUB500-TW-DATE TO RLT0B23 IN PPRLT0P-REC                
           END-IF.                                                              
           IF  OLD-PAGE9-DATA NOT = NEW-PAGE9-DATA                              
               MOVE PANGSUB500-TW-DATE TO RLT0723 IN PPRLT0P-REC                
           END-IF.                                                              
      *                                                                         
       8400-ADJUST-RLT0AB.                                                      
           IF  RLT0290 IN PPRLT0P-REC = "Y"                                     
               MOVE RLT0210 IN PPRLT0P-REC TO RLT0A10 IN PPRLT0P-REC            
               MOVE RLT0211 IN PPRLT0P-REC TO RLT0A11 IN PPRLT0P-REC            
               MOVE RLT0220 IN PPRLT0P-REC TO RLT0A20 IN PPRLT0P-REC            
               MOVE RLT0221 IN PPRLT0P-REC TO RLT0A21 IN PPRLT0P-REC            
               MOVE RLT0222 IN PPRLT0P-REC TO RLT0A22 IN PPRLT0P-REC            
               MOVE RLT0224 IN PPRLT0P-REC TO RLT0A24 IN PPRLT0P-REC            
               MOVE RLT0230 IN PPRLT0P-REC TO RLT0A30 IN PPRLT0P-REC            
               MOVE RLT0280 IN PPRLT0P-REC TO RLT0A90 IN PPRLT0P-REC            
               MOVE RLT0281 IN PPRLT0P-REC TO RLT0A91 IN PPRLT0P-REC            
                                                                                
               MOVE RLT0261 IN PPRLT0P-REC TO RLT0A61 IN PPRLT0P-REC            
               MOVE RLT0262 IN PPRLT0P-REC TO RLT0A62 IN PPRLT0P-REC            
               MOVE RLT0263 IN PPRLT0P-REC TO RLT0A63 IN PPRLT0P-REC            
               MOVE RLT0264 IN PPRLT0P-REC TO RLT0A64 IN PPRLT0P-REC            
               MOVE RLT0265 IN PPRLT0P-REC TO RLT0A65 IN PPRLT0P-REC            
               MOVE RLT0266 IN PPRLT0P-REC TO RLT0A66 IN PPRLT0P-REC            
               MOVE RLT0267 IN PPRLT0P-REC TO RLT0A67 IN PPRLT0P-REC            
               MOVE RLT0268 IN PPRLT0P-REC TO RLT0A68 IN PPRLT0P-REC            
               MOVE RLT0360 IN PPRLT0P-REC TO RLT0361 IN PPRLT0P-REC            
               IF  RLT0271 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A71 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0271 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0272 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A72 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0272 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0273 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A73 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0273 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0274 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A74 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0274 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0275 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A75 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0275 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0276 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A76 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0276 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0277 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A77 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0277 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0278 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0A78 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0278 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
           END-IF.                                                              
      *    IF  RLT0290 IN PPRLT0P-REC NOT = "Y"                                 
      *        MOVE "N" TO RLT0A10 IN PPRLT0P-REC                               
      *    END-IF.                                                              
                                                                                
           IF  RLT0291 IN PPRLT0P-REC = "Y"                                     
               MOVE RLT0210 IN PPRLT0P-REC TO RLT0B10 IN PPRLT0P-REC            
               MOVE RLT0211 IN PPRLT0P-REC TO RLT0B11 IN PPRLT0P-REC            
               MOVE RLT0220 IN PPRLT0P-REC TO RLT0B20 IN PPRLT0P-REC            
               MOVE RLT0221 IN PPRLT0P-REC TO RLT0B21 IN PPRLT0P-REC            
               MOVE RLT0222 IN PPRLT0P-REC TO RLT0B22 IN PPRLT0P-REC            
               MOVE RLT0224 IN PPRLT0P-REC TO RLT0B24 IN PPRLT0P-REC            
               MOVE RLT0230 IN PPRLT0P-REC TO RLT0B30 IN PPRLT0P-REC            
               MOVE RLT0280 IN PPRLT0P-REC TO RLT0B90 IN PPRLT0P-REC            
               MOVE RLT0A91 IN PPRLT0P-REC TO RLT0B91 IN PPRLT0P-REC            
                                                                                
               MOVE RLT0261 IN PPRLT0P-REC TO RLT0B61 IN PPRLT0P-REC            
               MOVE RLT0262 IN PPRLT0P-REC TO RLT0B62 IN PPRLT0P-REC            
               MOVE RLT0263 IN PPRLT0P-REC TO RLT0B63 IN PPRLT0P-REC            
               MOVE RLT0264 IN PPRLT0P-REC TO RLT0B64 IN PPRLT0P-REC            
               MOVE RLT0265 IN PPRLT0P-REC TO RLT0B65 IN PPRLT0P-REC            
               MOVE RLT0266 IN PPRLT0P-REC TO RLT0B66 IN PPRLT0P-REC            
               MOVE RLT0267 IN PPRLT0P-REC TO RLT0B67 IN PPRLT0P-REC            
               MOVE RLT0268 IN PPRLT0P-REC TO RLT0B68 IN PPRLT0P-REC            
               IF  RLT0271 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B71 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0271 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0272 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B72 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0272 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0273 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B73 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0273 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0274 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B74 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0274 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0275 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B75 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0275 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0276 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B76 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0276 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0277 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B77 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0277 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
               IF  RLT0278 IN PPRLT0P-REC > 0                                   
                   COMPUTE RLT0B78 IN PPRLT0P-REC = 0.001425                    
                           - (RLT0278 IN PPRLT0P-REC / 1000000)                 
               END-IF                                                           
           END-IF.                                                              
      *    IF  RLT0291 IN PPRLT0P-REC NOT = "Y"                                 
      *        MOVE "N" TO RLT0B10 IN PPRLT0P-REC                               
      *    END-IF.                                                              
      *                                                                         
       8500-WRITE-TO-JOURNAL.                                                   
           IF  PPRLT0P-NORMAL                     AND ,                         
               WK-OLD-RECORD NOT = WK-NEW-RECORD  AND ,                         
               FORCE IN DATAPAGE1-I NOT = "PAUL "                               
               INITIALIZE PPJRN10PR                                             
               MOVE CORR PPRLT0PR IN WK-NEW-RECORD TO EXTERNALLY-KEY            
               MOVE LINK-PROGRAM-CODE     TO JRNPROGRAM                         
               MOVE LINK-BRANCH-CODE      TO JRNBRANCH                          
               MOVE ZEROS                 TO TARGETDATE                         
               MOVE PANGSUB500-TW-DATE    TO JRNDATE                            
               MOVE SYSTEM-TIME           TO JRNTIME                            
               MOVE PANGCL150-USER-ID     TO JRNUSER                            
               MOVE SPACES                TO JRNREASON                          
               MOVE EXTERNALLY-KEY        TO JRNID                              
               IF  ADD-MODE                                                     
                   MOVE "PT"              TO JRNTYPE                            
                   MOVE ZEROS             TO JRNRECORD                          
                   MOVE WK-NEW-RECORD     TO JRNRECORD                          
                   CALL "PANGSUB440" USING CALL-PANGSUB440-ARGUMENT             
               ELSE                                                             
                   MOVE "UB"              TO JRNTYPE                            
                   MOVE ZEROS             TO JRNRECORD                          
                   MOVE WK-OLD-RECORD     TO JRNRECORD                          
                   CALL "PANGSUB440" USING CALL-PANGSUB440-ARGUMENT             
                   MOVE "UP"              TO JRNTYPE                            
                   MOVE ZEROS             TO JRNRECORD                          
                   MOVE WK-NEW-RECORD     TO JRNRECORD                          
                   CALL "PANGSUB440" USING CALL-PANGSUB440-ARGUMENT             
               END-IF                                                           
           END-IF.                                                              
      *                                                                         
       8900-SEND-TO-DATAQ.                                                      
           IF  PPRLT0P-NORMAL                                                   
      *        WK-OLD-RECORD NOT = WK-NEW-RECORD                                
               INITIALIZE DTAQ01 CALL-QSNDDTAQ01-ARGUMENT                       
      *      株                                         
               IF  ADD-MODE                                                     
                   MOVE "A" TO DTAQ01-MSGTYPE                                   
               ELSE                                                             
                   MOVE "C" TO DTAQ01-MSGTYPE                                   
               END-IF                                                           
               MOVE PANGSUB500-TW-DATE TO DTAQ01-DATE                           
               COMPUTE DTAQ01-TIME =  SYSTEM-TIME / 100                         
               MOVE CORR PPRLT0PR IN WK-NEW-RECORD TO DTAQ01                    
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDCS0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO CS0010                                  
               MOVE RLT0020 IN WK-NEW-RECORD TO CS0020                          
               MOVE "Y" TO SEREDCS0B-STATUS                                     
               CALL "SEREDCS0B" USING SEREDCS0B-STATUS SECS0P-RECORD            
               MOVE CS0025                 TO DTAQ01-RLT0020-CS0025             
               MOVE CS0030                 TO DTAQ01-RLT0020-CS0030             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDCS0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO CS0010                                  
               MOVE RLT0240 IN WK-NEW-RECORD TO CS0020                          
               MOVE "Y" TO SEREDCS0B-STATUS                                     
               CALL "SEREDCS0B" USING SEREDCS0B-STATUS SECS0P-RECORD            
               MOVE CS0025                 TO DTAQ01-RLT0240-CS0025             
               MOVE CS0030                 TO DTAQ01-RLT0240-CS0030             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0230 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0230-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0230-SA0050             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0430 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0430-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0430-SA0050             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0530 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0530-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0530-SA0050             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0630 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0630-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0630-SA0050             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0A30 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0A30-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0A30-SA0050             
      *---------------------------------------------------------------*         
               INITIALIZE CALL-SEREDSA0B-ARGUMENT                               
               MOVE LINK-BRANCH-CODE TO SA0010                                  
               MOVE RLT0B30 IN WK-NEW-RECORD TO SA0020                          
               CALL "SEREDSA0B" USING SEREDSA0B-STATUS SESA0P-RECORD            
               MOVE SA0100                 TO DTAQ01-RLT0B30-SA0100             
               MOVE SA0050                 TO DTAQ01-RLT0B30-SA0050             
      *---------------------------------------------------------------*         
               MOVE "MONLIB"         TO CALL-QSNDDTAQ01-LIBRARY                 
               MOVE PANGSUB110-HST300 TO CALL-QSNDDTAQ01-DTAQ01-NAME            
               MOVE DTAQ01            TO CALL-QSNDDTAQ01-DATA                   
               MOVE 2000              TO CALL-QSNDDTAQ01-DTAQ01-LENGTH          
               IF  CALL-QSNDDTAQ01-DTAQ01-NAME NOT = SPACES                     
                   CALL "QSNDDTAQ" USING CALL-QSNDDTAQ01-DTAQ01-NAME            
                                         CALL-QSNDDTAQ01-LIBRARY                
                                         CALL-QSNDDTAQ01-DTAQ01-LENGTH          
                                         CALL-QSNDDTAQ01-DATA                   
               END-IF                                                           
           END-IF.                                                              
      *                                                                         
       9000-START-PPRLT0P-RTN.                                                  
           START PPRLT0P KEY IS >= EXTERNALLY-DESCRIBED-KEY.                    
           IF  NOT PPRLT0P-NORMAL AND ,                                         
               NOT PPRLT0P-NOT-FOUND                                            
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0030"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9000-START-SECSAP-RTN.                                                   
           START SECSAP KEY IS >= EXTERNALLY-DESCRIBED-KEY.                     
           IF  NOT SECSAP-NORMAL AND ,                                          
               NOT SECSAP-NOT-FOUND                                             
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0030"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "SECSAP    "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE SECSAP-FILE-STATUS TO PANGCL040-FILE-STATUS-O               
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9100-READ-PPRLT0P-NEXT-RTN.                                              
           IF  RECORD-LOCK                                                      
               READ PPRLT0P NEXT                                                
           ELSE                                                                 
               READ PPRLT0P NEXT NO LOCK                                        
           END-IF.                                                              
           IF  NOT PPRLT0P-NORMAL AND ,                                         
               NOT PPRLT0P-AT-END                                               
               MOVE "N" TO PROCESS-SW                                           
               IF  PPRLT0P-RECORD-LOCK                                          
                   MOVE "FIO0060"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               ELSE                                                             
                   MOVE "FIO0040"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               END-IF                                                           
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9100-READ-SECSAP-NEXT-RTN.                                               
           READ SECSAP NEXT.                                                    
           IF  NOT SECSAP-NORMAL AND ,                                          
               NOT SECSAP-AT-END                                                
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0040"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "SECSAP    "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE SECSAP-FILE-STATUS TO PANGCL040-FILE-STATUS-O               
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9200-READ-PPRLT0P-PRIOR-RTN.                                             
           IF  RECORD-LOCK                                                      
               READ PPRLT0P PRIOR                                               
           ELSE                                                                 
               READ PPRLT0P PRIOR NO LOCK                                       
           END-IF.                                                              
           IF  NOT PPRLT0P-NORMAL AND ,                                         
               NOT PPRLT0P-AT-END                                               
               MOVE "N" TO PROCESS-SW                                           
               IF  PPRLT0P-RECORD-LOCK                                          
                   MOVE "FIO0060"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               ELSE                                                             
                   MOVE "FIO0050"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               END-IF                                                           
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9300-READ-PPRLT0P-FIRST-RTN.                                             
           IF  RECORD-LOCK                                                      
               READ PPRLT0P FIRST                                               
           ELSE                                                                 
               READ PPRLT0P FIRST NO LOCK                                       
           END-IF.                                                              
           IF  NOT PPRLT0P-NORMAL AND ,                                         
               NOT PPRLT0P-AT-END                                               
               MOVE "N" TO PROCESS-SW                                           
               IF  PPRLT0P-RECORD-LOCK                                          
                   MOVE "FIO0060"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               ELSE                                                             
                   MOVE "FIO0100"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               END-IF                                                           
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9400-READ-PPRLT0P-LAST-RTN.                                              
           IF  RECORD-LOCK                                                      
               READ PPRLT0P LAST                                                
           ELSE                                                                 
               READ PPRLT0P LAST NO LOCK                                        
           END-IF.                                                              
           IF  NOT PPRLT0P-NORMAL AND ,                                         
               NOT PPRLT0P-AT-END                                               
               MOVE "N" TO PROCESS-SW                                           
               IF  PPRLT0P-RECORD-LOCK                                          
                   MOVE "FIO0060"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               ELSE                                                             
                   MOVE "FIO0110"       TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               END-IF                                                           
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
++++++ 9500-READ-PPSYS0P-RANDOM-RTN.                                            
  .        READ PPSYS0P.                                                        
  .        IF  NOT PPSYS0P-NORMAL AND ,                                         
  .            NOT PPSYS0P-NOT-FOUND                                            
  .            MOVE "N" TO PROCESS-SW                                           
  .            MOVE "FIO0030"           TO PANGCL040-MESSAGE-CODE-O             
  .                                        LINK-MSG-CODE                        
  .            MOVE "PPSYS0P   "        TO PANGCL040-FILE-NAME-O                
  .                                        LINK-MSG-FILE-NAME                   
  .            MOVE PPSYS0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
  .                                        LINK-MSG-FILE-STATUS                 
++++++     END-IF.                                                              
      *                                                                         
++++++ 9500-READ-PPBRN1P-RANDOM-RTN.                                            
  .        READ PPBRN1P.                                                        
  .        IF  NOT PPBRN1P-NORMAL AND ,                                         
  .            NOT PPBRN1P-NOT-FOUND                                            
  .            MOVE "N" TO PROCESS-SW                                           
  .            MOVE "FIO0030"           TO PANGCL040-MESSAGE-CODE-O             
  .                                        LINK-MSG-CODE                        
  .            MOVE "PPBRN1P   "        TO PANGCL040-FILE-NAME-O                
  .                                        LINK-MSG-FILE-NAME                   
  .            MOVE PPBRN1P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
  .                                        LINK-MSG-FILE-STATUS                 
++++++     END-IF.                                                              
      *                                                                         
++++++ 9500-READ-PPRLT0L1-RANDOM-RTN.                                           
  .        READ PPRLT0L1.                                                       
  .        IF  NOT PPRLT0L1-NORMAL AND ,                                        
  .            NOT PPRLT0L1-NOT-FOUND                                           
  .            MOVE "N" TO PROCESS-SW                                           
  .            MOVE "FIO0020"           TO PANGCL040-MESSAGE-CODE-O             
  .                                        LINK-MSG-CODE                        
  .            MOVE "PPRLT0L1  "        TO PANGCL040-FILE-NAME-O                
  .                                        LINK-MSG-FILE-NAME                   
  .            MOVE PPRLT0L1-FILE-STATUS TO PANGCL040-FILE-STATUS-O             
  .                                        LINK-MSG-FILE-STATUS                 
++++++     END-IF.                                                              
      *                                                                         
++++++ 9500-READ-PPRLT0P-RANDOM-RTN.                                            
           IF  RECORD-LOCK                                                      
  .            READ PPRLT0P                                                     
           ELSE                                                                 
  .            READ PPRLT0P NO LOCK                                             
           END-IF.                                                              
  .        IF  NOT PPRLT0P-NORMAL AND ,                                         
  .            NOT PPRLT0P-NOT-FOUND                                            
  .            MOVE "N" TO PROCESS-SW                                           
  .            MOVE "FIO0020"           TO PANGCL040-MESSAGE-CODE-O             
  .                                        LINK-MSG-CODE                        
  .            MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
  .                                        LINK-MSG-FILE-NAME                   
  .            MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
  .                                        LINK-MSG-FILE-STATUS                 
++++++     END-IF.                                                              
      *                                                                         
       9600-WRITE-PPRLT0P-RTN.                                                  
           WRITE PPRLT0P-REC.                                                   
           IF  NOT PPRLT0P-NORMAL                                               
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0070"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9700-REWRITE-PPRLT0P-RTN.                                                
           REWRITE PPRLT0P-REC.                                                 
           IF  NOT PPRLT0P-NORMAL                                               
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0080"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
       9800-DELETE-PPRLT0P-RTN.                                                 
           DELETE PPRLT0P.                                                      
           IF  NOT PPRLT0P-NORMAL                                               
               MOVE "N" TO PROCESS-SW                                           
               MOVE "FIO0090"           TO PANGCL040-MESSAGE-CODE-O             
                                           LINK-MSG-CODE                        
               MOVE "PPRLT0P   "        TO PANGCL040-FILE-NAME-O                
                                           LINK-MSG-FILE-NAME                   
               MOVE PPRLT0P-FILE-STATUS TO PANGCL040-FILE-STATUS-O              
                                           LINK-MSG-FILE-STATUS                 
           END-IF.                                                              
      *                                                                         
