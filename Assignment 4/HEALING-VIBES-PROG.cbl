       IDENTIFICATION DIVISION.
       PROGRAM-ID.     HEALING-VIBES-PROG.
       AUTHOR.     AUSTIN_OGLETREE.
      **********************************************************
      *  This program is designed to take one input file,      
      *  "PR4F22-VIBESINVENmoreerrors.TXT", and split it into
      *  five files.
      *  
      *  These five files are named as ERROR.txt, PR4F22-LAX1.txt,
      *  PR4F22-SEA1.txt, PR4F22-SLC1.txt, and PR4F22-SLC2.txt.
      *  
      *  The purpose of this is to assist our example client,
      *  Dr. Drakea. Contained within our primary file is an
      *  unordered list of her various healing creams with
      *  them listed by first where they come from, their name,
      *  and so on and so forth. 
      *  
      *  Errors exist within the files, and must be sorted out 
      *  to the aforementioned error file using a sort merge.
      *  At first we are only looking for files that are 
      *  erroneous based upon their location codes, which
      *  are the first four digits of each entry.
      *  
      *  I use one temp file for this, my SD SORT-FILE. The goal
      *  is to sort them into where they go based on location,
      *  we are only interested in the Utah locations. The loc-
      *  ations are warehouses that store her products.
      *  
      *  FOR THE MERGE,
      *  
      *  The ASCENDING KEYS are: Warehouse ID: Major.
      *                          Vendor ID:    Intermediate.     
      *                          Product ID:   Minor.
      *  
      *  Using the sorted files and after dividing them into the
      *  five files, we need to merge the SLC1 and SLC2 files
      *  into one file we name Utah.txt.
      *  
      *  We want to display the number of error files to the
      *  DISPLAY as well.
      *  
      *  After this point, we must then make a report that 
      *  displays all of the information correctly, taking
      *  into account the errors present in these files as well.
      *  I made functions to accomplish this, like displaying
      *  adjusted product names, types, and expanded warehouse
      *  names and vendors.
      *  
      *  We want to make the report seperate each total product
      *  amount based on warehouse, vendor, and product type.
      *  
      *  To accomplish this I use a TRIPLE CONTROL BREAK,
      *  USING THE SAME ASCENDING KEYS used for the merge.
      *  
      *  The program must accumulate totals across these breaks,
      *  as well as a final grand total for every item within
      *  the report. Important to note, some products do not 
      *  contain actual information pertaining to the product.
      *  
      *  Failure to account for this will lead to your math
      *  being wrong.
      *  
      *  Within are comments that should be helpful to 
      *  understandning the logic of the program.
      *           
      **********************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBMPC.
       OBJECT-COMPUTER.    IBMPC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *  
      *  Simple File Control and assigning.
      *  

           SELECT UNSORTED-VIBES-INVEN 
               ASSIGN TO 'PR4F22-VIBESINVENmoreerrors.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-VIBES-INVEN
               ASSIGN TO 'SORTED-VIBES.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LAX1-FILE
               ASSIGN TO 'PR4F22-LAX1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SEA1-FILE
               ASSIGN TO 'PR4F22-SEA1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SLC1-FILE
               ASSIGN TO 'PR4F22-SLC1.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SLC2-FILE
               ASSIGN TO 'PR4F22-SLC2.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE
               ASSIGN TO 'ERROR.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UTAH-FILE
               ASSIGN TO 'UTAH.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.


      *             
      *  This is the TEMPORARY file used for the sort.
      *  

           SELECT SORT-FILE
               ASSIGN TO 'SORTINGFILE.TMP'.

      *  
      *  Our final report is the VIBE-REPORT.TXT.
      *  

           SELECT VIBE-REPORT
               ASSIGN TO PRINTER 'VIBE-REPORT.TXT'.

    

       DATA DIVISION.
       FILE SECTION.

      *  
      *  The following is the storage areas for
      *  all of the files.
      *  

       FD UNSORTED-VIBES-INVEN
           RECORD CONTAINS 128 CHARACTERS.
       01  UNSORTED-VIBES.
           05  WAREHOUSE-ID-IN                  PIC X(4).
           05  VENDOR-ID-IN                     PIC A.
           05  PRODUCT-ID-IN                    PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               

       FD SORTED-VIBES-INVEN
           RECORD CONTAINS 128 CHARACTERS.
       01  SORTED-VIBES.
           05  WAREHOUSE-ID-D                   PIC X(4).
           05  VENDOR-ID-D                      PIC A.
           05  PRODUCT-ID-D                     PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               

       FD LAX1-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  LAX1.
           05  WAREHOUSE-ID-LAX1                PIC X(4).
           05  VENDOR-ID                        PIC A.
           05  PRODUCT-ID                       PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               

       FD SEA1-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  SEA1.
           05  WAREHOUSE-ID-SEA1                PIC X(4).
           05  VENDOR-ID                        PIC A.
           05  PRODUCT-ID                       PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               
    
       FD SLC1-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  SLC1.
           05  WAREHOUSE-ID-SLC1                PIC X(4).
           05  VENDOR-ID                        PIC A.
           05  PRODUCT-ID                       PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               

       FD SLC2-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  SLC2.
           05  WAREHOUSE-ID-SLC2                PIC X(4).
           05  VENDOR-ID                        PIC A.
           05  PRODUCT-ID                       PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
               

       FD ERROR-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  ERROR-FILE-FIELD.
           05  WAREHOUSE-ID-ERROR               PIC X(4).
           05  VENDOR-ID                        PIC A.
           05  PRODUCT-ID                       PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.
              

       FD UTAH-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  UTAH.
           05  WAREHOUSE-ID-UTAH                PIC X(4).
           05  VENDOR-ID-UTAH                   PIC A.
           05  PRODUCT-ID-UTAH                  PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN-UTAH OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.

      *  
      *  Important, this is the SD SORT file below.
      *  
               


       SD SORT-FILE
           RECORD CONTAINS 128 CHARACTERS.
       01  SORT-RECORDS.
           05  WAREHOUSE-ID-SORT                PIC X(4).
           05  VENDOR-ID-SORT                   PIC A.
           05  PRODUCT-ID-SORT                  PIC X(3).
           05  PRODUCT-DATA-ARRAY-IN OCCURS 5 TIMES.
               10 PRODUCT-NAME-IN               PIC X(13).
               10 PRODUCT-SIZE-IN               PIC A.
               10 PRODUCT-TYPE-IN               PIC A.
               10 NUM-IN-STOCK-IN               PIC 9(4).
               10 PURCHASE-PRICE-IN             PIC S999V99.

               
      *  
      *  VIBE-REPORT is used to print the actual report.
      *  

       FD    VIBE-REPORT
             RECORD CONTAINS 80 CHARACTERS.

       01    VIBE-OUTPUT-REC            PIC X(80).
      *********
       WORKING-STORAGE SECTION.

      *  
      *  Below are important variables for use later.
      *
      * The TOTALS are used to store the totals.
      * CAL1 is used to calculate the totals. It is moved
      * later into the TOTAL fields.
      * 
      * ERRORNUM is what is used to display the error mess-
      * age for the DISPLAY
      * 
      * SUB holds the index for our arrays.
      * 
      * PROD-NAME-OUT2 is important later, to use with 
      * the product total names.
      *  
       
       01    WS-WORK-AREAS.

             05    TOTAL-PROD-COST           PIC 99999999V99     .
             05    TOTAL-VENDOR-COST         PIC 99999999V99     .
             05    TOTAL-WAREHOUSE-COST      PIC 99999999V99     .
             05    CAL1                      PIC 99999999V99     .
             05    THE-GRAND-TOTAL           PIC 99999999V99     .
             05    ERRORNUM                  PIC 99              . 
             05    SUB                       PIC 9 VALUE 1.
             05    PROD-NAME-OUT2            PIC X(13).

      * 
      *   Flags and switches hold the values neccessary for
      *  reading the loop and the array index, respectively.
      * 

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05 FIRST-RECORD                             VALUE 'Y'.
      * 
      *  The hold field is for the control break between the
      *  WAREHOUSE, VENDOR, and PRODUCT holds.
      * 

       01 HOLD-FIELD.
           05  WAREHOUSE-HOLD                   PIC X(4).
           05  VENDOR-HOLD                      PIC X(1).
           05  PRODUCT-HOLD                     PIC X.
           
      *      
      *  Current date is to correctly print the date.
      *  Merely for formatting reasons.
      * 

       01  CURRENT-DATE.
           05  CD-YEAR             PIC XXXX.
           05  CD-MONTH            PIC XX.
           05  CD-DAY              PIC XX.

      
      * 
      *  This array holds the incoming data from our
      * UTAH file.
      * 

       01  PRODUCT-ARRAY-OUT OCCURS 5 TIMES.
             05 PRODUCT-NAME-OUT               PIC X(13).
             05 PRODUCT-SIZE-OUT               PIC A.
             05 PRODUCT-TYPE-OUT              PIC A.
             05 NUM-IN-STOCK-OUT               PIC 9(4).
             05 PURCHASE-PRICE-OUT             PIC S999V99.
             05  FILLER                    PIC X(4) VALUE SPACES.

    
      *  
      *  This following code block is a TABLE, used to
      *  correctly display the vendors of each appropriate
      *  product by matching our single ALPHANUMERIC value
      *  to be expanded.
      *     

       01 VENDOR-TEXT.

          05 PIC X(15)           VALUE 'MAD HATTER OILS'.
          05 PIC X(15)           VALUE 'PURE CREAMS'.
          05 PIC X(15)           VALUE 'CHEEBS HERBS'.

       01 VENDOR-TABLE REDEFINES
          VENDOR-TEXT OCCURS 3 TIMES
          INDEXED BY VEN-INDEX.
              05 VEND-ID-TABLE              PIC X.
              05 VEND-NAME-TABLE            PIC X(14).
       

      *************************OUTPUT AREA*****************************
      
      *  
      *  The output area is self explanatory.
      *  It is used to print to the screen.
      *  
      *  However, some names may be confusing later.
      *  I intend to point them out.
      *  
      *  WRITE-LINE is used for a later function.
      *  
  
        01 WRITE-LINE.
           05 FILLER         PIC X VALUE SPACES.

        01 REPORT-HEADER-1.
          05 FILLER          PIC X(34) VALUE SPACES.
          05 REPORT-LINE     PIC X(13) VALUE 'HEALING VIBES'.
          

        01 REPORT-HEADER-2.
          
          05 FILLER PIC X(10) VALUES SPACES.

          05  H1-DATE.
               10  H1-MONTH        PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-DAY          PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-YEAR         PIC XXXX.

          

          05 FILLER            PIC X(13) VALUE SPACES.
          05 INVEN-REPORT      PIC X(16) VALUE 'INVENTORY REPORT'.
          
          05 FILLER            PIC X(11) VALUE SPACES.
          05 UTAH              PIC X(4)  VALUE 'UTAH'.
      
       
      *  
      *  WAREHOUSE-OUT Displays the expanded Warehouse name.
      *            

       01 WAREHOUSE-HEADER.
          05 FILLER          PIC X(2) VALUE SPACES.
          05 WAREHOUSE       PIC X(11) VALUE 'WAREHOUSE: '.
          05 WAREHOUSE-OUT   PIC X(16)             .

          
      *  
      *  VENDOR-OUT does the same as WAREHOUSE-OUT.
      *  

       01 VENDOR-HEADER.
          05 FILLER           PIC X(5) VALUE SPACES .
          05 VENDOR           PIC X(8) VALUE 'VENDOR: '.

          05 VENDOR-OUT       PIC X(15).

      *  
      *  PRODUCT-LINE holds no useful data besides display
      *  purposes.
      *  

       01 PRODUCT-LINE.
          05 FILLER           PIC X(8) VALUE SPACES .
          05 PRODUCT          PIC X(7) VALUE 'PRODUCT'.

          05 FILLER           PIC X(7) VALUE SPACES.
          05 PROD             PIC X(4) VALUE 'PROD'.

          05 FILLER           PIC X(4) VALUE SPACES.
          05 PRODUCT2         PIC X(7) VALUE 'PRODUCT'.

          05 VILLER           PIC X(5) VALUE SPACES.
          05 PROD2            PIC X(4) VALUE 'PROD'.

          05 FILLER           PIC X(5) VALUE SPACES.
          05 WORD-IN          PIC X(2) VALUE 'IN'.

          05 FILLER           PIC X(7) VALUE SPACES.
          05 WORD-TOTAL       PIC X(5) VALUE 'TOTAL'.

      *  
      *  Same as the above line.
      *  

       01 NAME-LINE.
          
          05 FILLER                 PIC X(10) VALUE SPACES.
          05 NAME                   PIC X(4)  VALUE 'NAME'.

          05 FILLER                 PIC X(9) VALUE SPACES.
          05 WORD-ID                PIC X(2) VALUE 'ID'.

          05 FILLER                 PIC X(6) VALUE SPACES.
          05 THE-WORD-SIZE          PIC X(4) VALUE 'SIZE'.

          05 FILLER                 PIC X(7) VALUE SPACES.               
          05 WORD-TYPE              PIC X(4) VALUE 'TYPE'.
          
          05 FILLER                 PIC X(4) VALUE SPACES.     
          05 STOCK                  PIC X(5) VALUE 'STOCK'.

          05 FILLER                 PIC X(5) VALUE SPACES.
          05 COST                   PIC X(4) VALUE 'COST'.

      *  
      *  The DETAIL-LINE1 is designed to hold and display
      *  most of the data for this report, like the number
      *  in stock of each item and their prices and such.
      *  
      *  I thought I may have needed a detail line 2,
      *  but I did not.
      *  

       01 DETAIL-LINE1.
          
          05 FILLER                 PIC X(5) VALUE SPACES.
          05 PROD-NAME-OUT          PIC X(13).

          05 FILLER                 PIC X(4) VALUE SPACES.
          05 PROD-ID-OUT            PIC X(3).

          05 FILLER                 PIC X(3) VALUE SPACES.
          05 PROD-SIZE-OUT          PIC X(11).

          05 FILLER                 PIC X(3) VALUE SPACES.
          05 PROD-TYPE-OUT          PIC X(5).

          05 FILLER                 PIC X(3) VALUE SPACES.
          05 PROD-STOCK-OUT         PIC Z999.

          05 FILLER                 PIC X(3) VALUE SPACES.
          05 TOTAL-DETAIL-OUT         PIC $,$$$,$$$.99.

      *  
      *  TOTAL-PRODUCT shows the total amount of each 
      *  product after all of their sizes have been read
      *  in. They are seperated, again, by Warehouse 
      *  location, vendor, and product type.
      *  
      *  The following TOTAL lines do the same, except
      *  for the total for each vendor and warehouse,
      *  respectively.
      *  
              

       01 TOTAL-PRODUCT.
          05 FILLER              PIC  X(15) VALUE SPACES  .
          05 TOTAL      PIC  X(15)  VALUE 'TOTAL PRODUCT: '.
  
          05 TOTAL-PROD-OUT            PIC  X(13).
          05 FILLER                    PIC  X(13).
         
          05 TOTAL-PRODUCT-OUT       PIC $$,$$$,$$$.99         .

       01 TOTAL-VENDOR.
          05 FILLER              PIC  X(12) VALUE SPACES  .
          05 TOTAL      PIC  X(18)  VALUE 'TOTAL FOR VENDOR: '.
  
          05 TOTAL-VEND-PROD-OUT            PIC  X(15).
          05 FILLER                    PIC  X(10).
         
          05 TOTAL-VENDOR-OUT       PIC $$$,$$$,$$$.99         .

      *   
      *  'TOTFORWAREHOU' is supposed to stand for
      *  Total For Warehouse, while the NAME version
      *  holds which total the warehouse is being displayed,
      *  and the OUT version is for the actual numbers of that.
      *  

       01 TOTAL-WAREHOUSE.
          05 FILLER              PIC X(9) VALUE SPACES.
          05 TOTFORWAREHOU  PIC X(21) VALUE 'TOTAL FOR WAREHOUSE: '.

          05 TOTFORWAREHOU-NAME      PIC X(16).

          05 FILLER                 PIC X(7) VALUE SPACES.
          05 TOTFORWAREHOUT-OUT      PIC $,$$$,$$$,$$$.99.

       01 GRAND-TOTAL.
          05 FILLER             PIC X(17) VALUE SPACES.
          05 WORD-GRAND-TOT     PIC X(17) VALUE 'GRAND TOTAL: UTAH'.
          05 GRAND-TOTAL-OUT    PIC $$,$$$,$$$,$$$.99.
      
       PROCEDURE DIVISION.

      *  
      *  The MAIN MODULE starts with the 120 SORT
      *  MERGE, displays the error count accumulated 
      *  from the sorts, and performs the 900 CLOSE
      *  ROUTINE.
      *  
      *  I loop through later functions to continue
      *  the program.
      *  

       100-MAIN-MODULE.

           PERFORM 120-SORT-MERGE
          
           DISPLAY ERRORNUM ' . RECORDS THAT HAD ERRORS.'

           PERFORM 900-CLOSE-ROUTINE
           

           .

      *  
      *  105 Writes a line, nothing more.
      *  
      *  It does use the WRITE-LINE 
      *  output variable mentioned earlier.
      *  

       105-WRITE-A-LINE.

        MOVE WRITE-LINE TO VIBE-OUTPUT-REC

        WRITE VIBE-OUTPUT-REC AFTER ADVANCING 1 LINES       

       .

      *  
      *  120 SORT MERGE opens all files, then proceeds
      *  to sort the original big file into five smaller
      *  files.
      *  
      *  We then open the file, the proceed to 130 READ
      *  DIVIDE FILE.
      *  
      *  What 130 does is perform a basic read of our new
      *  SORTED FILES file. 
      *  
      *  As it reads, it performs 140 INPUT SORT
      *  LOGIC, which seperates the files based upon
      *  the location of the products listed within
      *  the files.
      *  
      *  After this is done, 130 cedes control back to 120.
      *  
      *  From there it finishes the sort and merge, producing
      *  the Utah file, which is the main file we will be working
      *  with.
      *  
       120-SORT-MERGE.
           
          OPEN OUTPUT LAX1-FILE
               OUTPUT SEA1-FILE
               OUTPUT SLC1-FILE
               OUTPUT SLC2-FILE
               OUTPUT ERROR-FILE
                     


           SORT SORT-FILE
                ON ASCENDING KEY WAREHOUSE-ID-IN,
                   ASCENDING KEY VENDOR-ID-IN,
                   ASCENDING KEY PRODUCT-ID-IN
                USING UNSORTED-VIBES-INVEN
                GIVING SORTED-VIBES-INVEN

           

           OPEN INPUT SORTED-VIBES-INVEN
           PERFORM 130-READ-DIVIDE-FILE

           
           SORT SORT-FILE
                ON ASCENDING KEY WAREHOUSE-ID-SORT,
                   ASCENDING KEY VENDOR-ID-SORT,
                   ASCENDING KEY PRODUCT-ID-SORT
                USING SLC1-FILE,
                      SLC2-FILE
                GIVING SORTED-VIBES-INVEN

           MERGE SORT-FILE
                ON ASCENDING KEY WAREHOUSE-ID-SORT,
                                 VENDOR-ID-SORT,
                                 PRODUCT-ID-SORT
                                    
                USING SLC1-FILE,
                      SLC2-FILE
                GIVING UTAH-FILE

            CLOSE SEA1-FILE
            CLOSE ERROR-FILE
            CLOSE LAX1-FILE

            

            
            PERFORM 200-HOUSE-KEEPING
         
                
       .

      *  
      *  The aforementioned read function.
      *  

       130-READ-DIVIDE-FILE.
         

            PERFORM UNTIL NO-MORE-DATA
               READ SORTED-VIBES-INVEN
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 140-INPUT-SORT-LOGIC

               END-READ
           END-PERFORM

         
           
         
       .

      *  
      *  140, which sorts the files based upon on
      *  the location of the products.
      *  
      

       140-INPUT-SORT-LOGIC.

          

          EVALUATE TRUE
           
             WHEN WAREHOUSE-ID-D EQUALS 'LAX1'
               MOVE SORTED-VIBES TO LAX1
               WRITE LAX1

             WHEN WAREHOUSE-ID-D EQUALS 'SEA1'
               MOVE SORTED-VIBES TO SEA1
               WRITE SEA1

             WHEN WAREHOUSE-ID-D EQUALS 'SLC1'
               MOVE SORTED-VIBES TO SLC1
               WRITE SLC1

             WHEN WAREHOUSE-ID-D EQUALS 'SLC2'
               MOVE SORTED-VIBES TO SLC2
               WRITE SLC2

             WHEN WAREHOUSE-ID-D NOT EQUALS 
               'LAX1' OR 'SEA1' OR 'SLC1' OR 'SLC2'
               MOVE SORTED-VIBES TO ERROR-FILE-FIELD 
               ADD 1 TO ERRORNUM
               WRITE ERROR-FILE-FIELD

          END-EVALUATE

        
                
       . 

      *  
      *  200 HOUSEKEEPING is the start of phase 2 of the program,
      *  actually writing the report.
      *  
      *  It opens our VIBE REPORT, to write to our report file.
      *  Then, apply our date method to display our date.
      *  
      *  Finally, the program prepares the report file with its 
      *  headers for further writes, and move the program to the
      *  READ function, 230 READ UTAH.
      *  

         200-HOUSE-KEEPING.
        
         OPEN OUTPUT VIBE-REPORT

           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD

           MOVE CD-MONTH TO H1-MONTH
           MOVE CD-DAY TO H1-DAY
           MOVE CD-YEAR TO H1-YEAR

          PERFORM 105-WRITE-A-LINE

          MOVE REPORT-HEADER-1 TO VIBE-OUTPUT-REC
          WRITE VIBE-OUTPUT-REC AFTER ADVANCING 1 LINES
   
          MOVE REPORT-HEADER-2 TO VIBE-OUTPUT-REC
          WRITE VIBE-OUTPUT-REC AFTER ADVANCING 1 LINES

          PERFORM 105-WRITE-A-LINE          
           
          PERFORM 230-READ-UTAH 
         .

      *  
      *  You should come back to this to fully understand
      *  the flow of the program. 230 is after this one
      *  and one other.
      *  
      *  215 WRITE ARRAY does the bulk of the work of 
      *  editing, validating, and printing the data.
      *  
      *  I will enclose comments within it to help 
      *  comprehension of the program, since it
      *  is complicated.
      *  

         

         215-WRITE-ARRAY.

      *  
      *  Immediately I call the 240 REPORT PROCESS.
      *  240 is responsible for the control breaks,
      *  mostly. It also prints the total lines,
      *  using utility functions later.
      *  
      *  After completing the breaks to determine
      *  whether or not to print the detail line
      *  again or perform a total write, control
      *  returns to 215.
      *  


            
           PERFORM 240-REPORT-PROCESS

      *  
      *  This next block reads in the data from
      *  out UTAH file using a large PERFROM 
      *  VARYING, with several nested loops within.
      *  
      *  Again, all this function does is print the
      *  detail line to our specifications.
      *  

           PERFORM VARYING SUB
              FROM 1 BY 1 UNTIL SUB > 5

      *  
      *  This next block moves the array into the
      *  our storage array.
      *  
      *  Note the nested IF, its purpose is to 
      *  only put the first product name into the
      *  detail line between product breaks.
      *  
      *  The idea, is it only prints the first
      *  index. If it is not the first index,
      *  SPACES are moved to the detail line.
      *  

              MOVE PRODUCT-DATA-ARRAY-IN-UTAH(SUB) TO
                      PRODUCT-ARRAY-OUT(SUB)

                   IF SUB EQUAL 1
                   
                      MOVE PRODUCT-LINE TO VIBE-OUTPUT-REC   
                      WRITE VIBE-OUTPUT-REC
                      AFTER ADVANCING 1 LINES

                      MOVE NAME-LINE TO VIBE-OUTPUT-REC   
                      WRITE VIBE-OUTPUT-REC
                      AFTER ADVANCING 1 LINES  

                      PERFORM 105-WRITE-A-LINE                 

                      MOVE PRODUCT-NAME-OUT(SUB) TO PROD-NAME-OUT
                      MOVE PRODUCT-NAME-OUT(SUB) TO PROD-NAME-OUT2

                   ELSE 

                      MOVE SPACES TO PROD-NAME-OUT

                   END-IF

      *  
      *  This next block of code reads in the size 
      *  of our products into our storage array.
      *  
      *  Using a nested EVALUATE, we validate our
      *  data. We are only expecting X, L, M, or S.
      *  
      *  If data comes in without one of these characters,
      *  a REFERENCE modification is performed to indicate
      *  which bad character has been read in to cause
      *  the report to not list the size of the product.
      *  
           
           
              MOVE PRODUCT-SIZE-OUT(SUB) TO PROD-SIZE-OUT
                 EVALUATE TRUE
                   WHEN PROD-SIZE-OUT EQUALS 'X'
                       MOVE 'EXTRA LARGE' TO PROD-SIZE-OUT
                   WHEN PROD-SIZE-OUT EQUALS 'L'
                       MOVE 'LARGE' TO PROD-SIZE-OUT
                   WHEN PROD-SIZE-OUT EQUALS 'M'
                       MOVE 'MEDIUM' TO PROD-SIZE-OUT
                   WHEN PROD-SIZE-OUT EQUALS 'S'
                       MOVE 'SMALL' TO PROD-SIZE-OUT
                   WHEN PROD-SIZE-OUT NOT EQUALS 'X'
                   OR 'L' OR 'M' OR 'S'
                       MOVE 'BAD' TO PROD-SIZE-OUT
                       MOVE PRODUCT-SIZE-OUT(SUB) TO
                          PROD-SIZE-OUT (5:1)
                    
                 END-EVALUATE

      *  
      *  This block of code does the same as the above,
      *  but for the type of item, oil or cream.
      *  
      *  There are no errors in our files, so no code is
      *  implemented to account for such.
      *  
      *  It can be modified quickly to do so, much like the
      *  above block does, if neccessary.
      *  
                  

              MOVE PRODUCT-TYPE-OUT(SUB) TO PROD-TYPE-OUT

                   EVALUATE TRUE

                     WHEN PRODUCT-TYPE-OUT(SUB) EQUALS 
                      'C' MOVE 'CREAM' TO PROD-TYPE-OUT
 
                     WHEN PRODUCT-TYPE-OUT(SUB) EQUALS
                      'O' MOVE 'OIL' TO PROD-TYPE-OUT

                   END-EVALUATE

      *  
      *  This code moves the number in stock of each item
      *  to the correct output variable to print.
      *  
      *  The earlier mentioned errors of missing data on 
      *  some items are accounted for here.
      *  
      *  If the following data is not numeric, which
      *  it should be for counting the number in stock of 
      *  each item, zeros are moved to the fields responsible
      *  for our later calculations.
      *  
      *  This is to ensure no false accumulation of data
      *  happens for our totals.
      *  

              MOVE NUM-IN-STOCK-OUT(SUB) TO PROD-STOCK-OUT 
                   IF NUM-IN-STOCK-OUT(SUB) IS NUMERIC
                      MOVE NUM-IN-STOCK-OUT(SUB) TO
                      PROD-STOCK-OUT

                   ELSE MOVE 0 TO PROD-STOCK-OUT
                   MOVE 0 TO PURCHASE-PRICE-OUT(SUB)

                   END-IF

      *  
      *  This following block performs our basic calculation.
      *  It also moves the purchase price to the correct
      *  output division variables.
      *  
      *  Each variable was discussed earlier, they store
      *  the totals of each break that occurs in this program.
                               

              MOVE PURCHASE-PRICE-OUT(SUB) TO TOTAL-DETAIL-OUT

                  COMPUTE CAL1 = NUM-IN-STOCK-OUT(SUB) * 
                                 PURCHASE-PRICE-OUT(SUB)

              MOVE CAL1 TO TOTAL-DETAIL-OUT

              ADD CAL1 TO TOTAL-PROD-COST
              ADD CAL1 TO TOTAL-VENDOR-COST
              ADD CAL1 TO TOTAL-WAREHOUSE-COST 
              ADD CAL1 TO THE-GRAND-TOTAL

              MOVE 0 TO CAL1

              MOVE PRODUCT-ID-UTAH TO PROD-ID-OUT

              MOVE DETAIL-LINE1 TO VIBE-OUTPUT-REC
                 WRITE VIBE-OUTPUT-REC
                 AFTER ADVANCING 1 LINES

           
           
           

           

           END-PERFORM


         .

      *  
      *  220 VENDOR TABLE uses a TABLE to validate out vendor
      *  names. It checks against our read in values to the above
      *  mentioned TABLE within the WORKING STORAGE area.
      *  
      *  Provided a vendor does not match, it prints out 
      *  INVALID, followed by the letter that did not match
      *  what we should be expecting.
      *  
      *  If it is valid, we alter the DISPLAY OUTPUT
      *  variables to reflect the full name of the vendor.
      *  
      *  Both instances are done through REFERENCE MODIFICATION.
      *  


         220-VENDOR-TABLE.
          
           SET VEN-INDEX TO 1
         SEARCH VENDOR-TABLE
                  

              AT END

                  MOVE 'INVALID' TO VENDOR-OUT

                  MOVE VENDOR-HOLD 
                     TO VENDOR-OUT (9:1)
                  
                  MOVE VENDOR-HEADER TO VIBE-OUTPUT-REC
                  WRITE VIBE-OUTPUT-REC 
                  AFTER ADVANCING 2 LINES
                  PERFORM 105-WRITE-A-LINE


             WHEN VENDOR-HOLD (1:1) EQUALS VEND-ID-TABLE (VEN-INDEX)

                  MOVE VEND-ID-TABLE (VEN-INDEX) (1:1)
                      TO VENDOR-OUT(1:1)
                  MOVE VEND-NAME-TABLE (VEN-INDEX) (1:14) 
                      TO VENDOR-OUT(2:14)
                  MOVE VENDOR-HEADER TO VIBE-OUTPUT-REC
                  WRITE VIBE-OUTPUT-REC 
                  AFTER ADVANCING 2 LINES 
                  PERFORM 105-WRITE-A-LINE
 
                 

             END-SEARCH

 
         .

      *  
      *  230 READ UTAH reads the file.
      *  
      *  Until it reaches its end, it performs the
      *  215 WRITE ARRAY function, which is dicussed above.
      *  

         230-READ-UTAH.
          
          MOVE ' ' TO EOF-FLAG
          OPEN INPUT UTAH-FILE

          PERFORM UNTIL NO-MORE-DATA
               READ UTAH-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 215-WRITE-ARRAY
                       

               END-READ
           END-PERFORM

           .

      *  
      *  240 REPORT PROCESS is master control for the breaks
      *  within the program.
      *  
      *  Warehouse change triggers all of the breaks, to 
      *  list the total of the warehouse and vendors with
      *  the products.
      *  
      *  Vendor change or BREAK triggers itself and the product
      *  BREAK.
      *  
      *  Product change triggers itself to start a new detail line.
      *  
      *  It is important that only VENDOR BREAK writes its own total,
      *  or else the printing gets thrown off. Same for the WAREHOUSE
      *  BREAK. The product total is printed within its actual break
      *  function.
      *  



         240-REPORT-PROCESS.

            EVALUATE TRUE
              WHEN FIRST-RECORD = 'Y'
                   MOVE 'N' TO FIRST-RECORD
                   MOVE WAREHOUSE-ID-UTAH TO WAREHOUSE-HOLD
                   MOVE VENDOR-ID-UTAH TO VENDOR-HOLD
                   MOVE PRODUCT-ID-UTAH TO PRODUCT-HOLD
                   PERFORM 205-WRITE-WAREHOUSE
                   PERFORM 305-WRITE-VENDOR
                   
                   
              WHEN WAREHOUSE-ID-UTAH NOT EQUAL TO WAREHOUSE-HOLD
                   PERFORM 410-PRODUCT-BREAK
                   PERFORM 420-WRITE-TOTAL-VENDOR
                   PERFORM 105-WRITE-A-LINE
                   PERFORM 425-WRITE-TOTAL-WAREHOUSE
                   PERFORM 105-WRITE-A-LINE
                   PERFORM 210-WAREHOUSE-BREAK
                   PERFORM 310-VENDOR-BREAK

              WHEN VENDOR-ID-UTAH NOT EQUAL TO VENDOR-HOLD
                   PERFORM 410-PRODUCT-BREAK
                   PERFORM 420-WRITE-TOTAL-VENDOR
                   PERFORM 310-VENDOR-BREAK
                   

              WHEN PRODUCT-ID-UTAH NOT EQUAL TO PRODUCT-HOLD 
                  
                   PERFORM 410-PRODUCT-BREAK

            END-EVALUATE

           
            
         
           .
      
      *  
      *  205 Writes the name of the warehouse, as well
      *  as expands the name.
      *  

       205-WRITE-WAREHOUSE.
           
           
           IF WAREHOUSE-ID-UTAH EQUAL 'SLC1'
              MOVE 'UTAH WAREHOUSE 1' TO WAREHOUSE-OUT
           ELSE MOVE 'UTAH WAREHOUSE 2' TO WAREHOUSE-OUT
           
           END-IF

           MOVE WAREHOUSE-HEADER TO VIBE-OUTPUT-REC
           WRITE VIBE-OUTPUT-REC 
           AFTER ADVANCING 1 LINES
         .

      *  
      *  210 is the actual WAREHOUSE BREAK. It moves
      *  the newly encountered WAREHOUSE-ID-UTAH read
      *  in from the WRITE ARRAY function to the HOLD
      *  to correctly perform a CONTROL BREAK.
      *  

       210-WAREHOUSE-BREAK.
           
           MOVE WAREHOUSE-ID-UTAH TO WAREHOUSE-HOLD
           
           PERFORM 205-WRITE-WAREHOUSE
             
         .

      *  
      *  305 performs 220 to validate and expand
      *  the VENDOR-ID to the output report.
      *  
      *  Its place here is to be triggered by the
      *  following VENDOR BREAK.
      *  

         305-WRITE-VENDOR.
           PERFORM 220-VENDOR-TABLE
         
          

         .

      *  
      *  310 VENDOR BREAK prints the incoming vendor and
      *  the outgoing vendor by calling 220 to then write
      *  the relevant data.
      *  
      *  It also resets the running total for the VENDOR TOTAL.


 
         310-VENDOR-BREAK.
            MOVE 0 TO TOTAL-VENDOR-OUT
            MOVE VENDOR-ID-UTAH TO VENDOR-HOLD
            
            PERFORM 305-WRITE-VENDOR
         .

        
      *  
      *  410 CONTROL BREAKS based upon if a new 
      *  product is coming in.
      *  

         410-PRODUCT-BREAK.
          
          
          MOVE PRODUCT-ID-UTAH TO PRODUCT-HOLD
          PERFORM 415-WRITE-TOTAL-PRODUCT
          PERFORM 105-WRITE-A-LINE
         .
         
      *  
      *  415 WRITES the PRODUCT TOTAL to the detail
      *  line. The data must be fed into our working
      *  storage before being placed into our output
      *  division.
      *  

         415-WRITE-TOTAL-PRODUCT.
          PERFORM 105-WRITE-A-LINE
          MOVE TOTAL-PROD-COST TO TOTAL-PRODUCT-OUT
          MOVE PROD-NAME-OUT2 TO TOTAL-PROD-OUT
          MOVE TOTAL-PRODUCT TO VIBE-OUTPUT-REC
            WRITE VIBE-OUTPUT-REC
            AFTER ADVANCING 1 LINES
          MOVE 0 TO TOTAL-PROD-COST
          
         .

      *  
      *  420 does the same as the above but for 
      *  VENDOR TOTALS.
      *  

         420-WRITE-TOTAL-VENDOR.
          MOVE TOTAL-VENDOR-COST TO TOTAL-VENDOR-OUT
          MOVE VENDOR-OUT TO TOTAL-VEND-PROD-OUT
          MOVE TOTAL-VENDOR TO VIBE-OUTPUT-REC
            WRITE VIBE-OUTPUT-REC
            AFTER ADVANCING 1 LINES
          MOVE 0 TO TOTAL-VENDOR-COST
         .

      *  
      *  425 also does the same as 415 and 420,
      *  but for the WAREHOUSE TOTAL.
      *  

         425-WRITE-TOTAL-WAREHOUSE.
          MOVE TOTAL-WAREHOUSE-COST TO TOTFORWAREHOUT-OUT
          MOVE WAREHOUSE-OUT TO TOTFORWAREHOU-NAME
          MOVE TOTAL-WAREHOUSE TO VIBE-OUTPUT-REC
            WRITE VIBE-OUTPUT-REC
            AFTER ADVANCING 1 LINES
          MOVE 0 TO TOTAL-WAREHOUSE-COST
         .

      *  
      *  430 WRITE FINAL TOTAL writes the total
      *  for the current product, vendor, warehouse,
      *  and total of the amount of products for the Doctor
      *  within the State of UTAH.
      *  
      *  And it moves that info to the total line.
      *  

         430-WRITE-FINAL-TOTAL.
          PERFORM 415-WRITE-TOTAL-PRODUCT
          PERFORM 105-WRITE-A-LINE
          PERFORM 420-WRITE-TOTAL-VENDOR
          PERFORM 105-WRITE-A-LINE
          PERFORM 425-WRITE-TOTAL-WAREHOUSE

          MOVE THE-GRAND-TOTAL TO GRAND-TOTAL-OUT
          MOVE GRAND-TOTAL TO VIBE-OUTPUT-REC
          WRITE VIBE-OUTPUT-REC
          AFTER ADVANCING 3 LINES

          .
       
      *  
      *  900 closes the last two files, and
      *  executes the 430 FINAL TOTAL function.
      *  
      *  900 is called from 100 MAIN after all 
      *  of UTAH has been read.
      *  

       900-CLOSE-ROUTINE.
            PERFORM 430-WRITE-FINAL-TOTAL

            CLOSE VIBE-REPORT
                  UTAH-FILE
                 
                 
              
                        

              STOP RUN
           .

      *  
      *  In short, we take one file, split
      *  it into 5, then take the two that 
      *  represent UTAH, and write a report
      *  of all of its information.
      *  
      *  That's it, thanks for reading!
      *  


