       IDENTIFICATION DIVISION.
       PROGRAM-ID. main.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 GLB PIC X(10) VALUE "GLOBAL".

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY GLB
      * // Set break point #1. //// break $source:$line
           DISPLAY WHEN-COMPILED.
           DISPLAY FUNCTION WHEN-COMPILED.
       STOP RUN.