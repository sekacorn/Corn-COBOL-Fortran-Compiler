# COBOL-to-Fortran Compiler

## 📌 Overview
This project is a **COBOL-to-Fortran Compiler** that takes COBOL source code as input, parses it, and generates equivalent Fortran code.  
It also includes **report security, real-time streaming, and a web dashboard** for live monitoring.

This compiler is designed for **junior developers** who want to learn about COBOL-to-Fortran translation and **enhance their understanding of both languages**.

---

## 🚀 Features
✅ **COBOL Parsing** → Converts COBOL code into an intermediate representation (AST).  
✅ **Fortran Code Generation** → Translates COBOL statements into Fortran equivalents.  
✅ **Advanced File Handling** → Supports `OPEN`, `READ`, and `WRITE` operations in COBOL.  
✅ **Loop Handling** → Implements `PERFORM UNTIL` and `PERFORM VARYING` loops.  
✅ **SQL Handling** → Detects `EXEC SQL ... END-EXEC` for database integration.  
✅ **Report Encryption** → Uses AES-256-GCM encryption for secure report storage.  
✅ **Real-time Report Streaming** → Streams report data over TCP for live updates.  
✅ **Web Dashboard** → Provides an API for viewing reports via a web browser.  
✅ **Authentication** → Uses API tokens to secure report access.  

---

## 📜 How to Use

### **1️⃣ Install Dependencies**
Ensure you have **Rust** installed on your system. If not, install it via:  
```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
Then, navigate to the project folder and run:
```sh
cd COBOL-Fortran-Compiler
cargo build
```

### **2️⃣ Running the Compiler**
To compile COBOL code into Fortran:
```sh
cargo run
```

### **3️⃣ Starting the Report Server**
To enable real-time report streaming:
```sh
cargo run --bin report_streaming
```
This will start a server on `127.0.0.1:8080`.

### **4️⃣ Fetching Reports via Web API**
Start the web dashboard API:
```sh
cargo run --bin web_dashboard
```
Then, open a browser and go to:
```
http://127.0.0.1:8080/report
```
Or fetch via `curl`:
```sh
curl -H "Authorization: Bearer SECRET_TOKEN" http://127.0.0.1:8080/report
```

---

## 📌 Example COBOL Code
A simple COBOL program that moves values and displays output:
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SALES.

PROCEDURE DIVISION.
START.
    DISPLAY "Sales Report".
    OPEN INPUT SALES-FILE.
    READ SALES-FILE.
    PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
    WRITE SALES-RECORD TO SALES-OUTPUT.
    EXEC SQL SELECT * FROM SALES END-EXEC.
    STOP RUN.
```

### 🔄 **Generated Fortran Code**
```fortran
PROGRAM SALES
  IMPLICIT NONE
  INTEGER :: SALES_RECORD

  CALL MAIN
  STOP
END PROGRAM SALES

SUBROUTINE START
  INTEGER :: SALES_FILE, SALES_OUTPUT
  OPEN(UNIT=10, FILE='SALES-FILE', STATUS='OLD')
  READ(UNIT=10, FMT=*) 
  DO WHILE (END-OF-FILE == .FALSE.)
    CALL PROCESS_RECORDS
  END DO
  WRITE(UNIT=10, FMT=*) SALES_RECORD
  ! EXEC SQL TRANSLATION NOT IMPLEMENTED
  ! EXEC SQL SELECT * FROM SALES END-EXEC
  STOP
END SUBROUTINE START
```

---

## 🔥 **Limitations of the COBOL-to-Fortran Compiler**
While this compiler provides a solid translation of COBOL to Fortran, there are **certain limitations**:

### **1️⃣ Incomplete Support for COBOL File Handling**
🚫 **Does not fully support complex COBOL file operations**, such as indexed and relative file access.  
🚫 **Does not handle sequential file processing as efficiently as COBOL**.  
🚫 **No automatic file record locking for concurrent processing**.  

### **2️⃣ Limited SQL Translation**
🚫 **Detects `EXEC SQL ... END-EXEC`, but does not translate it to functional Fortran SQL equivalents**.  
🚫 **No direct database connectivity**.  
🚫 **No support for embedded SQL statements within COBOL procedures**.  

### **3️⃣ Lack of COBOL REPORT-WRITER Handling**
🚫 **COBOL's `REPORT-WRITER` feature is not implemented**.  
🚫 **No built-in support for structured report formatting in Fortran**.  

### **4️⃣ No Support for COBOL Screens Section**
🚫 **COBOL’s `SCREENS SECTION` (UI-related features) are not translated into Fortran UI equivalents**.  
🚫 **User interfaces in COBOL are not directly mapped to Fortran programs**.  

### **5️⃣ Limited Support for COBOL Subroutine Calls**
🚫 **`CALL` statements are not yet fully mapped to Fortran subroutine calls**.  
🚫 **Inter-module COBOL procedure communication is not converted**.  

### **6️⃣ No Advanced COBOL Data Types Handling**
🚫 **Does not fully support COBOL’s `PIC` clauses and redefines**.  
🚫 **Fortran's data structures are simpler than COBOL's, causing possible loss of detail in conversion**.  

---

## 📬 Contributing

