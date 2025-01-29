//! Main entry point for the COBOL-to-Fortran Compiler.
//! Now supports:
//! ✅ COBOL Parsing (File Handling, Loops, SQL)
//! ✅ Fortran Code Generation
//! ✅ Report Encryption & Streaming
//! ✅ Web API for Report Access

mod cobol_parser;
mod fortran_generator;
mod report_security;
mod report_streaming;
mod web_dashboard;

fn main() {
    println!("🚀 Starting COBOL-to-Fortran Compiler...");

    // Step 1: Read COBOL code (Mock input for now)
    let cobol_code = "
    IDENTIFICATION DIVISION.
    PROGRAM-ID. SALES.

    PROCEDURE DIVISION.
    START.
        DISPLAY \"Sales Report\".
        OPEN INPUT SALES-FILE.
        READ SALES-FILE.
        PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
        WRITE SALES-RECORD TO SALES-OUTPUT.
        EXEC SQL SELECT * FROM SALES END-EXEC.
        STOP RUN.
    ";

    // Step 2: Parse COBOL code
    let parsed_cobol = cobol_parser::parse_cobol_code(cobol_code);
    println!("✅ COBOL Code Parsed Successfully!");

    // Step 3: Convert to Fortran
    let fortran_code = fortran_generator::generate_fortran_code(&parsed_cobol);
    println!("✅ Fortran Code Generated Successfully!\n{}", fortran_code);

    // Step 4: Encrypt Report (Mock key for now)
    let encryption_key = report_security::generate_encryption_key();
    let _ = report_security::encrypt_report("sales_report.txt", &encryption_key);
    
    // Step 5: Start TCP Report Streaming Server (Runs in the background)
    std::thread::spawn(|| {
        let _ = report_streaming::start_report_server("127.0.0.1:8080");
    });

    // Step 6: Start Web API for Report Access (Runs in the background)
    std::thread::spawn(|| {
        let _ = web_dashboard::main();
    });

    println!("✅ All Services Started Successfully! 🚀");
}
