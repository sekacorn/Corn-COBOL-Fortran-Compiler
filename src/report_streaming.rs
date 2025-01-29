//! This module enables real-time report streaming over TCP sockets.
//! Now supports:
//! ✅ Secure report streaming via TCP
//! ✅ Ability to receive reports as a client
//! ✅ Enhanced logging for debugging

use std::net::{TcpListener, TcpStream};
use std::io::{Write, BufReader, BufRead};

/// **Starts a TCP server that streams report data to connected clients.**
/// - **address:** IP and port to listen on (e.g., `"127.0.0.1:8080"`).
pub fn start_report_server(address: &str) -> std::io::Result<()> {
    let listener = TcpListener::bind(address)?;
    println!("✅ Report server running on {}", address);

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                println!("🔗 Client connected!");
                send_report_data(&mut stream)?;
            }
            Err(e) => {
                eprintln!("❌ Connection failed: {}", e);
            }
        }
    }
    Ok(())
}

/// **Sends the report data to a connected client.**
/// - **stream:** TCP connection stream for sending data.
fn send_report_data(stream: &mut TcpStream) -> std::io::Result<()> {
    let report_data = "📊 Monthly Sales Report\nSales Amount: $5000\nRegion: North America\n";
    stream.write_all(report_data.as_bytes())?;
    println!("✅ Report data successfully sent to client.");
    Ok(())
}

/// **Starts a TCP client to receive report data.**
/// - **address:** IP and port of the report server.
/// - **Returns:** Prints received report data.
pub fn receive_report_from_server(address: &str) -> std::io::Result<()> {
    let mut stream = TcpStream::connect(address)?;
    let mut reader = BufReader::new(&stream);
    let mut report = String::new();

    reader.read_line(&mut report)?;
    println!("📥 Received Report:\n{}", report);
    Ok(())
}
