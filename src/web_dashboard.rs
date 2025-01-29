//! This module provides a web-based API for accessing reports securely.
//! Now supports:
//! ✅ Secure API authentication using tokens
//! ✅ Live report data retrieval via HTTP requests
//! ✅ Error handling for unauthorized access

use actix_web::{web, App, HttpRequest, HttpResponse, HttpServer, Responder};

/// **Validates an authentication token for secure report access.**
/// - **req:** The HTTP request containing the token.
/// - **Returns:** `true` if the token is valid, `false` otherwise.
fn validate_token(req: &HttpRequest) -> bool {
    if let Some(auth_header) = req.headers().get("Authorization") {
        return auth_header.to_str().unwrap_or("") == "Bearer SECRET_TOKEN";
    }
    false
}

/// **Handles API requests for fetching live reports.**
/// - **req:** The HTTP request containing the authentication token.
/// - **Returns:** The report data if authenticated, or an error message.
async fn report_handler(req: HttpRequest) -> impl Responder {
    if validate_token(&req) {
        HttpResponse::Ok().body("📊 Live Sales Report\nSales Amount: $5000\nRegion: North America")
    } else {
        HttpResponse::Unauthorized().body("❌ Invalid Token")
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    println!("🌍 Web API running on http://127.0.0.1:8080/report");

    HttpServer::new(|| {
        App::new().route("/report", web::get().to(report_handler))
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
