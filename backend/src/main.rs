use actix_web::{App, HttpServer};
use actix_files::Files;

const HOST: &str = "localhost";
const PORT: u16 = 8000;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    
    println!("Starting server at: {}:{}", HOST, PORT);

    HttpServer::new(|| {
        App::new()
            .service(Files::new("/", "../frontend/"))
    })
    .bind(format!("{}:{}", HOST, PORT))?
    .run()
    .await
}
