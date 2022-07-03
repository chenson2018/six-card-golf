use actix_web::{App, HttpServer};
use actix_files::Files;

#[actix_web::main]
async fn main() -> std::io::Result<()> {

    const HOST: &str = if cfg!(windows) {
        "localhost"
    } else {
        "0.0.0.0"
    };

    const PORT: u16 = 8001;

    println!("Starting server at: {}:{}", HOST, PORT);

    HttpServer::new(|| {
        App::new()
            .service(Files::new("/", "./frontend/"))
    })
    .bind(format!("{}:{}", HOST, PORT))?
    .run()
    .await
}

