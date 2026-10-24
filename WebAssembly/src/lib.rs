use spin_sdk::http::{IntoResponse, Request, Response};
use spin_sdk::http_component;

mod models;
mod repository;
mod service;
mod handlers;
mod traits;
#[cfg(test)]
mod tests;

use repository::Repository;
use service::Service;
use handlers::Handlers;
use traits::RepositoryInterface; // Import trait to use methods

#[http_component]
fn handle_magnetar_webassembly(req: Request) -> anyhow::Result<impl IntoResponse> {
    // Initialize layers
    let repo = Repository::new("default");

    // Ensure DB is initialized
    let _ = repo.initialize();

    let service = Service::new(repo);
    let handlers = Handlers::new(service);

    handlers.handle_request(req)
}
