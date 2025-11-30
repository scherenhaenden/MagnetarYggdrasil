import Vapor

struct HealthController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        routes.get("health", use: health)
    }

    func health(req: Request) async throws -> HTTPStatus {
        return .ok
    }
}
