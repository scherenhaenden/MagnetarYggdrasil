import jester
import std/json

# Return type for handlers
type ResponseData* = tuple[code: HttpCode, body: string]

proc jsonResponse*(code: HttpCode, body: JsonNode): ResponseData =
  return (code, $body)

proc jsonResponse*(code: HttpCode, body: auto): ResponseData =
  return jsonResponse(code, %*body)
