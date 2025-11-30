import jester
import std/json
import ../utils/response_utils

proc healthCheck*(): ResponseData =
  return jsonResponse(Http200, %*{"status": "ok"})
