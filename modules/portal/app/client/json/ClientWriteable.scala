package client.json

import play.api.libs.json.Writes
import play.api.mvc.RequestHeader

trait ClientWriteable[T] {
  def clientWrites(implicit requestHeader: RequestHeader): Writes[T]
}
