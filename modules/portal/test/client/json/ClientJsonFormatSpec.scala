package client.json

import models.{UserProfileF, UserProfile}
import models.base.Accessor
import play.api.libs.json.Json
import play.api.test.{FakeRequest, PlaySpecification}

class ClientJsonFormatSpec extends PlaySpecification {
  // Needed for client JSON serialization, since output
  // contains URLs with context-dependent hostname etc
  implicit val fr = FakeRequest(GET, "/")
  val user = UserProfile(UserProfileF(id = Some("bob"), identifier = "bob", name = "Bob"))

  "client JSON conversion" should {
    // test Accessor and AnyModel conversions, since these unfortunately
    // contain type casts
    "work for accessor models" in {
      val json = Json.toJson(user)(client.json.accessorJson.clientWrites)
      json must_== Json.toJson(user)(client.json.userProfileJson.clientWrites)
    }

    "work for any model" in {
      val json = Json.toJson(user)(client.json.anyModelJson.clientWrites)
      json must_== Json.toJson(user)(client.json.userProfileJson.clientWrites)
    }
  }
}
