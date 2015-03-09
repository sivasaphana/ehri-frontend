package controllers.base


import jp.t2v.lab.play2.auth._
import play.api.mvc.{Cookie, RequestHeader, Result}

class RememberMeTokenAccessor(maxAge: Int) extends CookieTokenAccessor() {
  override def put(token: AuthenticityToken)(result: Result)(implicit request: RequestHeader): Result = {
    val remember = request.tags.get("rememberme").exists(_ == "true") || request.session.get("rememberme").exists (_ == "true")
    val _maxAge = if (remember) Some(maxAge) else None
    println("Remembering with maxage: " + _maxAge)
    val c = Cookie(cookieName, sign(token), _maxAge, cookiePathOption, cookieDomainOption, cookieSecureOption, cookieHttpOnlyOption)
    result.withCookies(c)
  }
}
