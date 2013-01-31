package controllers.base

import play.api.mvc._
import play.api.data.Form
import play.api.libs.concurrent.Execution.Implicits._
import defines.ContentType
import models.base.{Formable, Persistable, AccessibleEntity}
import defines.PermissionType
import models.{Entity, UserProfile}
import models.forms.VisibilityForm
import rest.EntityDAO

/**
 * Controller trait for extending Entity classes which server as
 * context for the creation of DocumentaryUnits, i.e. Agent and
 * DocumentaryUnit itself.
 */
trait CreationContext[CF <: Persistable, T <: AccessibleEntity] extends EntityRead[T] {

  def childCreateAction(id: String, ct: ContentType.Value)(f: Entity => Seq[(String,String)] => Seq[(String,String)] => UserProfile => Request[AnyContent] => Result) = {
    withItemPermission(id, PermissionType.Create, contentType, Some(ct)) { item => implicit user =>
      implicit request =>
      getGroups(Some(user)) { users => groups =>
        f(item)(users)(groups)(user)(request)
      }
    }
  }

  def childCreatePostAction[CT<:Persistable](id: String, form: Form[CT], ct: ContentType.Value)(
        f: Entity => Either[(Form[CT],Form[List[String]]),Entity] => UserProfile => Request[AnyContent] => Result) = {
    withItemPermission(id, PermissionType.Create, contentType, Some(ct)) { item => implicit user =>
      implicit request =>
        implicit val maybeUser = Some(user)
        val accessorForm = VisibilityForm.form
          .bindFromRequest(fixMultiSelects(request.body.asFormUrlEncoded, rest.RestPageParams.ACCESSOR_PARAM))
        val accessors = accessorForm.value.getOrElse(List())
        form.bindFromRequest.fold(
          errorForm => f(item)(Left((errorForm,accessorForm)))(user)(request),
          citem => {
            AsyncRest {
              rest.EntityDAO(entityType, maybeUser)
                .createInContext(id, ct, citem, accessors).map { itemOrErr =>
              // If we have an error, check if it's a validation error.
              // If so, we need to merge those errors back into the form
              // and redisplay it...
                if (itemOrErr.isLeft) {
                  itemOrErr.left.get match {
                    case err: rest.ValidationError => {
                      val serverErrors = citem.errorsToForm(err.errorSet)
                      val filledForm = form.fill(citem).copy(errors = form.errors ++ serverErrors)
                      Right(f(item)(Left((filledForm,accessorForm)))(user)(request))
                    }
                    case e => Left(e)
                  }
                } else itemOrErr.right.map {
                  citem => f(item)(Right(citem))(user)(request)
                }
              }
            }
          }
        )
    }
  }
}
