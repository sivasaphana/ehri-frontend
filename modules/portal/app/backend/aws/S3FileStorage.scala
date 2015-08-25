package backend.aws

import java.io.File
import java.net.URI
import javax.inject.Inject

import awscala.Credentials
import awscala.s3.{Bucket, PutObjectResult, S3}
import backend.FileStorage

import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
case class S3FileStorage @Inject ()(implicit config: play.api.Configuration) extends FileStorage {
  override def putFile(instance: String, classifier: String, path: String, file: File)(implicit executionContext:
  ExecutionContext): Future[URI] = {
    val awsConfig: AwsConfig = AwsConfig.fromConfig(fallback = Map("aws.instance" -> instance))
    implicit val s3 = S3(Credentials(awsConfig.accessKey, awsConfig.secret)).at(awscala.Region(awsConfig.region))
    val bucket: Bucket = s3.bucket(classifier).getOrElse(sys.error(s"Bucket $classifier not found"))
    val read: PutObjectResult = bucket.putAsPublicRead(path, file)
    Future.successful(new URI(s"https://${read.bucket.name}.s3-${awsConfig.region}.amazonaws.com/${read.key}"))
  }
}
