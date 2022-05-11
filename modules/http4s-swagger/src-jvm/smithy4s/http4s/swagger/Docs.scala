/*
 *  Copyright 2021 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s
package http4s
package swagger

import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.webjars.WebJarAssetLocator

private[smithy4s] abstract class Docs[F[_]](
    hasId: HasId,
    path: String,
    swaggerUiPath: String,
    swaggerUIInitJs: String
)(implicit F: Sync[F])
    extends Http4sDsl[F]
    with Compat.DocsClass[F] {

  val jsonSpec = hasId.id.namespace + '.' + hasId.id.name + ".json"

  val actualPath: Path = Uri.Path.unsafeFromString("/" + path)

  object DocPath {
    def unapply(p: Path): Boolean = {
      p match {
        case `actualPath`                                               => true
        case `actualPath` / ""                                          => true
        case `actualPath` / file if file.equalsIgnoreCase("index.html") => true
        case _                                                          => false
      }
    }
  }

  def fun: F[fs2.Stream[F, Byte]] = F.delay {

    def petStoreConfig(line: String) =
      line.contains("url: \"https://petstore.swagger.io/v2/swagger.json\"")

    val read = fs2.io
      .readClassLoaderResource(s"$swaggerUiPath/$swaggerUIInitJs")
      .through(fs2.text.utf8.decode)

    val transform =
      read
        .through(fs2.text.lines)
        .map(str =>
          if (
            str.contains("url: \"https://petstore.swagger.io/v2/swagger.json\"")
          )
            str.replace(
              "https://petstore.swagger.io/v2/swagger.json",
              "/" + jsonSpec
            )
          else str
        )
        .through(fs2.text.utf8.encode)

    read
      .forall(!petStoreConfig(_))
      .flatMap(notExists =>
        if (notExists)
          fs2.Stream.raiseError(
            new Exception("the swagger ui file format has changed bruv")
          )
        else transform
      )

  }

  def routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case r @ GET -> DocPath() if r.uri.query.isEmpty =>
      PermanentRedirect(
        Location(Uri.unsafeFromString(s"/$path/index.html"))
      )

    case request @ GET -> `actualPath` / filePath =>
      val resource = s"$swaggerUiPath/$filePath"
      staticResource(resource, Some(request)).getOrElseF(NotFound())

    case request @ GET -> Root / `jsonSpec` =>
      staticResource(jsonSpec, Some(request))
        .getOrElseF(InternalServerError())

    case _ @GET -> Root / `swaggerUIInitJs` =>
      F.map(fun)(entityBody =>
        Response(
          body = entityBody
        )
      )

  }
}

object Docs extends Compat.DocsCompanion {}

trait SwaggerUiInit {
  private[this] lazy val swaggerUiVersion: String =
    new WebJarAssetLocator().getWebJars.get("swagger-ui")

  protected lazy val swaggerUiPath =
    s"META-INF/resources/webjars/swagger-ui/$swaggerUiVersion"

  protected lazy val swaggerUIInitJs = "swagger-initializer.js"

}
