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

package smithy4s.http4s.swagger

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.typelevel.ci.CIString
import weaver._

object DocsSpec extends SimpleIOSuite with TestCompat {

  test(
    "GET /incorrect-format-swaggerinit.js returns an error because the init file has an incorrect format"
  ) {

    val initFile = "incorrect-format-swaggerinit.js"
    val app = docs("docs", "swaggerui", initFile).routes.orNotFound

    val request =
      Request[IO](
        method = Method.GET,
        uri = Uri.unsafeFromString("/" + initFile)
      )

    for {
      response <- app.run(request)
      body <- response.body.compile.toList
        .map(_.toArray)
        .map(new String(_))
        .attempt
    } yield {
      expect(response.status == Status.Ok) and
        expect(body.isLeft) and expect(
          body.left.exists(
            _.getMessage() == s"Unexcepted file format for file: $initFile"
          )
        )
    }
  }

  test(
    "GET /missing-swaggerinit.js returns an error because the file does not exist and an swagger init file is expected"
  ) {

    val initFile = "missing-swaggerinit.js"
    val app = docs("docs", "swaggerui", initFile).routes.orNotFound

    val request =
      Request[IO](
        method = Method.GET,
        uri = Uri.unsafeFromString("/" + initFile)
      )

    for {
      response <- app.run(request)
      body <- response.body.compile.toList
        .map(_.toArray)
        .map(new String(_))
        .attempt
    } yield {
      expect(response.status == Status.Ok) and
        expect(body.isLeft) and expect(
          body.left.exists(
            _.getMessage() == "Resource swaggerui/missing-swaggerinit.js not found"
          )
        )
    }

  }

  // ensure we can fetch static resources
  List("docs", "example/docs", "very/long/example/docs").foreach { path =>
    val app = docs(path, "swaggerui", "").routes.orNotFound

    test(s"GET $path/test-file.json fetches requested file") {
      val filePath = s"/$path/test-file.json"
      val request =
        Request[IO](method = Method.GET, uri = Uri.unsafeFromString(filePath))
      app.run(request).map { response =>
        expect(response.status == Status.Ok)
      }
    }
  }

  // These tests use the swagger-ui resource
  List("docs", "example/docs", "very/long/example/docs").foreach { path =>
    val app = docs(path).routes.orNotFound

    test(s"GET /swagger-initializer.js replaces url with spec file location") {
      val request =
        Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString("/swagger-initializer.js")
        )

      for {
        response <- app.run(request)
        body <- response.body.compile.toList.map(_.toArray).map(new String(_))
      } yield {
        expect(response.status == Status.Ok) and
          expect(
            response.headers.headers
              .find(_.name == CIString("content-type"))
              .exists(_.value == "text/javascript")
          )
        expect(
          body.contains("url: \"/foobar.test-spec.json\"")
        )
      }

    }

    test(s"GET /$path redirects to expected location") {
      val request =
        Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(s"/$path/index.html")
        )
      app.run(request).map { response =>
        val redirectUri = response.headers
          .get(CIString("Location"))
          .map(_.head)
          .map(_.value)

        expect(response.status == Status.PermanentRedirect) and
          expect.eql(
            redirectUri,
            Some(s"/$path/index.html")
          )
      }
    }
    test(s"GET /$path/ redirects to expected location") {
      val request =
        Request[IO](method = Method.GET, uri = Uri.unsafeFromString(s"/$path"))
      app.run(request).map { response =>
        val redirectUri = response.headers
          .get(CIString("Location"))
          .map(_.head)
          .map(_.value)

        expect(response.status == Status.PermanentRedirect) and
          expect.eql(
            redirectUri,
            Some(s"/$path/index.html")
          )
      }
    }
    test(s"GET /$path/index.html redirects to expected location") {
      val request =
        Request[IO](method = Method.GET, uri = Uri.unsafeFromString(s"/$path"))
      app.run(request).map { response =>
        val redirectUri = response.headers
          .get(CIString("Location"))
          .map(_.head)
          .map(_.value)

        expect(response.status == Status.PermanentRedirect) and
          expect.eql(
            redirectUri,
            Some(s"/$path/index.html")
          )
      }
    }
    test(
      s"GET /$path/index.html?url=/test-file.json does not redirect"
    ) {
      val request =
        Request[IO](
          method = Method.GET,
          uri = Uri.unsafeFromString(
            s"/$path/index.html?url=/test-file.json"
          )
        )
      app.run(request).map { response =>
        expect(response.status == Status.Ok)
      }
    }
  }

  test("GET /test-spec.json fetches service spec") {
    val filePath = "/foobar.test-spec.json"
    val request =
      Request[IO](method = Method.GET, uri = Uri.unsafeFromString(filePath))
    val app = docs("docs").routes.orNotFound
    app.run(request).map { response =>
      expect(response.status == Status.Ok)
    }
  }

  test(s"GET /irrelevant returns 404") {
    val filePath = s"/irrelevant"
    val request =
      Request[IO](method = Method.GET, uri = Uri.unsafeFromString(filePath))
    val app = docs("docs").routes.orNotFound
    app.run(request).map { response =>
      expect(response.status == Status.NotFound)
    }
  }
}
