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

package smithy4s.aws

import cats.effect.Temporal
import cats.effect.Resource
import cats.implicits._

trait AwsEnvironment[F[_]] {
  def credentials: F[AwsCredentials]
  def region: F[AwsRegion]
  def timestamp: F[Timestamp]
  def httpClient: SimpleHttpClient[F]
}

object AwsEnvironment {

  def default[F[_]](
      client: SimpleHttpClient[F],
      region: AwsRegion
  )(implicit F: Temporal[F]): Resource[F, AwsEnvironment[F]] =
    AwsCredentialsProvider.default[F](client).map { credentialsF =>
      make(
        client,
        F.pure(region),
        credentialsF,
        // note: fromEpochMilli would be nice
        F.realTime.map(_.toSeconds).map(Timestamp.fromEpochSecond(_))
      )
    }

  def make[F[_]](
      client: SimpleHttpClient[F],
      awsRegion: F[AwsRegion],
      creds: F[AwsCredentials],
      time: F[Timestamp]
  ): AwsEnvironment[F] = new AwsEnvironment[F] {
    def credentials: F[AwsCredentials] = creds

    def region: F[AwsRegion] = awsRegion

    def timestamp: F[Timestamp] = time

    def httpClient: SimpleHttpClient[F] = client
  }

}
