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

import aws.protocols.AwsJson1_0
import aws.protocols.AwsJson1_1
import smithy4s.Hints

private[aws] sealed trait AwsProtocol extends Product with Serializable {}

private[aws] object AwsProtocol {

  def apply(hints: Hints): Option[AwsProtocol] =
    hints
      .get(AwsJson1_0)
      .map(AWS_JSON_1_0.apply)
      .orElse(
        hints
          .get(AwsJson1_1)
          .map(AWS_JSON_1_1.apply)
      )

  // See https://awslabs.github.io/smithy/1.0/spec/aws/aws-json-1_0-protocol.html#differences-between-awsjson1-0-and-awsjson1-1
  final case class AWS_JSON_1_0(value: AwsJson1_0) extends AwsProtocol
  final case class AWS_JSON_1_1(value: AwsJson1_1) extends AwsProtocol

}
