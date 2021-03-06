package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.utils.Config
import monix.eval.Task

object Web3Service {
  case class Sha3Request(data: ByteString)
  case class Sha3Response(data: ByteString)

  case class ClientVersionRequest()
  case class ClientVersionResponse(value: String)
}

class Web3Service {
  import Web3Service._

  def sha3(req: Sha3Request): ServiceResponse[Sha3Response] = {
    Task(Right(Sha3Response(crypto.kec256(req.data))))
  }

  def clientVersion(req: ClientVersionRequest): ServiceResponse[ClientVersionResponse] = {
    Task(Right(ClientVersionResponse(Config.clientVersion)))
  }
}
