package io.iohk.ethereum.consensus.ethash

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.domain.{Address, Block}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

class EthashBlockCreator(
    val pendingTransactionsManager: ActorRef,
    val getTransactionFromPoolTimeout: FiniteDuration,
    consensus: EthashConsensus,
    ommersPool: ActorRef
) extends TransactionPicker {

  lazy val fullConsensusConfig = consensus.config
  private lazy val consensusConfig = fullConsensusConfig.generic
  lazy val miningConfig = fullConsensusConfig.specific
  private lazy val coinbase: Address = consensusConfig.coinbase
  private lazy val blockGenerator: EthashBlockGenerator = consensus.blockGenerator
  lazy val blockchainConfig = consensus.blockchainConfig

  def getBlockForMining(
      parentBlock: Block,
      withTransactions: Boolean = true,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy] = None
  ): Future[PendingBlockAndState] = {
    val transactions =
      if (withTransactions) getTransactionsFromPool else Future.successful(PendingTransactionsResponse(Nil))
    getOmmersFromPool(parentBlock.hash).zip(transactions).map { case (ommers, pendingTxs) =>
      blockGenerator.generateBlock(
        parentBlock,
        pendingTxs.pendingTransactions.map(_.stx.tx),
        coinbase,
        ommers.headers,
        initialWorldStateBeforeExecution
      )
    }
  }

  private def getOmmersFromPool(parentBlockHash: ByteString): Future[OmmersPool.Ommers] = {
    (ommersPool ? OmmersPool.GetOmmers(parentBlockHash))(Timeout(miningConfig.ommerPoolQueryTimeout))
      .mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error("Failed to get ommers, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

}
