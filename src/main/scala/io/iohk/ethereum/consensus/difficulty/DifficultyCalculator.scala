package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.consensus.ethash.difficulty.{ConfiguredDifficultyCalculator, EthashDifficultyCalculator}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

trait DifficultyCalculator {
  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: BlockHeader): BigInt
}

object DifficultyCalculator {
  def apply(blockchainConfig: BlockchainConfig): DifficultyCalculator = {
    blockchainConfig.powTargetTime match {
      case Some(targetTime) => new ConfiguredDifficultyCalculator(targetTime)
      case None => new EthashDifficultyCalculator(blockchainConfig)
    }
  }
}
