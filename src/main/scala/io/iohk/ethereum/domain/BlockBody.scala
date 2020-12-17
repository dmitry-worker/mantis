package io.iohk.ethereum.domain

import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

import scala.collection.immutable


class BlockBody private(private val transactions: Array[SignedTransaction], val uncleNodesList: Seq[BlockHeader]) {

  lazy val numberOfTxs: Int = transactions.length

  lazy val numberOfUncles: Int = uncleNodesList.size

  def toShortString: String =
    s"BlockBody { transactionsList: ${transactions.map(_.hashAsHexString)}, uncleNodesList: ${uncleNodesList.map(_.hashAsHexString)} }"

  /**
    * Works as `copy` for case classes
    * @param transactions - new transactions for the block
    *        Beware using array-based collections as an argument, because:
    *  {{{
    *  val ar1 = Array(1, 2, 3)
    *  val seq = ar1.toSeq
    *  val ar2 = seq.toArray
    *  ar2.update(0,0)
    *  ar1
    *  }}}
    *  would return (0, 2, 3), because ar1 === ar2
    * @return new BlockBody with same uncles but new `transactions`.
    */
  def withTransactions(transactions: Seq[SignedTransaction]): BlockBody = {
    new BlockBody(transactions.toArray, this.uncleNodesList)
  }

  /**
    * @param uncles - new uncles for the block
    * @return new BlockBody with same `transactions` but replaced uncles.
    */
  def withUncles(uncles: Seq[BlockHeader]): BlockBody = {
    BlockBody(this.transactions, uncles)
  }

  def transactionIterator: Iterator[SignedTransaction] = {
    transactions.iterator
  }

  /**
    * @return A WrappedArray instance, no memory copied
    */
  def transactionsAsIndexedSeq: immutable.IndexedSeq[SignedTransaction] = {
    transactions.toIndexedSeq
  }

  /**
    * @return A WrappedArray instance, no memory copied
    */
  def transactionsAsSeq: Seq[SignedTransaction] = {
    transactions.toSeq
  }

  /**
    * @param i - Index of transaction in this body.
    * Guaranteed to return `None` when negative or out of bounds.
    * @return Optional transaction at index i, zero-based
    */
  def getTransactionByIndex(i: Int): Option[SignedTransaction] = {
    transactions.lift(i)
  }

  /**
    * @return an iterator that allows traversing backwards
    * constant-time access is guaranteed by `Array`
    */
  def transactionReverseIterator: Iterator[SignedTransaction] = {
    transactions.reverseIterator
  }

  /**
    * @param p - predicate on transaction
    * @return first transaction on which `p` is `true` along with its index
    */
  def findTransactionWhere(p: SignedTransaction => Boolean): Option[(Int, SignedTransaction)] = {
    transactions.indexWhere(p) match {
      case -1 => None
      case i  => Some((i -> transactions(i)))
    }
  }

  /**
    * @return an iterator of Tuple2[SignedTransaction, Int]
    *         Serves as a faster alternative to `List[_].zipWithIndex().iterator()`
    */
  def transactionEnumerator: Iterator[(SignedTransaction, Int)] = {
    transactionIterator.zipWithIndex
  }


  override def toString: String =
    s"BlockBody{ transactionList: ${transactions.toSeq}, uncleNodesList: $uncleNodesList }"


  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[BlockBody]
  }

  override def equals(other: Any): Boolean = other match {
    case that: BlockBody =>
      (that canEqual this) &&
        (transactions sameElements that.transactions) &&
        uncleNodesList == that.uncleNodesList
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(transactions.toSeq, uncleNodesList)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}

object BlockBody {

  val empty = BlockBody(Seq.empty, Seq.empty)

  def apply(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[BlockHeader]): BlockBody = {
    new BlockBody(transactionList.toArray, uncleNodesList)
  }

  def blockBodyToRlpEncodable(
      blockBody: BlockBody,
      signedTxToRlpEncodable: SignedTransaction => RLPEncodeable,
      blockHeaderToRlpEncodable: BlockHeader => RLPEncodeable
  ): RLPEncodeable =
    RLPList(
      RLPList(blockBody.transactions.map(signedTxToRlpEncodable): _*),
      RLPList(blockBody.uncleNodesList.map(blockHeaderToRlpEncodable): _*)
    )

  implicit class BlockBodyEnc(msg: BlockBody) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      blockBodyToRlpEncodable(
        msg,
        stx => SignedTransactionEnc(stx).toRLPEncodable,
        header => BlockHeaderEnc(header).toRLPEncodable
      )
    }
  }

  implicit class BlockBlodyDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockBody: BlockBody = BlockBodyRLPEncodableDec(rawDecode(bytes)).toBlockBody
  }

  def rlpEncodableToBlockBody(
      rlpEncodeable: RLPEncodeable,
      rlpEncodableToSignedTransaction: RLPEncodeable => SignedTransaction,
      rlpEncodableToBlockHeader: RLPEncodeable => BlockHeader
  ): BlockBody =
    rlpEncodeable match {
      case RLPList((transactions: RLPList), (uncles: RLPList)) =>
        BlockBody(
          transactions.items.map(rlpEncodableToSignedTransaction),
          uncles.items.map(rlpEncodableToBlockHeader)
        )
      case _ => throw new RuntimeException("Cannot decode BlockBody")
    }

  implicit class BlockBodyRLPEncodableDec(val rlpEncodeable: RLPEncodeable) {
    def toBlockBody: BlockBody = {
      import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

      rlpEncodableToBlockBody(
        rlpEncodeable,
        rlp => SignedTransactionRlpEncodableDec(rlp).toSignedTransaction,
        rlp => BlockHeaderDec(rlp).toBlockHeader
      )

    }
  }

}
