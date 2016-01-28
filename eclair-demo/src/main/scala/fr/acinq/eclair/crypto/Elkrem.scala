package fr.acinq.eclair.crypto

import fr.acinq.bitcoin.{Crypto, BinaryData}

/**
  * see https://github.com/LightningNetwork/lnd/blob/master/elkrem/elkrem.go
  */
object Elkrem {
  def hashLeft(value: BinaryData): BinaryData = Crypto.hash256(value)

  def hashRight(value: BinaryData): BinaryData = Crypto.hash256(value :+ 1.toByte)

  def hash(value: BinaryData, isRightChild: Boolean): BinaryData = {
    if (isRightChild) hashRight(value) else hashLeft(value)
  }

  def parentIndex(pos: Int) = (pos - 1) / 2

  def childrenIndex(pos: Int) = (2 * pos + 1, 2 * pos + 2)

  def isRightChild(pos: Int) = (pos % 2) == 0

  case class Sender(root: BinaryData, curpos: Int)

  object Sender {
    def init(root: BinaryData) = Sender(root, 0)

    def getHash(sender: Sender, index: Int): BinaryData = {
      if (index == 0) sender.root else hash(getHash(sender, parentIndex(index)), isRightChild(index))
    }
  }

  case class Receiver(nodes: Map[Int, BinaryData])

  object Receiver {
    def init = Receiver(Map.empty[Int, BinaryData])

    def addHash(receiver: Receiver, hash: BinaryData, index: Int): Receiver = {
      val (l, r) = childrenIndex(index)
      val map = receiver.nodes + (index -> hash)
      val map1 = map -(l, r)
      Receiver(map1)
    }

    def getHash(receiver: Receiver, index: Int): Option[BinaryData] =
      if (index == 0)
        None
      else {
        receiver.nodes.get(index) match {
          case Some(value) => Some(value)
          case None => getHash(receiver, parentIndex(index)).map(h => hash(h, isRightChild(index)))
        }
      }
  }
}
