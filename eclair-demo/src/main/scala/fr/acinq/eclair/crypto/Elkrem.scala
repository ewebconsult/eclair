package fr.acinq.eclair.crypto

import fr.acinq.bitcoin.{Crypto, BinaryData}

/**
  * see https://github.com/LightningNetwork/lnd/blob/master/elkrem/elkrem.go
  */
object Elkrem {

  /**
    * Sender state
    *
    * @param root   secret root
    * @param height tree height (3 means 8 leaves, 15 nodes total)
    * @param moves  current position i.e. moves down the tree to get the current hash.
    *               false means left, true means right.
    */
  case class Sender(root: BinaryData, height: Int, moves: Vector[Boolean]) {
    val numberOfHashes = (1 << height) - 1

    def hash = Sender.hash(root, moves)

    def hasNext = !moves.isEmpty
  }

  object Sender {
    /**
      *
      * @param root   secret hash root
      * @param height tree height
      * @return a Sender object that "points" at the first hash in the tree (i.e. the bottom-left leave)
      */
    def init(root: BinaryData, height: Int) = Sender(root, height, Vector.fill(height - 1)(false))

    def next(sender: Sender): Sender = sender.copy(moves = nextMoves(sender.moves, sender.height))

    def hash(root: BinaryData, moves: Seq[Boolean]): BinaryData = moves.foldLeft(root)(Elkrem.hash)

    def hash(root: BinaryData, pos: Int): BinaryData = hash(root, moves(pos))
  }

  /**
    * Receiver state
    *
    * @param height tree height
    * @param nodes  node map (index -> hash)
    * @param moves  current position
    */
  case class Receiver(height: Int, nodes: Map[Int, BinaryData], moves: Vector[Boolean]) {
    def getHash(index: Int): Option[BinaryData] = Receiver.getHash(this, index)
  }

  object Receiver {
    def init(height: Int) = Receiver(height, Map.empty[Int, BinaryData], Vector.fill(height - 1)(false))

    /**
      * add a new hash to the receiver
      *
      * @param receiver receiver stats
      * @param value    hash
      * @return an updated receiver state
      */
    def addHash(receiver: Receiver, value: BinaryData): Receiver = {
      val pos = index(receiver.moves)
      val nodes1 = receiver.nodes + (pos -> value) - leftChildIndex(pos) - rightChildIndex(pos)
      val moves1 = nextMoves(receiver.moves, receiver.height)
      receiver.copy(nodes = nodes1, moves = moves1)
    }

    /**
      *
      * @param receiver receiver state
      * @param pos      position of the hash in the tree
      * @return Some(hash) if the hash at this position can be computed, None otherwise
      */
    def getHash(receiver: Receiver, pos: Int): Option[BinaryData] = {
      if (pos == 0) None
      else {
        receiver.nodes.get(pos) match {
          case Some(value) => Some(value)
          case None => getHash(receiver, parentIndex(pos)).map(h => hash(h, isRightChild(pos)))
        }
      }
    }
  }

  def nextMoves(moves: Vector[Boolean], height: Int): Vector[Boolean] = {
    if (moves.isEmpty) moves
    else moves.last match {
      case true => moves.take(moves.size - 1)
      case false =>
        val moves1 = moves.updated(moves.size - 1, true)
        moves1 ++ Vector.fill(height - 1 - moves1.size)(false)
    }
  }

  def index(moves: Seq[Boolean]): Int = moves.foldLeft(0)(childIndex)

  def moves(pos: Int): Seq[Boolean] = if (pos == 0) Seq.empty[Boolean] else moves(parentIndex(pos)) :+ isRightChild(pos)

  def hashLeft(value: BinaryData): BinaryData = Crypto.hash256(value)

  def hashRight(value: BinaryData): BinaryData = Crypto.hash256(value :+ 1.toByte)

  def hash(value: BinaryData, isRightChild: Boolean): BinaryData = if (isRightChild) hashRight(value) else hashLeft(value)

  def parentIndex(pos: Int) = (pos - 1) / 2

  def childIndex(pos: Int, isRightChild: Boolean): Int = if (isRightChild) rightChildIndex(pos) else leftChildIndex(pos)

  def leftChildIndex(pos: Int) = 2 * pos + 1

  def rightChildIndex(pos: Int) = 2 * pos + 2

  def isRightChild(pos: Int) = (pos % 2) == 0
}
