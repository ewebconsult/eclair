package fr.acinq.eclair.crypto

import fr.acinq.bitcoin.BinaryData
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by fabrice on 28/01/16.
  */
@RunWith(classOf[JUnitRunner])
class ElkremSpec extends FunSuite {
  import Elkrem._

  test("sender can compute hashes") {
    val root: BinaryData = "this is the root".getBytes("UTF-8")
    val sender = Sender.init(root)
    assert(Sender.getHash(sender, 0) == root)
    assert(Sender.getHash(sender, 5) == hashLeft(hashRight(root)))
    assert(Sender.getHash(sender, 10) == hashRight(hashRight(hashLeft(root))))
  }
  test("receiver can compute hashes") {
    val root: BinaryData = "this is the root".getBytes("UTF-8")
    val sender = Sender.init(root)
    val receiver = Receiver.init

    val receiver1 = Receiver.addHash(receiver, Sender.getHash(sender, 14), 14)
    assert(receiver1.nodes(14) == Sender.getHash(sender, 14))
    val receiver2 = Receiver.addHash(receiver1, Sender.getHash(sender, 13), 13)
    assert(receiver2.nodes(13) == Sender.getHash(sender, 13))

    val receiver3 = Receiver.addHash(receiver2, Sender.getHash(sender, 6), 6)
    assert(receiver3.nodes(6) == Sender.getHash(sender, 6))
    assert(receiver3.nodes.get(13).isEmpty)
    assert(receiver3.nodes.get(14).isEmpty)
    assert(Receiver.getHash(receiver3, 13) == Some(Sender.getHash(sender, 13)))
    assert(Receiver.getHash(receiver3, 14) == Some(Sender.getHash(sender, 14)))

    val receiver4 = Receiver.addHash(receiver3, Sender.getHash(sender, 10), 10)
    assert(Receiver.getHash(receiver4, 10) == Some(Sender.getHash(sender, 10)))
    val receiver5 = Receiver.addHash(receiver4, Sender.getHash(sender, 1), 1)
    assert(Receiver.getHash(receiver5, 1) == Some(Sender.getHash(sender, 1)))
    assert(Receiver.getHash(receiver5, 10).isEmpty)
    assert(Receiver.getHash(receiver5, 4).isEmpty)
    assert(Receiver.getHash(receiver5, 10) == Some(Sender.getHash(sender, 10)))
  }
}
