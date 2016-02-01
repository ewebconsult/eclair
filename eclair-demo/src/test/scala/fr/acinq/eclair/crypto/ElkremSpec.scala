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
    val root = BinaryData("5a5f20f2b8501b9bc922e0763968fca05f276c298c101eaa7cb6c88ba02e0b3b").reverse
    val sender = Sender.init(root, 4)

    def hashes(sender: Sender, max: Int, acc: Seq[BinaryData] = Seq.empty[BinaryData]) : Seq[BinaryData] = {
      if (acc.size == max) acc else hashes(Sender.next(sender), max, acc :+ sender.hash)
    }

    val results = hashes(sender,15)
    val expected: Seq[BinaryData] = Seq(
      "337d823876c6db822bbe63492aef42f8aeafd963ebd8b2e4dd26f787ad70fb66",
      "1a4ea5614e7b59f5c9781f99e7654d89fefd5aac3c10f76405252cb911ca1bec",
      "403f1a1f328f6935bc2f5663b92582f48dd3a16e0ebe315a7de1c4c368620f6d",
      "44228228872eb996e40b37395b1b1cd4845f7c31263958f7f10ebf46f0114568",
      "9dba0c7e76e7c5c6b4680f4163429706f69b0e490f9f969b3cd53b736aaddb37",
      "b7459906d6ffaa584bb517455837fd0d8c0a30dba8ba9c424e3142376f96ce93",
      "d32d85d39660870e7e3911ac1dbb82295d66141b782d1d156ea8002ea800ac6c",
      "8e113c41eb6f12f705bdfb6b0e1e7a957873f318415fc434b9dcac90967bd067",
      "b87caafd83f0e5a8568de6535b9d11030218faf92eb1e13a92314ac042094e68",
      "4a0c0f7af16ac95eef46a3a76e62a34de1ba8576e38a3d7ecda4b296f13103cb",
      "dc2754c86920f8a79796593622f88eb0783a4660e5a8e557fb1c3fa868445dd1",
      "2119e994c7389617a0f3833e153e4dc26c19a317182614dc4d5ec4d8ee06e525",
      "ba76d25c8239f49be12f8cffca9d17403c8e6eaee5ef6d59b85870decdad2915",
      "24a08d0e279f89032ccbd799eb6c3b9f9d7387178ea2d5edbf3b74b608facf84",
      "5a5f20f2b8501b9bc922e0763968fca05f276c298c101eaa7cb6c88ba02e0b3b"
    )
    val expected1 = expected.map(_.reverse).map(BinaryData(_))
    assert(results === expected1)
  }
  test("receiver can compute hashes") {
    val root = BinaryData("5a5f20f2b8501b9bc922e0763968fca05f276c298c101eaa7cb6c88ba02e0b3b").reverse
    val sender = Sender.init(root, 4)
    val receiver = Receiver.init(4)

    val receiver1 = Receiver.addHash(receiver, sender.hash)
    val sender1 = Sender.next(sender)
    assert(receiver1.nodes(7) === Sender.hash(sender1.root, 7))
    assert(receiver1.getHash(7) === Some(Sender.hash(sender1.root, 7)))

    val receiver2 = Receiver.addHash(receiver1, sender1.hash)
    val sender2 = Sender.next(sender1)
    assert(receiver2.nodes(8) === Sender.hash(sender2.root, 8))
    assert(receiver2.getHash(7) === Some(Sender.hash(sender1.root, 7)))

    val receiver3 = Receiver.addHash(receiver2, sender2.hash)
    val sender3 = Sender.next(sender2)
    assert(receiver3.nodes(3) === Sender.hash(sender3.root, 3))
    assert(receiver3.nodes.get(7).isEmpty)
    assert(receiver3.nodes.get(8).isEmpty)
    assert(receiver3.getHash(3) === Some(Sender.hash(sender3.root, 3)))
    assert(receiver3.getHash(7) === Some(Sender.hash(sender3.root, 7)))
    assert(receiver3.getHash(8) === Some(Sender.hash(sender3.root, 8)))
  }
}
