package cml.serializers

import cml._
import com.esotericsoftware.kryo.io.{Output, Input}
import com.esotericsoftware.kryo.{Kryo, Serializer}

class TreeSerializer extends Serializer[Tree[_, _]] {
  var accumClass: Class[_] = classOf[Any]
  var valueClass: Class[_] = classOf[Any]

  override def write(kryo: Kryo, output: Output, `object`: Tree[_, _]): Unit = {
    val sa = kryo.getSerializer(accumClass)
    val sv = kryo.getSerializer(valueClass)

    def writeTree(t: Tree[_, _]): Unit =
      t match {
        case Leaf(a, v) => {
          output.writeBoolean(true)
          kryo.writeObject(output, a, sa)
          kryo.writeObject(output, v, sv)
        }
        case Node(l, a, r) => {
          output.writeBoolean(false)
          writeTree(l)
          kryo.writeObject(output, a, sa)
          writeTree(r)
        }
      }

    writeTree(`object`)
  }

  override def read(kryo: Kryo, input: Input, `type`: Class[Tree[_, _]]): Tree[_, _] = {
    val sa = kryo.getSerializer(accumClass)
    val sv = kryo.getSerializer(valueClass)

    def readTree(): Tree[_, _] = {
      if (input.readBoolean()) {
        val a = kryo.readObject(input, accumClass, sa)
        val v = kryo.readObject(input, valueClass, sv)
        Leaf(a, v)
      } else {
        val l = readTree()
        val a = kryo.readObject(input, accumClass, sa)
        val r = readTree()
        Node(l, a, r)
      }
    }

    readTree()
  }

  override def setGenerics(kryo: Kryo, generics: Array[Class[_]]): Unit = {
    accumClass = generics(0)
    valueClass = generics(1)
  }
}
