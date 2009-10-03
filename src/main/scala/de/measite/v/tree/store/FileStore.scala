package de.measite.v.tree.store

import java.io.RandomAccessFile

import de.measite.v.tree.RTreeStoredNode

case class FileStore(val file: String) {

	val MAGIC_BYTES = "vDBv001\n"

	val raf = new RandomAccessFile(file, "rws");

	{
		val magic = new StringBuilder(MAGIC_BYTES.length + 1);
		for (i <- 0 until MAGIC_BYTES.length) {
			magic.append(raf.readChar)
		}
		if (!(magic.toString == MAGIC_BYTES)) {
			throw new IllegalStateException("Invalid store.");
		}
		val rootID = raf.readLong
		()
	}

	def load(position: Long) : Any = {
		raf.seek(position)

		// RIFF
		// (id)      : dword     =  long
		// (len)     : dword     =  long
		// (fourcc)  : byte[4]   => String
		// (payload) : byte[len]
		//
		// We try to stay RIFF-like.
		// # id      <- the index of the previous block
		// # len     <- length 'till next block
		// # fourcc  <- 4 magic bytes (content type)
		// # payload <- (#bytes + payload[#bytes] + whitespace)

		val pre = raf.readLong
		val len = raf.readLong
		val fourcc = {
			val sb = new StringBuilder(5)
			for (i <- 0 until 4) sb.append(
				(raf.readByte & 0xFF).asInstanceOf[char]
			)
			sb.toString
		}
		val pay  = raf.readInt
		val load = new Array[byte](pay)
		raf.readFully(load)

		// we've got the "raw" data, load the payload

		fourcc match {
			case "DATA" => {
				// DATA node == serialized Object
			}
			case "NODE" => {
				// NODE == RTreeNode
			}
		}

		()
	}

	def serialize(node: RTreeStoredNode) : Unit = {
		()
	}

}
