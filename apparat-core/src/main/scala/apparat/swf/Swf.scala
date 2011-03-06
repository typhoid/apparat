/*
 * This file is part of Apparat.
 * 
 * Apparat is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Apparat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Apparat. If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2009 Joa Ebert
 * http://www.joa-ebert.com/
 * 
 */
package apparat.swf

import apparat.swc.Swc
import apparat.utils.IO._
import java.io.{
	BufferedInputStream => JBufferedInputStream,
	File => JFile,
	FileInputStream => JFileInputStream,
	FileOutputStream => JFileOutputStream,
	ByteArrayInputStream => JByteArrayInputStream,
	ByteArrayOutputStream => JByteArrayOutputStream,
	InputStream => JInputStream,
	OutputStream => JOutputStream
}
import java.util.zip.{Inflater => JInflater}
import scala.annotation.tailrec
import apparat.utils.{Dumpable, Deflate, IndentingPrintWriter}
import apparat.lzma.LZMA

object Swf {
	def fromFile(file: JFile): Swf = {
		val name = file.getName.toLowerCase

		if(name endsWith ".swc") {
			fromSwc(Swc fromFile file)
		} else if(name endsWith ".swf") {
			val swf = new Swf
			swf read file
			swf
		} else {
			using(new JFileInputStream(file)) {
				input => {
					val b0 = input.read()

					if(('F' == b0 || 'C' == b0 || 'Z' == b0) && 'W' == input.read() && 'S' == input.read()) {
						val swf = new Swf
						swf read file
						swf
					} else if ('P' == b0 && 'K' == input.read()) {
						fromSwc(Swc fromFile file)
					} else {
						error("Unknown file "+file.getAbsolutePath+".")
					}
				}
			}
		}
	}

	def fromFile(pathname: String): Swf = fromFile(new JFile(pathname))

	def fromSwc(swc: Swc) = {
		val swf = new Swf
		swf read swc
		swf
	}

	def fromInputStream(input: JInputStream, length: Long) = {
		val swf = new Swf
		swf.read(input, length)
		swf
	}
}

final class Swf extends Dumpable with SwfTagMapping {
	var compression: Option[SwfCompressionType] = Some(SwfZLibCompression)
	var version: Int = 10
	var frameSize: Rect = new Rect(0, 20000, 0, 20000)
	var frameRate: Float = 255.0f
	var frameCount: Int = 1
	var tags: List[SwfTag] = Nil

	def width = frameSize._2 / 20
	def height = frameSize._4 / 20

	def foreach(body: SwfTag => Unit) { tags foreach body }

	def read(file: JFile) { using(new JBufferedInputStream(new JFileInputStream(file), 0x1000))(read(_, file.length)) }

	def read(pathname: String) { read(new JFile(pathname)) }

	def read(input: JInputStream, inputLength: Long) { using(new SwfInputStream(input))(read(_, inputLength)) }

	def read(data: Array[Byte]) { using(new JByteArrayInputStream(data))(read(_, data.length)) }

	def read(swc: Swc) {
		swc.library match {
			case Some(data) => read(data)
			case None =>
		}
	}

	def read(input: SwfInputStream, inputLength: Long) {
		(input.readUI08(), input.readUI08(), input.readUI08()) match {
			case (x, 'W', 'S') =>
				compression = x match {
					case 'C' => Some(SwfZLibCompression)
					case 'Z' => Some(SwfLZMACompression)
					case 'F' => None
					case _ => error("Not a SWF file.")
				}
			case _ => error("Not a SWF file.")
		}

		version = input.readUI08()

		val uncompressedLength = input.readUI32()
		val uncompressed = compression match {
			case Some(SwfZLibCompression) =>
				assert(version > 5)
				uncompress(inputLength, uncompressedLength)(input)
			case Some(SwfLZMACompression) =>
				assert(version > 12)
				error("TODO: lzma decode")
			case None => input
		}

		try {
			frameSize = uncompressed.readRECT()
			frameRate = uncompressed.readFIXED8()
			frameCount = uncompressed.readUI16()

			// Issue 34, since 10.1
			// assert(frameSize.minX == 0 && frameSize.minY == 0, "Frame size: "+frameSize.minX+", "+frameSize.minY)
			assert(frameRate >= 0)
			assert(frameCount > 0)

			tags = tagsOf(uncompressed)
		} finally {
			if(compression.isDefined) {
				try {
					uncompressed.close()
				} catch {
					case _ =>
				}
			}
		}
	}

	private def tagsOf(implicit input: SwfInputStream): List[SwfTag] = {
		@tailrec def loop(tag: SwfTag, acc: List[SwfTag]): List[SwfTag] = {
			val result = tag :: acc
			if(tag.kind == SwfTags.End) result else loop(input.readTAG(), result)
		}

		loop(input.readTAG(), List.empty).reverse
	}

	def write(file: JFile) { using(new JFileOutputStream(file))(write _) }

	def write(pathname: String) { write(new JFile(pathname)) }

	def write(output: JOutputStream) { using(new SwfOutputStream(output))(write _) }

	def write(swc: Swc) {
		val byteArrayOutputStream = new JByteArrayOutputStream()

		try {
			write(byteArrayOutputStream)
			swc.library = Some(byteArrayOutputStream.toByteArray)
		} finally {
			try {
				byteArrayOutputStream.close()
			} catch {
				case _ =>
			}
		}
	}

	def write(output: SwfOutputStream) {
		compression match {
			case Some(SwfZLibCompression) =>
				if(version < 6) error("SWF version 6 is required for Zlib compression.")
			case Some(SwfLZMACompression) =>
				if(version < 13) error("SWF version 13 is required for LZMA compression.")
			case None => 
		}

		val byteArrayOutputStream = new JByteArrayOutputStream(0x08 + (tags.length << 0x03))
		val buffer = new SwfOutputStream(byteArrayOutputStream)

		try {
			buffer.writeRECT(frameSize)
			buffer.writeFIXED8(frameRate)
			buffer.writeUI16(frameCount)
			buffer.flush()

			tags foreach { buffer writeTAG _ }
			buffer.flush()

			val bytes = byteArrayOutputStream.toByteArray

			buffer.close()

			output.write(Array[Byte](
				compression match {
					case Some(SwfZLibCompression) => 'C'
					case Some(SwfLZMACompression) => 'Z'
					case None => 'F'
				}, 'W', 'S'))
			output.writeUI08(version)
			output.writeUI32(8 + bytes.length)

			compression match {
				case Some(SwfZLibCompression) => Deflate.compress(bytes, output)
				case Some(SwfLZMACompression) =>
					val lzmaBuffer = new JByteArrayOutputStream()
					LZMA.encode(new JByteArrayInputStream(bytes), bytes.length, lzmaBuffer)
					lzmaBuffer.flush()
					val lzmaCompressed = lzmaBuffer.toByteArray
					output.writeUI32(lzmaCompressed.length - 5)//compressed length sans props (5b)
					output.write(lzmaBuffer)
				case None => output write bytes
			}

			output.flush()
		} finally {
			if (null != buffer) {
				try {
					buffer.close()
				} catch {
					case _ =>
				}
			}
		}
	}

	def uncompress(inputLength: Long, uncompressedLength: Long)(implicit input: JInputStream) = {
		val totalBytes = (inputLength - 8).asInstanceOf[Int]//magic 8 is static part of header length
		val inflater = new JInflater()
		val bufferIn = new Array[Byte](totalBytes)
		val bufferOut = new Array[Byte]((uncompressedLength - 8).asInstanceOf[Int])

		readBytes(totalBytes, bufferIn)

		inflater setInput (bufferIn)

		var offset = -1
		while (0 != offset && !inflater.finished()) {
			offset = inflater inflate bufferOut
			if (0 == offset && inflater.needsInput) {
				error("Need more input.")
			}
		}

		new SwfInputStream(new JByteArrayInputStream(bufferOut))
	}

	def toByteArray = {
		val byteArrayOutputStream = new JByteArrayOutputStream()
		using(byteArrayOutputStream) { write _ }
		byteArrayOutputStream.toByteArray
	}

	def toLZMAByteArray = {
		val oldCompression = compression

		try {
			compression = None

			val swfByteArrayOutputStream = new JByteArrayOutputStream()
			val lzmaByteArrayOutputStream = new JByteArrayOutputStream()

			using(swfByteArrayOutputStream) { write _ }

			val byteArray = swfByteArrayOutputStream.toByteArray

			LZMA.encode(new JByteArrayInputStream(byteArray), byteArray.length, lzmaByteArrayOutputStream)
			lzmaByteArrayOutputStream.toByteArray
		} finally {
			compression = oldCompression
		}
	}

	override def dump(writer: IndentingPrintWriter) {
		writer <= "Swf:"
		writer withIndent {
			writer <= "Compression: "+compression.toString
			writer <= "Version: "+version
			writer <= "Framesize:"+frameSize
			writer <= "Framerate:"+frameRate
			writer <= "Framecount:"+frameCount
			writer <= "Tags:"
			writer withIndent {
				for(tag <- tags) tag match {
					case dumpable: Dumpable => dumpable dump writer
					case other => writer <= other.toString
				}
			}
		}
	}

	lazy val mainClass = tags find { _.kind == SwfTags.SymbolClass } match {
		case Some(symbolClass: SymbolClass) => symbolClass.symbols find { _._1 == 0 } map { _._2 }
		case _=> None
	}

	lazy val backgroundColor = tags find { _.kind == SwfTags.SetBackgroundColor } match {
		case Some(setBackgroundColor: SetBackgroundColor) => Some(setBackgroundColor.color)
		case _ => None
	}
}
