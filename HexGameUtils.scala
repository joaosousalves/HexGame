package Hex
import scala.io.StdIn.{readInt,readLine}
import scala.util.{Try, Success, Failure}
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

case class SaveGameState(savedGame: Boolean, userTurn: Cells.Cell, board: List[List[String]])

object HexGameUtils {

	//print game outputs
	def printMsgNl(msg: String): Unit= println(msg)
	def printMsg(msg: String): Unit= print(msg)

	//get users input
	def getUserInputSTR : String= readLine.trim()


	def getUserInputInt(msgError: String): Int={
		val intInput = Try(readInt())
		intInput match {
			case Success(num) => num
			case Failure(f) => 
				printMsgNl(msgError)
				getUserInputInt(msgError)
		}
	}

	def readSavedGameFromBinaryFile(filePath: String): Option[SaveGameState] = {
		val fileInputStream = new FileInputStream(filePath)
		val objectInputStream = new ObjectInputStream(fileInputStream)

		try {
			val data = objectInputStream.readObject()
			Some(data.asInstanceOf[SaveGameState])
		} catch {
			case _: Exception => None
		} finally {
			objectInputStream.close()
			fileInputStream.close()
		}
	}

	def saveGameStateToBinaryFile(saveGame: SaveGameState, fileName: String): Unit = {
		val fileOut = new FileOutputStream(fileName)
		val objectOut = new ObjectOutputStream(fileOut)
		try {
			objectOut.writeObject(saveGame)
		} finally {
			objectOut.close()
			fileOut.close()
		}
	}

	def getSavedGameState(filePath: String): SaveGameState={
		val savedGame = readSavedGameFromBinaryFile(filePath)
		savedGame match{
			case Some(data)=> data
			case None => SaveGameState(savedGame=false,userTurn=Cells.Empty, Nil )
		}
	}

}