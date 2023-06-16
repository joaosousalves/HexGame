package Hex
import scala.io.AnsiColor.{BOLD, RED, BLUE, RESET}
import java.time.Instant
import scala.annotation.tailrec
import Cells._
import GameRandom._
import HexGameUtils._
import CustomMessage._



case class GameState(board: List[List[Cells.Cell]])

object HexGame{
	type Board = List[List[Cells.Cell]]
	
	def createEmptyBoard(size: Int): Board={
		List.fill(size)(List.fill(size)(Cells.Empty))
	}

	def getBoardSize(board: List[List[Cells.Cell]]): Int = { 
		board match {
			case Nil => 0
			case head :: tail => 1 + getBoardSize(tail)
		} 
	}

	def isValidMove(board: Board, player: Cells.Cell, row: Int, col: Int): (Board,Boolean,String)={
		val boardSize = getBoardSize(board)
		if(isBetweenBoundaries(boardSize,row, col)){
			if(isEmptyCell(board, row, col)){
				(play(board, player, row, col), true, CustomMessage.messageToString(HumanPlay, row.toString, col.toString))
			}else{
				(board, false, CustomMessage.messageToString(ErrorCellIsOccoupied))
			}
		}else{
			(board, false, CustomMessage.messageToString(ErrorOutOfBoundaries,boardSize.toString))
		}
	}

	def isBetweenBoundaries(boardSize: Int, row: Int, col: Int): Boolean={
		(row >= 1 && row <= boardSize) && (col >= 1 && col <= boardSize)
	}

	def isEmptyCell(board: Board, row: Int, col: Int): Boolean={
		board(row-1)(col-1)==Cells.Empty
	}

	def play (board: Board, player: Cells.Cell, row: Int, col: Int): Board ={
		updateBoard(board, row-1, col-1, player)
	}

	def updateBoard(list: List[List[Cells.Cell]], rowIndex: Int, colIndex: Int, newCell: Cells.Cell): List[List[Cells.Cell]] = {
		list match {
			case Nil => Nil
			case head :: tail if rowIndex == 0 => updateBoardRows(head, colIndex, newCell) :: tail
			case head :: tail => head :: updateBoard(tail, rowIndex-1, colIndex, newCell)
		}
	}

	def updateBoardRows(list: List[Cells.Cell], colIndex: Int, newCell: Cells.Cell): List[Cells.Cell] = {
		list match {
			case Nil => Nil
			case head :: tail if colIndex == 0 => newCell :: tail
			case head :: tail => head :: updateBoardRows(tail, colIndex - 1, newCell)
		} 
	}

	def collectCellIndeces(board: Board, player: Cells.Cell): List[(Int, Int)] = {
		def loop(rows: List[List[Cells.Cell]], player: Cells.Cell,rowNum: Int): List[(Int, Int)] = rows match {
			case Nil => Nil
			case row :: remainingRows =>
				val cellIndices = zipWithIndex(row, player,rowNum, 0)
				cellIndices ::: loop(remainingRows, player, rowNum  + 1)
			}

		loop(board, player, 0)
	}

	def zipWithIndex[A](list: List[Cells.Cell], player: Cells.Cell,rowIndex: Int, colIndex: Int ): List[(Int, Int)] ={
		list match {
			case Nil => Nil
			case head :: tail => 
				if(head == player){
					(rowIndex, colIndex) :: zipWithIndex(tail, player,rowIndex ,colIndex  + 1)
				}else{
					zipWithIndex(tail, player,rowIndex ,colIndex + 1)
				}
		}
	}

	def isWinningPath(coordinates: List[(Int, Int)], boardSize: Int): Boolean={

		def depthFirstSearchLoop(currentCoord: (Int, Int), visited: List[(Int, Int)]): Boolean={
			if(currentCoord._1==boardSize-1){
				true
			}else{
				val lst = addElementToList(visited,currentCoord)
				val neighborOffsets = getNeighborCells(currentCoord._1,currentCoord._2, boardSize)
				val neighbors = neighborOffsets.filter { case (x, y) =>
				coordinates.contains(x,y) && !visited.contains((x, y))
				}
				neighbors.foldLeft(false)((acc, neighbor) => acc || depthFirstSearchLoop(neighbor, lst))
			}
		}

		coordinates.exists { case (x, y) =>
			if (x == 0) {
				depthFirstSearchLoop((x, y), Nil)
			}else{
				false
			}
		}
	}

	def addElementToList(lst: List[(Int, Int)], newElement: (Int, Int)): List[(Int,Int)]={
		lst match{
			case Nil => List(newElement)
			case head::tail => head ::addElementToList(tail, newElement)
		}
	}

	def getNeighborCells(line: Int, col: Int, boardSize: Int): List[(Int,Int)]={
		if(line==0 && col==0) //canto superior esquerdo
		{
			List((0,1),(1,0))

		}
		else if(line == boardSize-1 && col == boardSize-1) //canto inferior direito
		{
			List((boardSize-1,boardSize-2), (boardSize-2,boardSize-1))
		}
		else if(line ==0 && col == boardSize-1) //canto superior direito
		{
			List((0, boardSize-2),(1, boardSize-2), (1, boardSize-1))
		}
		else if(line == boardSize-1 && col == 0) //canto inferior esquerdo
		{
			List((boardSize-2,0),(boardSize-2,1),(boardSize-1,1))
		}
		else if(line==0 && (col>0 || col<(boardSize-1))) //meio superior
		{
			List((line, col-1),(line, col+1),(line+1,col-1),(line+1,col))
		}
		else if(line==boardSize-1 && (col>0 || col<(boardSize-1))) // meio inferior
		{
			List((line,col-1),(line,col+1),(line-1,col),(line-1,col+1))
		}
		else // linhas do meio
		{
			List((line,col-1),(line,col+1),(line-1,col),(line-1,col+1),(line+1,col-1),(line+1,col))
		}
	}

	def hasWon(board:Board, player:Cells.Cell): Boolean={
		val lst = if(player==Cells.Red)
		{
			transposeCoords(collectCellIndeces(board, player))
		}
		else
		{
			collectCellIndeces(board, player)
		}
		if(isWinningPath(lst, getBoardSize(board))){
			if(player==Cells.Red){
				printMsgNl(CustomMessage.messageToString(WonGame))
			}else{
				printMsgNl(CustomMessage.messageToString(LostGame))
			}
			true
		}else{
			false
		}
	}

	def transposeCoords(lst: List[(Int,Int)]): List[(Int, Int)]={
		lst.map({case (x,y) => (y,x)})
	}

	def printTUI(board: Board): Unit={
		def printLoop(board: Board, numberOfLines: Int, currentLine: Int): Unit={
			if(currentLine==0)
			{
				println(createHorizontalLine(numberOfLines/2,s"$BLUE   *$RESET"))
				printLoop(board, numberOfLines, currentLine+1)
			}
			else if(currentLine==numberOfLines)
			{
				println(" "*numberOfLines+createHorizontalLine(numberOfLines/2,s"$BLUE   *$RESET" ))
			}
			else if(currentLine%2!=0)
			{ 
				val tmpBoard = getHead(board)
				println(" "*(currentLine-1) + s"$RED*$RESET"+convertLineToString(tmpBoard._1)+s"$RED *$RESET")
				printLoop(tmpBoard._2, numberOfLines,currentLine+1)
			}
			else
			{
				println(" "*(currentLine+2) + createHorizontalLine(numberOfLines/2-1," \\ /")+ " \\")
				printLoop(board, numberOfLines,currentLine+1)
			}
		}
		printLoop(board, getBoardSize(board)*2, 0)
	}


	def getHead[A](lst: List[List[A]]): (List[A], List[List[A]])={
		lst match{
			case Nil => (Nil,Nil)
			case head::tail=> (head,tail)
		}
	}

	def createHorizontalLine(size: Int, symbol: String): String={
		@tailrec
		def strLoop(size: Int, str: String, symbol: String): String={
			size match{
				case 0 => str
				case _ => strLoop(size-1,str+ symbol,symbol)
			}
		}
		strLoop(size, "",symbol)
	}

	def convertLineToString(lst: List[Cells.Cell]): String = {
		@tailrec
		def loop(lst: List[Cells.Cell], acc: String): String = {
			lst match {
				case Nil => acc
				case cell :: rest => loop(rest, acc + " - "+ Cells.boardSymbol(cell))
			}
		}
		loop(lst, "")
	}

	def getPlayerCoords(board: Board, player: Cells.Cell): (Int, Int)={
		val boardSize = getBoardSize(board)
		printMsgNl(CustomMessage.messageToString(CustomMessage.TurnToPlay))

		printMsgNl(CustomMessage.messageToString(CustomMessage.ProvidePlayRowIndex, boardSize.toString))
		val rowIndex = getUserInputInt(CustomMessage.messageToString(CustomMessage.ProvidePlayRowIndex,boardSize.toString))
		printMsgNl(CustomMessage.messageToString(CustomMessage.ProvidePlayColIndex, boardSize.toString))
		val colIndex = getUserInputInt(CustomMessage.messageToString(CustomMessage.ProvidePlayColIndex,boardSize.toString))

		val play = isValidMove(board, player, rowIndex, colIndex)
		if(play._2)
		{
			(rowIndex,colIndex)
		}
		else
		{
			printMsgNl(play._3)
			getPlayerCoords(board, player)
		}
	}

	def getComputerCoords(board: Board, randomState:GameRandomState): ((Int, Int),GameRandomState)={
		val playLst = collectCellIndeces(board, Cells.Empty)
		val rand = randomState.nextInt(playLst.length)
		(playLst(rand._1),rand._2)
	}

	def makePlay(board: Board, player: Cells.Cell, randomState: GameRandomState ): (Board,GameRandomState, String)={
		if(player==Cells.Red)
		{
			val hPos = getPlayerCoords(board, player)
			(play(board, player,hPos._1, hPos._2), randomState, CustomMessage.messageToString(HumanPlay,hPos._1.toString, hPos._2.toString))
		}
		else
		{
			val cPos = getComputerCoords(board, randomState)
			(play(board, player, cPos._1._1+1, cPos._1._2+1),cPos._2,CustomMessage.messageToString(ComputerPlay,cPos._1._1.toString, cPos._1._2.toString))
		}
	}

	@tailrec
	def playerAceptsPlay(): Boolean={
		printMsgNl(CustomMessage.messageToString(UndoPlay))
		val choice = getUserInputSTR.toUpperCase
		choice match{
			case "U" => false
			case "C" => true
			case _ =>playerAceptsPlay()
		}
	}

	def getOppositePlayer(player:Cells.Cell): Cell={
		if(player==Cells.Red)
		{
			Cells.Blue
		}
		else
		{
			Cells.Red
		}
	}

	def getBoardConf(): Board={
		printMsg(CustomMessage.messageToString(CustomMessage.BeginGame))
		val boardSize = getUserInputInt(CustomMessage.messageToString(CustomMessage.ErrorNotInt))
		if(boardSize>=4){
			val board =  createEmptyBoard(boardSize)
			printTUI(board)
			board
		}else{
			printMsgNl(CustomMessage.messageToString(CustomMessage.ErrorWrongSize))
			getBoardConf()
		}
		
	}

	
	def initializeGame(): Unit={
		val randomState =  GameRandom(Instant.now.getEpochSecond)
		val savedGame = getSavedGameState("savedGame.bin")
		if(savedGame.savedGame){
			val savedBoard =convertStringBoardToBoard(savedGame.board)
			printMsgNl(CustomMessage.messageToString(CustomMessage.SavedGame))
			printTUI(savedBoard)
			val ans = getUserInputSTR.toUpperCase
			ans match{
				case "Y" => gameLoop(savedBoard , savedGame.userTurn,randomState)
				case "N" => gameLoop(getBoardConf(), Cells.Red, randomState)
				case _ => printMsgNl(CustomMessage.messageToString(CustomMessage.ErrorWrongOption))
					initializeGame()

			}
		}else{
			gameLoop(getBoardConf(), Cells.Red, randomState)
		}
	}


	def gameController(gameState: Board, player: Cells.Cell, randomState: GameRandomState, gameFinished:Boolean): Unit={
		if(!gameFinished)
		{
			printMsgNl(CustomMessage.messageToString(CustomMessage.ContinuePlaying))
			val playerInput=getUserInputSTR.toUpperCase
			playerInput match{
				case "C" => gameLoop(gameState, player, randomState)
				case "SE"=> printMsgNl(CustomMessage.messageToString(CustomMessage.ExitSave))
							val saveGameState = SaveGameState(savedGame=true, userTurn=player, board=convertBoardToString(gameState))
							saveGameStateToBinaryFile(saveGameState, "savedGame.bin")
				case "E"=> printMsgNl(CustomMessage.messageToString(CustomMessage.Exit))
				case _ => printMsgNl(CustomMessage.messageToString(ErrorWrongOption))
					gameController(gameState, player, randomState, false)
			}
		}
		else
		{
			printMsgNl(CustomMessage.messageToString(CustomMessage.NewGame))
			val choice = getUserInputSTR.toUpperCase
			choice match{
				case "N" => printMsgNl(CustomMessage.messageToString(CustomMessage.Exit))
				case "Y" => initializeGame()
				case _ => printMsgNl(CustomMessage.messageToString(ErrorWrongOption))
					gameController(gameState,player,randomState,gameFinished)
			}
		}
	}

	def gameLoop(gameState: Board, player: Cells.Cell, randomState: GameRandomState): Unit={
		if(!hasWon(gameState, getOppositePlayer(player)))
		{
			val move = makePlay(gameState, player, randomState)
			printMsgNl(move._3)
			printTUI(move._1)

			if(playerAceptsPlay())
			{
				gameController(move._1,getOppositePlayer(player),move._2,false)
			}
			else
			{
				gameController(gameState, player, randomState, false)
			}
		}
		else
		{
			gameController(Nil, player, randomState, true)
		}
	}

	def convertBoardToString(board: Board): List[List[String]]={
		board.map(_.map(_.toString))
	}

	def convertStringBoardToBoard(board: List[List[String]]): Board={
		board.map(_.map(strToCell))
	}

	def strToCell(str: String): Cells.Cell={
		str match{
			case "Red" => Cells.Red
			case "Blue" => Cells.Blue
			case "Empty" => Cells.Empty
		}
	}

	def main(args: Array[String]): Unit={
		initializeGame()
	}

}