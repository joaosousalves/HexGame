package Hex
object CustomMessage extends Enumeration {
	type Message = Value
	val BeginGame, SavedGame,TurnToPlay,ProvidePlayRowIndex, ProvidePlayColIndex, HumanPlay, ComputerPlay, UndoPlay, WonGame, LostGame, ContinuePlaying, NewGame, Exit, ExitSave, ErrorWrongSize, ErrorOutOfBoundaries, ErrorCellIsOccoupied, ErrorNotInt, ErrorWrongOption = Value
	def messageToString(msg: Message, placeHolder: String ="", placeHolder1: String =""): String={
		msg match {
			case BeginGame => "Let's beging the Hex board game versus the computer! To start please provide a number greater or equal to 4 for the board size:"
			case SavedGame => "Would you like to continue the saved game (Y\\N)"
			case TurnToPlay => "It's your turn to play!"
			case ProvidePlayRowIndex => s"Please provide the row index between 1 and $placeHolder!"
			case ProvidePlayColIndex => s"Please provide the colum index between 1 and $placeHolder!"
			case HumanPlay => s"You have played: $placeHolder $placeHolder1"
			case ComputerPlay => s"computer played: $placeHolder $placeHolder1"
			case UndoPlay => "If you would like to undo this play, please type U. Otherwise typy C to continue"
			case WonGame => "Congrats! You won the game!!"
			case LostGame => "Game over!! PC Wins!"
			case ContinuePlaying => "If you want to keep playing type C. If you would like to exit and save the game type SE, or just exit type E"
			case NewGame => "Would want to play a new game? (Y//N)"
			case Exit => "Goodbye! See you next time!!"
			case ExitSave => "You game has been saved successfully! See you next time!!"
			case ErrorWrongSize => "Please provide a value greater or equal to 4!"
			case ErrorOutOfBoundaries => s"Invalid play, coordinates must be between1 and $placeHolder!"
			case ErrorCellIsOccoupied => s"Invalid play, the cell is already occoupied!"
			case ErrorNotInt => "Incrrect ipnut! Please porvide a number."
			case ErrorWrongOption => "Please provide a correct option!"
		}
	}
}