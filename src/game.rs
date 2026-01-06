use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use colored::Colorize;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Square (pub usize, pub usize);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Move (pub Square, pub Square);

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum Color { White, Black, Blank }

impl fmt::Display for Color {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let c = match self {
			Color::White => "White",
			Color::Black => "Black",
			Color::Blank => "Empty",
		};
		write!(f, "{}", c)
	}
}

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub enum Role {
	Queen,
	King,
	Rook,
	Bishop,
	Knight,
	Pawn,
	Blank,
}

#[derive(PartialEq, Clone, Copy, Hash, Eq)]
pub struct Piece {
    pub role: Role,
	pub color: Color,
}

pub fn is_valid_idx(x: i8) -> bool { x >= 0 && x < 8  }

pub fn get_other_color(c: Color) -> Color { if c == Color::White { Color::Black } else { Color::White } }

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = match self.role {
            Role::Queen => "Q",
	     	Role::King => "K",
    	    Role::Bishop => "B",
		    Role::Knight => "N",
		    Role::Rook => "R",
		    Role::Pawn => "P",
			Role::Blank => "-",
        };
        write!(f, "{}", if self.color == Color::White {r.white()} else {r.green()} )
    }
}

// Immutable game state that can be shared via Rc
#[derive(Clone)]
pub struct GameState {
	pub board: [[Piece; 8]; 8],
	pub turn: Color,
	pub white_castle_left: bool,
	pub white_castle_right: bool,
	pub black_castle_left: bool,
	pub black_castle_right: bool,

	// These are characteristics of the state,
	// stored here to avoid recalculating them.
	// As such, they are not used for equality or hashing.
	pub white_check: bool,
	pub black_check: bool,
	pub checkmate: bool,
	pub move_list: Option<Vec<Move>>,

	// Used for branching and backtracking
	pub parent: Option<Rc<GameState>>,
	pub since_last_non_reversible_move: usize,
}

impl PartialEq for GameState {
	fn eq(&self, other: &Self) -> bool {
		self.board == other.board &&
		self.turn == other.turn &&
		self.white_castle_left == other.white_castle_left &&
		self.white_castle_right == other.white_castle_right &&
		self.black_castle_left == other.black_castle_left &&
		self.black_castle_right == other.black_castle_right
	}
}

impl Eq for GameState {}

impl Hash for GameState {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.board.hash(state);
		self.turn.hash(state);
		self.white_castle_left.hash(state);
		self.white_castle_right.hash(state);
		self.black_castle_left.hash(state);
		self.black_castle_right.hash(state);
	}
}

impl GameState {
    fn is_square_unoccupied(&self, square: Square, color: Color) -> bool {
        self.board[square.0][square.1].color != color
    }

	fn is_square_color(&self, square: Square, color: Color) -> bool {
        self.board[square.0][square.1].color == color
    }

    fn is_square_blank(&self, sq: &Square) -> bool {
		self.board[sq.0][sq.1].color == Color::Blank
	}

	#[inline(never)]
	pub fn generate_new_state(&self, mv: &Move) -> GameState {
		// Create a new GameState with the move applied. Do not modify self.
		let from = mv.0;
		let to = mv.1;
		let mut new_board = self.board;
		new_board[to.0][to.1] = self.board[from.0][from.1];
		if (to.0 == 7 || to.0 == 0) && new_board[to.0][to.1].role == Role::Pawn {
			new_board[to.0][to.1].role = Role::Queen;
		}
		new_board[from.0][from.1] = Piece { role: Role::Blank, color: Color::Blank };

		let white_castle_left = if self.white_castle_left {
			!((from.0 == 7 && from.1 == 0) || (from.0 == 7 && from.1 == 4))
		} else {
			false
		};

		let white_castle_right = if self.white_castle_right {
			!((from.0 == 7 && from.1 == 7) || (from.0 == 7 && from.1 == 4))
		} else {
			false
		};

		let black_castle_left = if self.black_castle_left {
			!((from.0 == 0 && from.1 == 0) || (from.0 == 0 && from.1 == 4))
		} else {
			false
		};

		let black_castle_right = if self.black_castle_right {
			!((from.0 == 0 && from.1 == 7) || (from.0 == 0 && from.1 == 4))
		} else {
			false
		};

		// Create new state with parent pointing to current state
		// This allows branching: multiple games can share the same parent state
		// Rc::clone() just increments the reference count, so the parent is shared
		let mut new_state = GameState {
			board: new_board,
			turn: get_other_color(self.turn),

			white_castle_left,
			white_castle_right,
			black_castle_left,
			black_castle_right,
			move_list: None,
			white_check: false,
			black_check: false,
			checkmate: false,
			parent: None,
			since_last_non_reversible_move: 0,
		};

		let black_moves = get_all_valid_moves_fast(&new_state, Some(Color::Black));
		let white_moves = get_all_valid_moves_fast(&new_state, Some(Color::White));

		let white_check = black_moves.iter().fold(false, |acc, mv| {
			let to_piece = new_state.board[mv.1.0][mv.1.1];
			if to_piece.role == Role::King {
				acc || to_piece.color == Color::White
			} else {
				acc
			}
		});

		let black_check = white_moves.iter().fold(false, |acc, mv| {
			let to_piece = new_state.board[mv.1.0][mv.1.1];
			if to_piece.role == Role::King {
				acc || to_piece.color == Color::Black
			} else {
				acc
			}
		});

		new_state.white_check = white_check;
		new_state.black_check = black_check;
		new_state.move_list = Some(if new_state.turn == Color::White { white_moves } else { black_moves });

		new_state.since_last_non_reversible_move = if
		    // If the move is a pawn promotion or a capture, reset the counter
		    self.board[mv.0.0][mv.0.1].role == Role::Pawn || self.board[mv.1.0][mv.1.1].role != Role::Blank { 
			0
		} else {
			self.since_last_non_reversible_move + 1
		};

		return new_state;
	}
}

// Game is just a reference to a GameState, enabling branching
#[derive(Clone)]
pub struct Game {
	pub state: Rc<GameState>,
}

// Implement Deref so we can access GameState fields directly (e.g., game.board instead of game.state.board)
impl Deref for Game {
	type Target = GameState;
	
	fn deref(&self) -> &Self::Target {
		&self.state
	}
}

impl fmt::Debug for GameState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	    fn fmt_row(row: [Piece; 8]) -> String {
		    format!("{} {} {} {} {} {} {} {}", row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7])
		}

		write!(f, "  0 1 2 3 4 5 6 7\n0 {}\n1 {}\n2 {}\n3 {}\n4 {}\n5 {}\n6 {}\n7 {}",
		       fmt_row(self.board[0]),
			   fmt_row(self.board[1]),
			   fmt_row(self.board[2]),
			   fmt_row(self.board[3]),
			   fmt_row(self.board[4]),
			   fmt_row(self.board[5]),
			   fmt_row(self.board[6]),
			   fmt_row(self.board[7]),
		)
    }
}
	
impl fmt::Debug for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	    fn fmt_row(row: [Piece; 8]) -> String {
		    format!("{} {} {} {} {} {} {} {}", row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7])
		}

		write!(f, "  0 1 2 3 4 5 6 7\n0 {}\n1 {}\n2 {}\n3 {}\n4 {}\n5 {}\n6 {}\n7 {}",
		       fmt_row(self.board[0]),
			   fmt_row(self.board[1]),
			   fmt_row(self.board[2]),
			   fmt_row(self.board[3]),
			   fmt_row(self.board[4]),
			   fmt_row(self.board[5]),
			   fmt_row(self.board[6]),
			   fmt_row(self.board[7]),
		)
    }
}

impl Game {
	pub fn from(game_state: GameState) -> Game {
		Game { state: Rc::new(game_state) }
	}

    pub fn new() -> Game {
	    let row_eight = [
			Piece { role: Role::Rook, color: Color::Black },
			Piece { role: Role::Knight, color: Color::Black },
			Piece { role: Role::Bishop, color: Color::Black },
			Piece { role: Role::Queen, color: Color::Black },
			Piece { role: Role::King, color: Color::Black },
			Piece { role: Role::Bishop, color: Color::Black },
			Piece { role: Role::Knight, color: Color::Black },
			Piece { role: Role::Rook, color: Color::Black },
		];
		let row_seven = [Piece { role: Role::Pawn, color: Color::Black }; 8];
		let row_six_thru_three = [Piece { role: Role::Blank, color:Color::Blank }; 8];
		let row_two = [Piece { role: Role::Pawn, color: Color::White }; 8];
		let row_one = [
			Piece { role: Role::Rook, color: Color::White },
			Piece { role: Role::Knight, color: Color::White },
			Piece { role: Role::Bishop, color: Color::White },
			Piece { role: Role::Queen, color: Color::White },
			Piece { role: Role::King, color: Color::White },
			Piece { role: Role::Bishop, color: Color::White },
			Piece { role: Role::Knight, color: Color::White },
			Piece { role: Role::Rook, color: Color::White },
		];
		Game {
			state: Rc::new(GameState {
				board: [
					row_eight,
					row_seven,
					row_six_thru_three,
					row_six_thru_three.clone(),
					row_six_thru_three.clone(),
					row_six_thru_three.clone(),
					row_two,
					row_one,
				],
				turn: Color::White,
				white_castle_left: true,
				white_castle_right: true,
				black_castle_left: true,
				black_castle_right: true,
				white_check: false,
				black_check: false,
				checkmate: false,
				move_list: Some(vec![Move(Square(6, 3), Square(4, 3))]), // Max possible moves in chess
				parent: None,
				since_last_non_reversible_move: 0,
			}),
		}
	}

	// Reverses most recently made move
	pub fn reverse(&mut self) -> () {
		// Access the parent GameState through Rc without moving
		if let Some(parent_state) = &self.parent {
			// Clone the Rc to point to the same parent state (shared reference)
			self.state = Rc::clone(parent_state);
		}
	}

	#[inline(never)]
	pub fn make_move(&mut self, mv: Move) -> () {
		// Create a new GameState with the move applied
		// The parent is the current state (shared via Rc for branching)
		let mut new_state = self.state.generate_new_state(&mv);
		new_state.parent = Some(Rc::clone(&self.state));
		new_state.checkmate = self.get_is_checkmate(&new_state);

		self.state = Rc::new(new_state);
	}

	// Evaluates if the current position is checkmate via the following criteria:
	// 1. The king is attacked (a move "to" Square equals the king's position)
	// 2. No moves will result in the king not being attacked
	#[inline(never)]
	fn get_is_checkmate(&self, game_state: &GameState) -> bool {
		// Checkmate always occurs at the start of your turn
		match game_state.turn {
			Color::Black => {		
				if game_state.black_check {
					for mv in game_state.move_list.as_ref().unwrap().iter() {
						let new_state = game_state.generate_new_state(mv);
						if !new_state.black_check {
							return false;
						}
					}
					return true;
				}
			}
			Color::White => {		
				if game_state.white_check {
					for mv in game_state.move_list.as_ref().unwrap().iter() {
						let new_state = game_state.generate_new_state(mv);
						if !new_state.white_check {
							return false;
						}
					}
					return true;
				}
			}
			Color::Blank => ()
		}

		return false;
	}
	
	pub fn get_all_valid_moves(&self) -> Vec<Move> {
        let mut output = vec![];
		for x in 0..8 {
			for y in 0..8 {
				let piece = self.board[x][y];
				if piece.color == self.turn {
					output.extend(get_moves(Square(x, y), self));
				}
			}
		}
		output
	}

	pub fn get_is_valid_move(&self, mv: Move) -> bool {
		let mvs = self.get_all_valid_moves();
		mvs.contains(&mv)
	}

	pub fn get_turn(&self) -> Color { self.turn } 
}

fn get_moves_step_out(sq: &Square, game: &GameState, piece_color: Color, steps: &[(i8, i8)]) -> Vec<Move> {
	let pairs = steps;
	let mut valid_moves = Vec::with_capacity(20);
	let mut valid_pairs = [true; 8];
	for i in 1..8 {
		for (idx, p) in pairs.iter().enumerate() {
			if !valid_pairs[idx] {
				continue;
			}

			let x = sq.0 as i8 + (i as i8 * p.0);
			let y = sq.1 as i8 + (i as i8 * p.1);
			let next_square = Square(x as usize, y as usize);
			if is_valid_idx(x) && is_valid_idx(y) {
				if game.is_square_blank(&next_square) {
					valid_moves.push(Move(Square(sq.0,sq.1), next_square));
				} else if game.is_square_color(next_square, piece_color) {
                    valid_pairs[idx] = false;
				} else {
					valid_pairs[idx] = false;
                    valid_moves.push(Move(Square(sq.0,sq.1), next_square));
				}
			}
		}
	}
	valid_moves
}

const KNIGHT_MOVE_PAIRS: [(i8, i8); 8] = [
	(1,2),
	(2,1),
	(-1,2),
	(2,-1),
	(1,-2),
	(-2,1),
	(-1,-2),
	(-2,-1),
];

const ROOK_MOVE_PAIRS: [(i8, i8); 4] = [
	(0,1),
	(0,-1),
	(1,0),
	(-1,0),
];

const BISHOP_MOVE_PAIRS: [(i8, i8); 4] = [
	(1,1),
	(1,-1),
	(-1,1),
	(-1,-1),
];

const QUEEN_MOVE_PAIRS: [(i8, i8); 8] = [
	(1,1),
	(1,-1),
	(-1,1),
	(-1,-1),
	(0,1),
	(0,-1),
	(1,0),
	(-1,0),
];

const KING_MOVE_PAIRS: [(i8, i8); 8] = [
	(0,1),
	(0,-1),
	(1,0),
	(1,1),
	(1,-1),
	(-1,0),
	(-1,1),
	(-1,-1),
];

#[inline(never)]
pub fn get_moves(sq: Square, game: &GameState) -> Vec<Move> {
	let piece = game.board[sq.0][sq.1];
	let mut valid_moves = vec![];
	match piece.role {
		Role::Queen => {
			valid_moves = get_moves_step_out(&sq, game, piece.color, &QUEEN_MOVE_PAIRS);
		}
		Role::King => {
			let pairs = [
				[0,1],
				[0,-1],
				[1,0],
				[1,1],
				[1,-1],
				[-1,0],
				[-1,1],
				[-1,-1]
			];
			for move_pair in pairs {
				let new_x = sq.0 as i8 + move_pair[0];
				let new_y = sq.1 as i8 + move_pair[1];
				if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
					let new_x = new_x as usize;
					let new_y = new_y as usize;
					if !game.is_square_color(Square(new_x, new_y), piece.color) {
						valid_moves.push(Move(Square(sq.0,sq.1), Square(new_x, new_y)));
					}
				}
			}

			match piece.color {
				Color::White => {
					if game.white_castle_left {
						if [(7,1), (7,2), (7,3)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(7, 4), Square(7, 0)));
							}
					}
					if game.white_castle_right {
						if [(7,6), (7,5)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(7, 4), Square(7, 7)));
							}
					}
				}
				Color::Black => {
					if game.black_castle_left {
						if [(0,1), (0,2), (0,3)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(0, 4), Square(0, 0)));
							}
					}
					if game.black_castle_right {
						if [(0,6), (0,5)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(0, 4), Square(0, 7)));
							}
					}
				}
				_ => ()
			}
		}
		Role::Bishop => {
			valid_moves = get_moves_step_out(&sq, game, piece.color, &BISHOP_MOVE_PAIRS);
		}
		Role::Knight => {
			for move_pair in &KNIGHT_MOVE_PAIRS {
				let new_x = sq.0 as i8 + move_pair.0;
				let new_y = sq.1 as i8 + move_pair.1;
				if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
					let new_x = new_x as usize;
					let new_y = new_y as usize;
					if game.is_square_unoccupied(Square(new_x, new_y), piece.color) {
						valid_moves.push(Move(Square(sq.0,sq.1), Square(new_x, new_y)));
					}
				}
			}
		}
		Role::Rook => {
			valid_moves = get_moves_step_out(&sq, game, piece.color, &ROOK_MOVE_PAIRS);
		}
		// TODO: En passant
		Role::Pawn => {
			match piece.color {
				Color::White => {
					if sq.0 > 0 && game.is_square_blank(&Square(sq.0 - 1, sq.1)) {
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1)));
					}
					if
					    sq.0 > 0 && sq.1 > 0
						&& game.is_square_color(Square(sq.0 - 1, sq.1 - 1), Color::Black)
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1 - 1)));
					}
					if
					    sq.0 > 0 && sq.1 < 7
						&& game.is_square_color(Square(sq.0 - 1, sq.1 + 1), Color::Black)
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1 + 1)));
					}
					if
						sq.0 == 6
						&& game.is_square_blank(&Square(sq.0 - 1, sq.1))
						&& game.is_square_blank(&Square(sq.0 - 2, sq.1))
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 2, sq.1)));
					}
				}
				Color::Black => {
					if sq.0 < 7 && game.is_square_blank(&Square(sq.0 + 1, sq.1)) {
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1)));
					}
					if
						sq.0 < 7 && sq.1 > 0
						&& game.is_square_color(Square(sq.0 + 1, sq.1 - 1), Color::White)
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1 - 1)));
					}
					if
						sq.0 < 7 && sq.1 < 7
						&& game.is_square_color(Square(sq.0 + 1, sq.1 + 1), Color::White)
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1 + 1)));
					}
					if // Only allow two forward if on starting row
						sq.0 == 1
						&& game.is_square_blank(&Square(sq.0 + 1, sq.1))
						&& game.is_square_blank(&Square(sq.0 + 2, sq.1))
					{
						valid_moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 2, sq.1)));
					}
				}
				Color::Blank => ()
			}
		}
		Role::Blank => ()
	}

	return valid_moves;
}

fn get_all_moves(game: &GameState) -> Vec<Move> {
	let mut output = vec![];
	for x in 0..8 {
		for y in 0..8 {
			output.extend(get_moves(Square(x, y), game));
		}
	}
	output
}

fn add_moves_step_out(output: &mut Vec<Move>, sq: &Square, game: &GameState, piece_color: Color, pairs: &[(i8, i8)]) -> () {
	let mut valid_pairs = [true; 8];	
	for i in 1..8 {
		for (idx, p) in pairs.iter().enumerate() {
			if !valid_pairs[idx] {
				continue;
			}

			let x = sq.0 as i8 + (i as i8 * p.0);
			let y = sq.1 as i8 + (i as i8 * p.1);
			let next_square = Square(x as usize, y as usize);
			if is_valid_idx(x) && is_valid_idx(y) {
				if game.is_square_blank(&next_square) {
					output.push(Move(Square(sq.0,sq.1), next_square));
				} else if game.is_square_color(next_square, piece_color) {
                    valid_pairs[idx] = false;
				} else {
					valid_pairs[idx] = false;
                    output.push(Move(Square(sq.0,sq.1), next_square));
				}
			}
		}
	}
}

#[inline(never)]
pub fn get_all_valid_moves_fast(game: &GameState, color_override: Option<Color>) -> Vec<Move> {
	// TODO: There should be a "Max possible moves" value in chess,
	// So we can pre-allocate and re-use an array to accel this.
	let mut moves = Vec::with_capacity(218);

	for x in 0..8 {
		for y in 0..8 {
			let piece = game.board[x][y];
			let sq = Square(x, y);
			let color = if color_override.is_some() { color_override.unwrap() } else { game.turn };
		if piece.color == color {
			match piece.role {
				Role::Queen => {
					add_moves_step_out(&mut moves, &sq, game, piece.color, &QUEEN_MOVE_PAIRS);
				}
				Role::King => {
					for move_pair in &KING_MOVE_PAIRS {
						let new_x = sq.0 as i8 + move_pair.0;
						let new_y = sq.1 as i8 + move_pair.1;
						if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
							let new_x = new_x as usize;
							let new_y = new_y as usize;
							if !game.is_square_color(Square(new_x, new_y), piece.color) {
								moves.push(Move(Square(sq.0,sq.1), Square(new_x, new_y)));
							}
						}
					}
		
					match piece.color {
						Color::White => {
							if game.white_castle_left {
								if [(7,1), (7,2), (7,3)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(7, 4), Square(7, 0)));
									}
							}
							if game.white_castle_right {
								if [(7,6), (7,5)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(7, 4), Square(7, 7)));
									}
							}
						}
						Color::Black => {
							if game.black_castle_left {
								if [(0,1), (0,2), (0,3)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(0, 4), Square(0, 0)));
									}
							}
							if game.black_castle_right {
								if [(0,6), (0,5)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(0, 4), Square(0, 7)));
									}
							}
						}
						_ => ()
					}
				}
				Role::Bishop => {
					add_moves_step_out(&mut moves, &sq, game, piece.color, &BISHOP_MOVE_PAIRS);
				}
				Role::Knight => {
					for move_pair in &KNIGHT_MOVE_PAIRS {
						let new_x = sq.0 as i8 + move_pair.0;
						let new_y = sq.1 as i8 + move_pair.1;
						if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
							let new_x = new_x as usize;
							let new_y = new_y as usize;
							if game.is_square_unoccupied(Square(new_x, new_y), piece.color) {
								moves.push(Move(Square(sq.0,sq.1), Square(new_x, new_y)));
							}
						}
					}
				}
				Role::Rook => add_moves_step_out(&mut moves, &sq, game, piece.color, &ROOK_MOVE_PAIRS),
				// TODO: En passant
				Role::Pawn => {
					match piece.color {
						Color::White => {
							if sq.0 > 0 && game.is_square_blank(&Square(sq.0 - 1, sq.1)) {
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1)));
							}
							if
								sq.0 > 0 && sq.1 > 0
								&& game.is_square_color(Square(sq.0 - 1, sq.1 - 1), Color::Black)
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1 - 1)));
							}
							if
								sq.0 > 0 && sq.1 < 7
								&& game.is_square_color(Square(sq.0 - 1, sq.1 + 1), Color::Black)
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 1, sq.1 + 1)));
							}
							if
								sq.0 == 6
								&& game.is_square_blank(&Square(sq.0 - 1, sq.1))
								&& game.is_square_blank(&Square(sq.0 - 2, sq.1))
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 - 2, sq.1)));
							}
						}
						Color::Black => {
							if sq.0 < 7 && game.is_square_blank(&Square(sq.0 + 1, sq.1)) {
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1)));
							}
							if
								sq.0 < 7 && sq.1 > 0
								&& game.is_square_color(Square(sq.0 + 1, sq.1 - 1), Color::White)
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1 - 1)));
							}
							if
								sq.0 < 7 && sq.1 < 7
								&& game.is_square_color(Square(sq.0 + 1, sq.1 + 1), Color::White)
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 1, sq.1 + 1)));
							}
							if // Only allow two forward if on starting row
								sq.0 == 1
								&& game.is_square_blank(&Square(sq.0 + 1, sq.1))
								&& game.is_square_blank(&Square(sq.0 + 2, sq.1))
							{
								moves.push(Move(Square(sq.0,sq.1), Square(sq.0 + 2, sq.1)));
							}
						}
						Color::Blank => ()
					}
				}
				Role::Blank => ()
			}
		}
	}
	}
	moves
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_board() {
		let mut board_one = Game::new();
        board_one.make_move(Move(Square(1,4), Square(3,4)));
	}

	#[test]
	fn test_reverse() {
		let mut game = Game::new();
		game.make_move(Move(Square(0,1), Square(2,2)));
		assert_eq!(game.board[0][1].role, Role::Blank);
		assert_eq!(game.board[2][2].role, Role::Knight);

		game.reverse();
		assert_eq!(game.board[2][2].role, Role::Blank);
		assert_eq!(game.board[0][1].role, Role::Knight);
	}

	#[test]
	fn test_check() {
		let mut game = Game::new();
		game.make_move(Move(Square(0, 4), Square(2, 3)));
		
		game.make_move(Move(Square(2, 3), Square(6, 5)));
		assert_eq!(game.white_check, true);
		assert_eq!(game.black_check, true);
	}

	#[test]
	fn test_checkmate() {
		let mut game = Game::new();

		// Move the white queen to attack the black king. This should
		// NOT be checkmate because the king can take the queen.
		game.make_move(Move(Square(7, 3), Square(1, 5)));
		assert_eq!(game.checkmate, false);

		// Pass the turn and defend the white queen-- now black is in CM
		game.make_move(Move(Square(0, 1), Square(0, 0)));
		game.make_move(Move(Square(7, 1), Square(3, 6)));
		assert_eq!(game.checkmate, true);
	}

    #[test]
	fn test_knight_move() {
        let board = Game::new();
		let knight_moves = get_moves(Square(0,1), &board);
		assert_eq!(
			knight_moves,
			vec![
				Move(Square(0, 1), Square(2, 2)),
				Move(Square(0, 1), Square(2, 0)),
			],
		);
    }

    #[test]
	fn test_pawn_move() {
        let board = Game::new();
		let left_pawn_moves = get_moves(Square(1,0), &board);
		let right_pawn_moves = get_moves(Square(1,7), &board);
        assert_eq!(
		    left_pawn_moves,
			vec![
			    Move(Square(1, 0), Square(2, 0)),
				Move(Square(1, 0), Square(3, 0)),
			],
		);

        assert_eq!(
		    right_pawn_moves,
			vec![
			    Move(Square(1, 7), Square(2, 7)),
				Move(Square(1, 7), Square(3, 7)),
			],
		);
    }

    #[test]
	fn test_rook_move() {
        let mut board = Game::new();
		let moves = get_moves(Square(0,0), &board);
		assert_eq!(moves, vec![]);

        board.make_move(Move(Square(0, 0), Square(2, 3)));
		let moves = get_moves(Square(2,3), &board);
		assert_eq!(
		    moves,
			vec![
				Move(Square(2, 3), Square(2, 4)),
				Move(Square(2, 3), Square(2, 2)),
				Move(Square(2, 3), Square(3, 3)),
				Move(Square(2, 3), Square(2, 5)),
				Move(Square(2, 3), Square(2, 1)),
				Move(Square(2, 3), Square(4, 3)),
				Move(Square(2, 3), Square(2, 6)),
				Move(Square(2, 3), Square(2, 0)),
				Move(Square(2, 3), Square(5, 3)),
				Move(Square(2, 3), Square(2, 7)),
				Move(Square(2, 3), Square(6, 3)),
			],
		);
	}

	#[test]
	fn test_bishop_move() {
        let mut board = Game::new();
		let moves = get_moves(Square(0,2), &board);
		assert_eq!(moves, vec![]);

        board.make_move(Move(Square(0, 2), Square(2, 3)));
		let moves = get_moves(Square(2,3), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(2, 3),Square(3, 4)),
				Move(Square(2, 3), Square(3, 2)),
				Move(Square(2, 3), Square(4, 5)),
				Move(Square(2, 3), Square(4, 1)),
				Move(Square(2, 3), Square(5, 6)),
				Move(Square(2, 3), Square(5, 0)),
				Move(Square(2, 3), Square(6, 7)),
			],
		);
	}

	#[test]
	fn test_king_move() {
        let mut board = Game::new();
		let moves = get_moves(Square(0,4), &board);
		assert_eq!(moves, vec![]);

		// Move everything out of the way to test castling
		board.make_move(Move(Square(0, 1), Square(7,7))); // B
		board.make_move(Move(Square(0, 2), Square(7,7))); // W
		board.make_move(Move(Square(0, 3), Square(7,7))); // B
		board.make_move(Move(Square(0, 5), Square(7,7))); // W
		board.make_move(Move(Square(0, 6), Square(7,7))); // B
		let moves = get_moves(Square(0, 4), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(0, 4),Square(0, 5)),
				Move(Square(0, 4), Square(0, 3)),
				Move(Square(0, 4), Square(0, 0)),
				Move(Square(0, 4), Square(0, 7)),
			],
		);

		// Shuffle back and forth
		board.make_move(Move(Square(0, 4), Square(0, 5))); // W
		board.make_move(Move(Square(0, 5), Square(0, 4))); // B
		let moves = get_moves(Square(0,4), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(0, 4),Square(0, 5)),
				Move(Square(0, 4), Square(0, 3)),
			],
		);

	}

	#[test]
	fn test_queen_move() {
        let mut board = Game::new();
		let moves = get_moves(Square(0,3), &board);
		assert_eq!(moves, vec![]);

        board.make_move(Move(Square(0, 3), Square(2, 3)));
		let moves = get_moves(Square(2,3), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(2, 3), Square(3, 4)),
				Move(Square(2, 3), Square(3, 2)),
				Move(Square(2, 3), Square(2, 4)),
				Move(Square(2, 3), Square(2, 2)),
				Move(Square(2, 3), Square(3, 3)),
				Move(Square(2, 3), Square(4, 5)),
				Move(Square(2, 3), Square(4, 1)),
				Move(Square(2, 3), Square(2, 5)),
				Move(Square(2, 3), Square(2, 1)),
				Move(Square(2, 3), Square(4, 3)),
				Move(Square(2, 3), Square(5, 6)),
				Move(Square(2, 3), Square(5, 0)),
				Move(Square(2, 3), Square(2, 6)),
				Move(Square(2, 3), Square(2, 0)),
				Move(Square(2, 3), Square(5, 3)),
				Move(Square(2, 3), Square(6, 7)),
				Move(Square(2, 3), Square(2, 7)),
				Move(Square(2, 3), Square(6, 3)),
			],
		);
	}
}
