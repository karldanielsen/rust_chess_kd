use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use rand::Rng;
use colored::Colorize;
use once_cell::sync::Lazy;

use crate::constants::{TRANSPOSITION_TABLE_SIZE, ALL_KNIGHT_MASKS, ALL_KING_MASKS, ALL_ROOK_MASKS, ALL_BISHOP_MASKS};

fn generate_zobrist_number() -> u64 {
	let mut rng = rand::rng();
	rng.random_range(0..TRANSPOSITION_TABLE_SIZE as u64)
}

fn role_to_zobrist_index(role: Role, color: Color, square: Square) -> usize {
	let role_offset = match role {
		Role::Queen => 0,
		Role::King => 1,
		Role::Rook => 2,
		Role::Bishop => 3,
		Role::Knight => 4,
		Role::Pawn => 5,
		Role::Blank => panic!("Tried to get Zobrist index for blank square, role: {:?}, color: {:?}, square: {:?}", role, color, square),
	};
	let color_offset = match color {
		Color::White => 0,
		Color::Black => 1,
		Color::Blank => panic!("Tried to get Zobrist index for blank square, role: {:?}, color: {:?}, square: {:?}", role, color, square),
	};
	(color_offset * 64 * 6) + (role_offset * 64) + (square.0 * 8 + square.1)
}

static ZOBRIST_NUMBERS: Lazy<[u64; 773]> = Lazy::new(|| {
	let mut numbers = [0u64; 773];
	for i in 0..773 {
		numbers[i] = generate_zobrist_number();
	}
	numbers
});

#[derive(Clone)]
pub struct ZobristHash {
	pub hash: u64,
}

impl ZobristHash {
	pub fn new() -> ZobristHash {
		ZobristHash { hash: 0 }
	}

	pub fn from(game_state: &GameState) -> ZobristHash {
		let mut hash = 0;
		for x in 0..8 {
			for y in 0..8 {
				let piece = game_state.board[x][y];
				if piece.color != Color::Blank {
					let idx = role_to_zobrist_index(piece.role, piece.color, Square(x, y));
					hash ^= ZOBRIST_NUMBERS[idx];
				}
			}
		}
		if game_state.turn == Color::Black {
			hash ^= ZOBRIST_NUMBERS[64 * 5];
		}

		if game_state.white_castle_left {
			hash ^= ZOBRIST_NUMBERS[64 * 12 + 1];
		}
		if game_state.white_castle_right {
			hash ^= ZOBRIST_NUMBERS[64 * 12 + 2];
		}
		if game_state.black_castle_left {
			hash ^= ZOBRIST_NUMBERS[64 * 12 + 3];
		}
		if game_state.black_castle_right {
			hash ^= ZOBRIST_NUMBERS[64 * 12 + 4];
		}
		ZobristHash { hash }
	}

	pub fn update_move(&mut self, game_state: &GameState, mv: &Move) {
		let from = mv.0;
		let to = mv.1;
		let piece = game_state.board[from.0][from.1];
		let target_piece = game_state.board[to.0][to.1];
		let from_idx = role_to_zobrist_index(piece.role, piece.color, from);
		let to_idx = role_to_zobrist_index(piece.role, piece.color, to);

		self.hash ^= ZOBRIST_NUMBERS[from_idx];
		self.hash ^= ZOBRIST_NUMBERS[to_idx];
		if target_piece.color != Color::Blank {
			let attacked_piece_idx = role_to_zobrist_index(target_piece.role, target_piece.color, to);
			self.hash ^= ZOBRIST_NUMBERS[attacked_piece_idx];
		}
	}

	pub fn update_turn(&mut self) {
		self.hash ^= ZOBRIST_NUMBERS[64 * 5];
	}

	pub fn update_castle_rights(&mut self, white_left: bool, white_right: bool, black_left: bool, black_right: bool) {
		if white_left {
			self.hash ^= ZOBRIST_NUMBERS[64 * 5 + 1];
		}
		if white_right {
			self.hash ^= ZOBRIST_NUMBERS[64 * 5 + 2];
		}
		if black_left {
			self.hash ^= ZOBRIST_NUMBERS[64 * 5 + 3];
		}
		if black_right {
			self.hash ^= ZOBRIST_NUMBERS[64 * 5 + 4];
		}
	}
}

const fn sq_to_idx(sq: Square) -> usize {
	sq.0 * 8 + sq.1
}

const fn idx_to_sq(idx: usize) -> Square {
	Square(idx / 8, idx % 8)
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Square (pub usize, pub usize);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Move (pub Square, pub Square);

impl Move {
	pub fn is_castle(mv: &Move) -> bool {
		mv.0 == Square(7, 4) && mv.1 == Square(7, 2) ||
		mv.0 == Square(7, 4) && mv.1 == Square(7, 6) ||
		mv.0 == Square(0, 4) && mv.1 == Square(0, 2) ||
		mv.0 == Square(0, 4) && mv.1 == Square(0, 6)
	}
}

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

#[derive(Clone)]
pub struct Bitboards {
	pub white_material: u64,
	pub black_material: u64,
	// pub white_pawns: u64,
	// pub black_pawns: u64,
	pub white_mobility: u64,
	pub black_mobility: u64,
}

impl fmt::Display for Bitboards {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "White Material: {:b},\nBlack Material: {:b},\nWhite Mobility: {:b},\nBlack Mobility: {:b}", self.white_material, self.black_material, self.white_mobility, self.black_mobility)
	}
}

impl Bitboards {
	pub fn new() -> Bitboards {
		Bitboards { white_material: 0, black_material: 0, white_mobility: 0, black_mobility: 0 }
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
	pub bitboards: Bitboards,
	pub zobrist_hash: ZobristHash,

	// Used for branching and backtracking
	pub move_count: usize,
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

	pub fn lazy_get_check_states(&self, game_state: &GameState) -> (bool, bool) {
		let mut black_king_mask = 0u64;
		let mut white_king_mask = 0u64;
		let mut black_attacks_mask = 0u64;
		let mut white_attacks_mask = 0u64;
		for x in 0..64 {
			let piece = new_state.board[x / 8][x % 8];
			if piece.color == Color::Black {
				match piece.role {
					Role::King => black_king_mask |= 1u64 << x,
					Role::Queen => black_attacks_mask |= ALL_ROOK_MASKS[x] | ALL_BISHOP_MASKS[x],
					Role::Rook => black_attacks_mask |= ALL_ROOK_MASKS[x],
					Role::Bishop => black_attacks_mask |= ALL_BISHOP_MASKS[x],
					Role::Knight => black_attacks_mask |= ALL_KNIGHT_MASKS[x],
					Role::Pawn => black_attacks_mask |= ALL_BLACK_PAWN_MASKS[x],
					_ => ()
				}
			} else if piece.color == Color::White {
				match piece.role {
					Role::King => white_king_mask |= 1u64 << x,
					Role::Queen => white_attacks_mask |= ALL_ROOK_MASKS[x] | ALL_BISHOP_MASKS[x],
					Role::Rook => white_attacks_mask |= ALL_ROOK_MASKS[x],
					Role::Bishop => white_attacks_mask |= ALL_BISHOP_MASKS[x],
					Role::Knight => white_attacks_mask |= ALL_KNIGHT_MASKS[x],
					Role::Pawn => white_attacks_mask |= ALL_WHITE_PAWN_MASKS[x],
					_ => ()
				}
			}
		}

		(white_king_mask & white_attacks_mask != 0, black_king_mask & black_attacks_mask != 0)
	};

	#[inline(never)]
	pub fn generate_new_state(&self, mv: &Move) -> GameState {
		// Create a new GameState with the move applied. Do not modify self.
		let from = mv.0;
		let to = mv.1;
		let mut new_board = self.board;

		// Move the piece to the new square
		new_board[to.0][to.1] = self.board[from.0][from.1];
		if (to.0 == 7 || to.0 == 0) && new_board[to.0][to.1].role == Role::Pawn {
			new_board[to.0][to.1].role = Role::Queen;
		}
		new_board[from.0][from.1] = Piece { role: Role::Blank, color: Color::Blank };

		let mut new_bitboards = self.bitboards.clone();
		if self.turn == Color::White {
			new_bitboards.white_material ^= (1u64 << sq_to_idx(from)) | (1u64 << sq_to_idx(to));
			new_bitboards.black_material &= !(1u64 << sq_to_idx(to));
		} else {
			new_bitboards.black_material ^= (1u64 << sq_to_idx(from)) | (1u64 << sq_to_idx(to));
			new_bitboards.white_material &= !(1u64 << sq_to_idx(to));
		}

		// If the move was a castle, also move the rook to the new square
		if Move::is_castle(mv) {
			if mv.1 == Square(7, 2) {
				new_board[7][3] = new_board[7][0];
				new_board[7][0] = Piece { role: Role::Blank, color: Color::Blank };
				new_bitboards.black_material ^= (1u64 << (7 * 8 + 3)) & (1u64 << (7 * 8));
			} else if mv.1 == Square(7, 6) {
				new_board[7][5] = new_board[7][7];
				new_board[7][7] = Piece { role: Role::Blank, color: Color::Blank };
				new_bitboards.black_material ^= (1u64 << (7 * 8 + 5)) & (1u64 << (7 * 8 + 7));
			} else if mv.1 == Square(0, 2) {
				new_board[0][3] = new_board[0][0];
				new_board[0][0] = Piece { role: Role::Blank, color: Color::Blank };
				new_bitboards.white_material ^= (1u64 << (0 * 8 + 3)) & (1u64 << (0 * 8));
			} else if mv.1 == Square(0, 6) {
				new_board[0][5] = new_board[0][7];
				new_board[0][7] = Piece { role: Role::Blank, color: Color::Blank };
				new_bitboards.white_material ^= (1u64 << 5) & (1u64 << 7);
			}
		}

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
			bitboards: new_bitboards,
			zobrist_hash: ZobristHash::new(),
			move_count: self.move_count + 1,
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


		// TODO: Can roll this into the check checks to avoid 2 passes
		new_state.bitboards.white_mobility = 0;
		for mv in &white_moves {
			new_state.bitboards.white_mobility |= 1u64 << sq_to_idx(mv.1);
		}

		new_state.bitboards.black_mobility = 0;
		for mv in &black_moves {
			new_state.bitboards.black_mobility |= 1u64 << sq_to_idx(mv.1);
		}

		new_state.white_check = white_check;
		new_state.black_check = black_check;
		new_state.move_list = Some(if new_state.turn == Color::White { white_moves } else { black_moves });

		let mut new_zobrist_hash = self.zobrist_hash.clone();
		new_zobrist_hash.update_move(self, &mv);
		new_zobrist_hash.update_turn();
		new_zobrist_hash.update_castle_rights( // Update only if these have changed
			white_castle_left == self.white_castle_left,
			white_castle_right == self.white_castle_right,
			black_castle_left == self.black_castle_left,
			black_castle_right == self.black_castle_right,
		);
		new_state.zobrist_hash = new_zobrist_hash;

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

		let mut game_state = GameState {
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
				bitboards: Bitboards {
					white_material: 0b11111111_11111111_00000000_00000000_00000000_00000000_00000000_00000000,
					black_material: 0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_11111111,
					white_mobility: 0b00000000_00000000_11111111_11111111_00000000_00000000_00000000_00000000,
					black_mobility: 0b00000000_00000000_00000000_00000000_11111111_11111111_00000000_00000000,
				},
				zobrist_hash: ZobristHash::new(),
				move_count: 0,
		};
		game_state.zobrist_hash = ZobristHash::from(&game_state);
		Game { state: Rc::new(game_state) }
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

#[inline(never)]
pub fn get_moves(sq: Square, game: &GameState) -> Vec<Move> {
	let piece = game.board[sq.0][sq.1];
	let mut valid_moves = vec![];
	match piece.role {
		Role::Queen => {
			valid_moves = get_moves_step_out(&sq, game, piece.color, &QUEEN_MOVE_PAIRS);
		}
		Role::King => {
			let mut king_mask = ALL_KING_MASKS[sq_to_idx(sq)];
			println!("King mask: {:b}", king_mask);
			king_mask &= if piece.color == Color::White {
				!game.bitboards.white_material
			} else {
				!game.bitboards.black_material
			};

			while king_mask != 0 {
				println!("King mask: {:b}", king_mask);
				let idx = king_mask.trailing_zeros() as usize;
				valid_moves.push(Move(sq, idx_to_sq(idx)));
				king_mask ^= 1u64 << idx;
			}

			match piece.color {
				Color::White => {
					if game.white_castle_left {
						if [(7,1), (7,2), (7,3)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(7, 4), Square(7, 2)));
							}
					}
					if game.white_castle_right {
						if [(7,6), (7,5)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(7, 4), Square(7, 6)));
							}
					}
				}
				Color::Black => {
					if game.black_castle_left {
						if [(0,1), (0,2), (0,3)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(0, 4), Square(0, 2)));
							}
					}
					if game.black_castle_right {
						if [(0,6), (0,5)]
							.into_iter()
							.all(|(x, y)| game.board[x][y].color == Color::Blank) {
								valid_moves.push(Move(Square(0, 4), Square(0, 6)));
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
			let mut knight_mask = ALL_KNIGHT_MASKS[sq_to_idx(sq)];
			knight_mask &= if piece.color == Color::White {
				!game.bitboards.white_material
			} else {
				!game.bitboards.black_material
			};

			let mut idx = 0;
			while knight_mask != 0 {
				if knight_mask & 1 != 0 {
					valid_moves.push(Move(sq, idx_to_sq(idx)));
				}
				knight_mask >>= 1;
				idx += 1;
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
					let mut king_mask = ALL_KING_MASKS[sq_to_idx(sq)];
					king_mask &= if piece.color == Color::White {
						!game.bitboards.white_material
					} else {
						!game.bitboards.black_material
					};

					while king_mask != 0 {
						let idx = king_mask.trailing_zeros() as usize;
						moves.push(Move(sq, idx_to_sq(idx)));
						king_mask &= king_mask - 1;
					}
		
					match piece.color {
						Color::White => {
							if game.white_castle_left {
								if [(7,1), (7,2), (7,3)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(7, 4), Square(7, 2)));
									}
							}
							if game.white_castle_right {
								if [(7,6), (7,5)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(7, 4), Square(7, 6)));
									}
							}
						}
						Color::Black => {
							if game.black_castle_left {
								if [(0,1), (0,2), (0,3)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(0, 4), Square(0, 2)));
									}
							}
							if game.black_castle_right {
								if [(0,6), (0,5)]
									.into_iter()
									.all(|(x, y)| game.board[x][y].color == Color::Blank) {
										moves.push(Move(Square(0, 4), Square(0, 6)));
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
					let mut knight_mask = ALL_KNIGHT_MASKS[sq_to_idx(sq)];
					knight_mask &= if piece.color == Color::White {
						!game.bitboards.white_material
					} else {
						!game.bitboards.black_material
					};

					let mut idx = 0;
					while knight_mask != 0 {
						if knight_mask & 1 != 0 {
							moves.push(Move(sq, idx_to_sq(idx)));
						}
						knight_mask >>= 1;
						idx += 1;
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
		println!("Bitboards: {}", board.bitboards);
		let knight_moves = get_moves(Square(0,1), &board);
		assert_eq!(
			knight_moves,
			vec![
				Move(Square(0, 1), Square(2, 0)),
				Move(Square(0, 1), Square(2, 2)),
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
		// For bitboard updates to work, we need to execute moves
		// on the correct color's turn.
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 1), Square(7,7))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 2), Square(7,6))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 3), Square(7,5))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 5), Square(7,4))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 6), Square(7,3))); // B
		let moves = get_moves(Square(0, 4), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(0, 4), Square(0, 3)),
				Move(Square(0, 4),Square(0, 5)),
				Move(Square(0, 4), Square(0, 2)),
				Move(Square(0, 4), Square(0, 6)),
			],
		);

		// Shuffle back and forth; castling should be invalidated
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 4), Square(0, 5))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 5), Square(0, 4))); // B
		let moves = get_moves(Square(0,4), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(0, 4),Square(0, 3)),
				Move(Square(0, 4), Square(0, 5)),
			],
		);

	}

	#[test]
	fn test_castle() {
        let mut board = Game::new();
		let moves = get_moves(Square(0,4), &board);
		assert_eq!(moves, vec![]);

		// Move everything out of the way to test castling
		// For bitboard updates to work, we need to execute moves
		// on the correct color's turn.
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 1), Square(7,7))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 2), Square(7,6))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 3), Square(7,5))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 5), Square(7,4))); // B
		board.make_move(Move(Square(7, 7), Square(7,7))); // W
		board.make_move(Move(Square(0, 6), Square(7,3))); // B
		let moves = get_moves(Square(0, 4), &board);
		assert_eq!(
			moves,
			vec![
				Move(Square(0, 4), Square(0, 3)),
				Move(Square(0, 4),Square(0, 5)),
				Move(Square(0, 4), Square(0, 2)),
				Move(Square(0, 4), Square(0, 6)),
			],
		);

		board.make_move(Move(Square(0, 4), Square(0, 2)));
		assert_eq!(
			board.board[0][4].role,
			Role::Blank,
		);
		assert_eq!(
			board.board[0][2].role,
			Role::King,
		);
		assert_eq!(
			board.board[0][3].role,
			Role::Rook,
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
