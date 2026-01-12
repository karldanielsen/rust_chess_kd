use crate::game;
use crate::transposition_tables::{TranspositionTable};
use std::cmp::Ordering;

pub struct Bot {
	max_depth: u32,
	mobility_weight: f32,
	square_control_weights: [[f32; 8]; 8],
	castle_bonus: f32,
	can_castle_bonus: f32,
	piece_weights: [f32; 6],
	attack_weights: [f32; 6],
	check_weight: f32,
	pawn_advance_weights: [f32; 6],
}

impl Bot {
	pub fn new(max_depth: u32, mobility_weight: f32, square_control_weights: [[f32; 8]; 8], castle_bonus: f32, can_castle_bonus: f32, piece_weights: [f32; 6], attack_weights: [f32; 6], check_weight: f32, pawn_advance_weights: [f32; 6]) -> Bot {
		Bot { max_depth, mobility_weight, square_control_weights, castle_bonus, can_castle_bonus, piece_weights, attack_weights, check_weight, pawn_advance_weights }
	}
}
// Hardcode a max depth for now, to avoid multithreading + waiting
pub const DEFAULT_MAX_DEPTH: u32 = 3;

pub const DEFAULT_MOBILITY_WEIGHT: f32 = 0.05;
pub const DEFAULT_SQUARE_CONTROL_WEIGHTS: [[f32; 8]; 8] = [
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
	[0.0, 0.0, 0.4, 0.4, 0.4, 0.4, 0.0, 0.0],
	[0.0, 0.0, 0.4, 0.8, 0.8, 0.4, 0.0, 0.0],
	[0.0, 0.0, 0.4, 0.8, 0.8, 0.4, 0.0, 0.0],
	[0.0, 0.0, 0.4, 0.4, 0.4, 0.4, 0.0, 0.0],
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
	[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
];

// Bonus for castling specifically-- since king mobility is _good_ but castling is _better_
pub const DEFAULT_CASTLE_BONUS: f32 = 10.0;

// Bonus for maintaining the ability to castle
pub const DEFAULT_CAN_CASTLE_BONUS: f32 = 1.0;

// Piece weights array indexed by Role enum: [Queen, King, Rook, Bishop, Knight, Pawn]
pub const DEFAULT_PIECE_WEIGHTS: [f32; 6] = [8.0, 15.0, 5.0, 3.1, 2.9, 1.0];

// Attack weights array indexed by Role enum: [Queen, King, Rook, Bishop, Knight, Pawn]
pub const DEFAULT_ATTACK_WEIGHTS: [f32; 6] = [4.0, 4.0, 2.5, 1.5, 1.5, 0.5];
pub const DEFAULT_PAWN_ADVANCE_WEIGHTS: [f32; 6] = [0.0, 0.0, 0.0, 0.1, 0.3, 0.5];

pub fn default_piece_weight(role: game::Role) -> f32 {
	match role {
		game::Role::Pawn => 1.0,
		game::Role::Bishop => 3.1,
		game::Role::Knight => 2.9,
		game::Role::King => 15.0,
		game::Role::Queen => 8.0,
		game::Role::Rook => 5.0,
		game::Role::Blank => 0.0,
	}
}

pub fn default_attack_weight(role: game::Role) -> f32 {
	match role {
		game::Role::Pawn => 0.5,
		game::Role::Bishop => 1.0,
		game::Role::Knight => 1.0,
		game::Role::King => 2.5, // Check weight adds to this
		game::Role::Queen => 2.0,
		game::Role::Rook => 1.5,
		game::Role::Blank => 0.0,
	}
}

// TODO:
// - Honestly a transposition table will not speed this up enough, even with iterative deepening.
//   Most likely we just need boring old game state simplification and move calculation optimization.
//   - Remove Move dependency, just pass around bitboards.
//   - Make game state static, and store a history of reversible moves (move + check states + maybe piece taken)
//   - Maybe don't look for check or checkmate at the bottom iterations?
// - For Transposition Table to do anything, we need to run and cache at greater depth.
//   Even depth 2 should be much better, since it will account for move ordering.
//   This also requires us to build a system to make in-progress evaluations portable.
//   We'll have to pick up and resume evaluations from where we left off.
// - Once speed to ~depth 6 is good, implement tournament tuning of weights
impl Bot {
	pub fn evaluate_position(&self, game: &mut game::Game, transposition_tables: &mut [TranspositionTable; 2]) -> (game::Move, f32) {
		let (mv, score) = self.minimax_eval(game, 0, f32::MIN, f32::MAX, transposition_tables);
		(mv, score)
	}

	pub fn minimax_eval(&self, game: &mut game::Game, depth: u32, mut floor: f32, mut ceil: f32, transposition_tables: &mut [TranspositionTable; 2]) -> (game::Move, f32) {
		let mut moves = game.get_move_list();

		if depth == 0 {
			let is_white_turn = game.get_turn() == game::Color::White;
			let mut moves_with_scores = moves.iter().map(|mv| {
				game.make_move(*mv);
				let score = transposition_tables[if game.get_turn() == game::Color::White { 1 } else { 0 }].get(game);
				game.reverse();
				(mv, score)
			}).collect::<Vec<(&game::Move, Option<f32>)>>();

			moves_with_scores.sort_unstable_by(|(_, a_score), (_, b_score)| {
				match (a_score, b_score) {
					(Some(a), Some(b)) => {
						if is_white_turn {
							b.partial_cmp(a).unwrap_or(Ordering::Equal)
						} else {
							a.partial_cmp(b).unwrap_or(Ordering::Equal)
						}
					}
					(Some(_), None) => {
						Ordering::Less
					}
					(None, Some(_)) => {
						Ordering::Greater
					}
					(None, None) => {
						Ordering::Equal
					}
				}
			});

			moves = moves_with_scores.into_iter().map(|(mv, _)| *mv).collect::<Vec<game::Move>>();
		}

		let mut best_move = game::Move(game::Square(0,0), game::Square(0,0));
		let mut best_score = if game.get_turn() == game::Color::White { f32::MIN } else { f32::MAX };
		for mv in moves {
			game.make_move(mv);
			match game::get_other_color(game.turn) {
				game::Color::White => if game.white_check {
					game.reverse();
					continue;
				}
				_ => if game.black_check {
					game.reverse();
					continue;
				}
			}
			if game.checkmate {
				game.reverse();
				// Checkmate is always bad for the player whose turn it is
				let checkmate_score = if game.get_turn() == game::Color::White { 100000.0 } else { -100000.0 };
				return (mv, checkmate_score);
			}

			let score = if self.check_for_repetition(game) {
				0.0
			} else if depth == 0 {
				let (_, base_score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_tables);
				let score = base_score + self.do_castle_bonus(game, &mv);
				score
			} else if depth < self.max_depth - 1 {
				if depth == 1 {
					let (_, score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_tables);
					transposition_tables[if game.get_turn() == game::Color::White { 1 } else { 0 }].set(game, score);
					score
				} else {
					let (_, score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_tables);
					score
				}
			} else { // At max depth, evaluate the position.
				self.eval(game)
			};

			game.reverse(); 

			// Alpha-beta pruning logic
			if game.get_turn() == game::Color::White {
				// White is maximizing - update alpha (floor) and best move
				if score > best_score {
					best_score = score;
					best_move = mv;
				}
				// Always update alpha (floor) - it's the best score we've found so far
				floor = if score > floor { score } else { floor };
				// Beta cutoff: if alpha >= beta, opponent won't allow this branch
				if floor >= ceil {
					break; // Prune remaining moves
				}
			} else {
				// Black is minimizing - update beta (ceil) and best move
				if score < best_score {
					best_score = score;
					best_move = mv;
				}
				// Always update beta (ceil) - it's the best score we've found so far
				ceil = if score < ceil { score } else { ceil };
				// Alpha cutoff: if beta <= alpha, opponent won't allow this branch
				if ceil <= floor {
					break; // Prune remaining moves
				}
			}
		}

		// If no move resulted in a score or checkmate, this is a draw.
		if best_move == game::Move(game::Square(0,0), game::Square(0,0)) {
			return (best_move, 0.0);
		}

		return (best_move, best_score);
	}

	pub fn check_for_repetition(&self, game: &game::Game) -> bool {
		if game.since_last_non_reversible_move == 50 {
			return true;
		}

		let original_game_state = &game.state;

		// In theory this can be optimized by only checking every other move,
		// but it's not worth the complexity for now.
		let mut look_back_to = game.since_last_non_reversible_move as i16;
		let mut current_game_state = &game.state; // &Rc<GameState>
		while let Some(next_game_state) = &current_game_state.parent { // Option<Rc<GameState>>
			if look_back_to < 0 {
				return false;
			}

			if *next_game_state == *original_game_state {
				return true;
			}

			current_game_state = next_game_state;
			look_back_to -= 1;
		}
		return false;
	}

	// To generate an evaluation score from White's perspective
	// Positive = good for White, Negative = good for Black
	fn eval(&self, game_state: &game::GameState) -> f32 {
		// Calculate White's advantages (positive)
		let white_material = self.material_score(game_state, game::Color::White);
		let white_attack = self.attack_score(game_state, moves, game::Color::White);
		let white_control = self.control_score(game_state, game::Color::White);
		let white_check = self.check_score(game_state, game::Color::White);
		let white_mobility = self.mobility_score(game_state, game::Color::White);
		let white_can_castle_bonus = self.can_castle_bonus(game_state, game::Color::White);
		let white_pawn_advance = self.pawn_advance_score(game_state, game::Color::White);

		// Calculate Black's advantages (negative, since we're from White's perspective)
		let black_material = self.material_score(game_state, game::Color::Black);
		let black_attack = self.attack_score(game_state, moves, game::Color::Black);
		let black_control = self.control_score(game_state, game::Color::Black);
		let black_check = self.check_score(game_state, game::Color::Black);
		let black_mobility = self.mobility_score(game_state, game::Color::Black);
		let black_can_castle_bonus = self.can_castle_bonus(game_state, game::Color::Black);
		let black_pawn_advance = self.pawn_advance_score(game_state, game::Color::Black);
	
		(white_material + white_attack + white_control + white_can_castle_bonus + white_check + white_mobility + white_pawn_advance) -
		(black_material + black_attack + black_control + black_can_castle_bonus + black_check + black_mobility + black_pawn_advance)
	}

	fn mobility_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		self.mobility_weight * (
			if color == game::Color::White {
				game_state.bitboards.white_mobility.count_ones() as f32
			} else {
				game_state.bitboards.black_mobility.count_ones() as f32
			}
		)
	}

	fn material_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		let mut score = 0.0;
		for x in 0..8 {
			for y in 0..8 {
				if game_state.board[x][y].color == color {
					let role = game_state.board[x][y].role;
					score += self.piece_weights[role as usize];
				}
			}
		}
		score
	}

	// TODO: Not weighted per-piece
	fn attack_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		self.attack_weights[0] * if color == game::Color::White {
			(game_state.bitboards.white_material & game_state.bitboards.black_mobility).count_ones() as f32
		} else {
			(game_state.bitboards.black_material & game_state.bitboards.white_mobility).count_ones() as f32
		}
	}

	// Center control is only valued on pawns-- for other pieces just mobility is enough
	fn control_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		let mut score = 0.0;
		for x in 2..6 {
			for y in 2..6 {
				if game_state.board[x][y].color == color && game_state.board[x][y].role == game::Role::Pawn {
					score += self.square_control_weights[x][y];
				}
			}
		}
		score
	}

	fn check_score(&self, game_state: &game::GameState, turn: game::Color) -> f32 {
		match turn {
			game::Color::White => if game_state.white_check { self.check_weight } else { 0.0 },
			game::Color::Black => if game_state.black_check { self.check_weight } else { 0.0 },
			game::Color::Blank => 0.0,
		}
	}
	fn can_castle_bonus(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		match color {
			game::Color::White => (if game_state.white_castle_left { self.can_castle_bonus } else { 0.0 }) + (if game_state.white_castle_right { self.can_castle_bonus } else { 0.0 }),
			game::Color::Black => (if game_state.black_castle_left { self.can_castle_bonus } else { 0.0 }) + (if game_state.black_castle_right { self.can_castle_bonus } else { 0.0 }),
			game::Color::Blank => 0.0,
		}
	}
	fn do_castle_bonus(&self, game_state: &game::GameState, mv: &game::Move) -> f32 {
		let bonus = match mv {
			game::Move(game::Square(7, 4), game::Square(7, 2)) => if game_state.board[7][2].role == game::Role::King { self.castle_bonus } else { 0.0 },
			game::Move(game::Square(7, 4), game::Square(7, 6)) => if game_state.board[7][6].role == game::Role::King { self.castle_bonus } else { 0.0 },
			game::Move(game::Square(0, 4), game::Square(0, 2)) => if game_state.board[0][2].role == game::Role::King { self.castle_bonus } else { 0.0 },
			game::Move(game::Square(0, 4), game::Square(0, 6)) => if game_state.board[0][6].role == game::Role::King { self.castle_bonus } else { 0.0 },
			_ => 0.0,
		};
		return bonus;
	}

	fn pawn_advance_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		let mut score = 0.0;
		for x in 1..7 {
			for y in 0..8 {
				if game_state.board[x][y].color == color && game_state.board[x][y].role == game::Role::Pawn {
					let score_idx = if color == game::Color::White { 5 - (x - 1) as usize } else { (x - 1) as usize };
					score += self.pawn_advance_weights[score_idx];
				}
			}
		}
		score
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_find_checkmate() {
		let row_eight = [
			game::Piece { role: game::Role::Rook, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Knight, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::King, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
		];
		let row_seven = [
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
			game::Piece { role: game::Role::Queen, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
		];
		let row_six = [
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::Black },
			game::Piece { role: game::Role::Bishop, color: game::Color::White },
		];
		let row_five = [
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
		];
		let row_four = [
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Knight, color: game::Color::White },
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
			game::Piece { role: game::Role::Queen, color: game::Color::White },
			game::Piece { role: game::Role::Bishop, color: game::Color::Black },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
		];
		let row_three = [
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
		];
		let row_two = [
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Bishop, color: game::Color::White },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Pawn, color: game::Color::White },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
		];
		let row_one = [
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Rook, color: game::Color::White },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::Blank, color: game::Color::Blank },
			game::Piece { role: game::Role::King, color: game::Color::White },
		];
		let mut starting_game_state = game::GameState {
			bitboards: game::Bitboards::new(),
			board: [
				row_eight,
				row_seven,
				row_six,
				row_five,
				row_four,
				row_three,
				row_two,
				row_one,
			],
			turn: game::Color::Black,
			white_castle_left: false,
			white_castle_right: false,
			black_castle_left: false,
			black_castle_right: false,
			white_check: false,
			black_check: false,
			checkmate: false,
			move_list: OnceCell::new(), // Max possible moves in chess
			parent: None,
			since_last_non_reversible_move: 0,
			move_count: 0,
			zobrist_hash: game::ZobristHash::new(),
		};
		starting_game_state.bitboards = game::Bitboards::from(&starting_game_state);
		starting_game_state.move_list = Some(game::get_all_valid_moves_fast(&starting_game_state, Some(game::Color::Black)));

		let mut game = game::Game::from(starting_game_state);
		let bot = Bot::new(3, 0.1, DEFAULT_SQUARE_CONTROL_WEIGHTS, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		let (mv, score) = bot.evaluate_position(&mut game, &mut [TranspositionTable::new(), TranspositionTable::new()]);
		assert_eq!(mv, game::Move(game::Square(1, 3), game::Square(5, 7)));
		assert_eq!(score, -100000.0);
	}

	#[test]
	fn test_check_for_repetition() {
		let mut game = game::Game::new();
		let bot = Bot::new(3, 0.1, DEFAULT_SQUARE_CONTROL_WEIGHTS, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		game.make_move(game::Move(game::Square(7, 1), game::Square(3, 3)));
		assert_eq!(bot.check_for_repetition(&game), false);

		game.make_move(game::Move(game::Square(0, 1), game::Square(4, 4)));
		assert_eq!(bot.check_for_repetition(&game), false);

		game.make_move(game::Move(game::Square(3, 3), game::Square(7, 1)));
		assert_eq!(bot.check_for_repetition(&game), false);

		game.make_move(game::Move(game::Square(4, 4), game::Square(0, 1)));
		assert_eq!(bot.check_for_repetition(&game), true);
	}
}