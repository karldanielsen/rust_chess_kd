use crate::game;
use crate::transposition_tables::{TranspositionTable, EntryType};
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone)]
pub struct Bot {
	max_depth: u32,
	pub mobility_weight: f32,
	pub center_control_weight: f32,
	pub castle_bonus: f32,
	pub can_castle_bonus: f32,
	pub piece_weights: [f32; 6],
	pub attack_weights: [f32; 6],
	pub check_weight: f32,
	pub pawn_advance_weights: [f32; 6],
}

impl Bot {
	pub fn new(max_depth: u32, mobility_weight: f32, center_control_weight: f32, castle_bonus: f32, can_castle_bonus: f32, piece_weights: [f32; 6], attack_weights: [f32; 6], check_weight: f32, pawn_advance_weights: [f32; 6]) -> Bot {
		Bot { 
			max_depth, 
			mobility_weight, 
			center_control_weight, 
			castle_bonus, 
			can_castle_bonus, 
			piece_weights, 
			attack_weights, 
			check_weight, 
			pawn_advance_weights,
		}
	}
}

impl fmt::Display for Bot {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "Mobility: {:.3}, Center Control: {:.3}, Castle Bonus: {:.3}, Can Castle Bonus: {:.3}, Check Weight: {:.3}", 
			self.mobility_weight, 
			self.center_control_weight, 
			self.castle_bonus, 
			self.can_castle_bonus, 
			self.check_weight)?;
		write!(f, "  Piece Weights: {:?}, Attack Weights: {:?}, Pawn Advance Weights: {:?}", 
			self.piece_weights, 
			self.attack_weights, 
			self.pawn_advance_weights)
	}
}

pub const DEFAULT_MOBILITY_WEIGHT: f32 = 0.05;
pub const CENTER_CONTROL_MASK: u64 = 0b00000000_00000000_00000000_11111111_11111111_00000000_00000000_00000000;
pub const CENTER_CONTROL_WEIGHT: f32 = 0.4;

// Row masks for rows 1-6 (excluding row 0 and 7) - used for pawn advance scoring
const ROW_MASKS: [u64; 6] = [
	0b11111111u64 << 8,   // Row 1 (x=1)
	0b11111111u64 << 16,   // Row 2 (x=2)
	0b11111111u64 << 24,   // Row 3 (x=3)
	0b11111111u64 << 32,   // Row 4 (x=4)
	0b11111111u64 << 40,   // Row 5 (x=5)
	0b11111111u64 << 48,   // Row 6 (x=6)
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
// - Fix the Lobotomization caused by TT tables
// - Iterative Deepening :)
// - Aggregate a list of "acceptable" moves the calling fn can pick from
// - Add Static Exchange Evaluation
// - Add Quiescence Search
//   - I don't think this requires us to implement iterative deepening/branch caching.
// - Most likely we just need boring old game state simplification and move calculation optimization.
//   - Make game state static, and store a history of reversible moves (move + check states + maybe piece taken)

impl Bot {
	pub fn evaluate_position(&self, game: &mut game::Game, transposition_table: &mut TranspositionTable) -> (game::Move, f32) {
		self.minimax_eval(game, 0, f32::MIN, f32::MAX, transposition_table)
	}

	pub fn minimax_eval(&self, game: &mut game::Game, depth: u32, mut floor: f32, mut ceil: f32, transposition_table: &mut TranspositionTable) -> (game::Move, f32) {
		let original_floor = floor;
		let original_ceil = ceil;
		
		// Get move count without holding a reference
		let move_count = {
			let moves_ref = game.get_move_list();
			moves_ref.len()
		};
		
		// For move ordering at depth 0, score each move by getting it individually
		let move_indices: Vec<usize> = if depth == 0 {
			let is_white_turn = game.get_turn() == game::Color::White;
			let mut moves_with_scores: Vec<(usize, Option<f32>, f32)> = (0..move_count).map(|idx| {
				// Get move at this index, use it, then drop reference before mutation
				let mv = {
					let moves_ref = game.get_move_list();
					moves_ref[idx]
				};

				let is_capture =
					game.bitboards.white_material & (1u64 << (mv.0.0 * 8 + mv.0.1)) != 0u64 ||
					game.bitboards.black_material & (1u64 << (mv.0.0 * 8 + mv.0.1)) != 0u64;

				// MVV - LVA Scoring, Most valuable victim, least valuable attacker
				let capture_score = if is_capture {
					let to_piece = game.board[mv.1.0][mv.1.1];
					let from_piece = game.board[mv.0.0][mv.0.1];
					let to_piece_value = default_piece_weight(to_piece.role);
					let from_piece_value = default_piece_weight(from_piece.role);
					to_piece_value * 20.0 - from_piece_value
				} else { 0.0 };
				game.make_move(mv);
				let score = transposition_table.get(game);
				game.reverse();
				(idx, score, capture_score)
			}).collect();

			moves_with_scores.sort_unstable_by(|(_, a_score, a_capture_score), (_, b_score, b_capture_score)| {
				// // If move involves a capture, sort by MVV - LVA score
				if *a_capture_score > 0.0 || *b_capture_score > 0.0 {
					return a_capture_score.partial_cmp(b_capture_score).unwrap_or(Ordering::Equal);
				}

				// Else, sort by previous evaluation
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

			moves_with_scores.into_iter().map(|(idx, _, _)| idx).collect()
		} else {
			(0..move_count).collect()
		};

		let mut tp_table_entry_type = EntryType::Exact;
		let mut best_move = game::Move(game::Square(0,0), game::Square(0,0));
		let mut best_score = if game.get_turn() == game::Color::White { f32::MIN } else { f32::MAX };

		// TODO: Can I just rip rayon here? GameState is thread safe,
		// we'd have to clone the "Game -> GameState" Rc, but I think
		// that's fine :thinking:
		//
		// OH transposition tables are shared. We'd need a thread-specific one.
		// Or a shared one behind a Mutex. Hmmmmm.
		for idx in move_indices {
			// Get move at this index, use it, then drop reference before mutation
			let mv = {
				let moves_ref = game.get_move_list();
				moves_ref[idx]
			};
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
				let tt_entry = transposition_table.get_depth(game, (self.max_depth - depth - 1) as usize);
				if let Some((tp_score, tp_entry_type)) = tt_entry {
					match tp_entry_type {
						EntryType::Exact => tp_score,
						EntryType::Lowerbound => {
							floor = f32::max(floor, tp_score);
							let (_, base_score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
							base_score + self.do_castle_bonus(game, &mv)
						},
						EntryType::Upperbound => {
							ceil = f32::min(ceil, tp_score);
							let (_, base_score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
							base_score + self.do_castle_bonus(game, &mv)
						},
					}
				} else {
					let (_, base_score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
					base_score + self.do_castle_bonus(game, &mv)
				}
			} else if depth < self.max_depth - 1 {
				let tt_entry = transposition_table.get_depth(game, (self.max_depth - depth - 1) as usize);
				if let Some((tp_score, tp_entry_type)) = tt_entry {
					match tp_entry_type {
						EntryType::Exact => tp_score,
						EntryType::Lowerbound => {
							floor = f32::max(floor, tp_score);
							let (_, score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
							score
						},
						EntryType::Upperbound => {
							ceil = f32::min(ceil, tp_score);
							let (_, score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
							score
						},
					}
				} else {
					let (_, score) = self.minimax_eval(game, depth + 1, floor, ceil, transposition_table);
					score
				}
			} else { // At max depth, evaluate the position.
				let tt_entry = transposition_table.get_depth(game, 0);
				if let Some((tp_score, tp_entry_type)) = tt_entry {
					match tp_entry_type {
						EntryType::Exact => tp_score,
						EntryType::Lowerbound => {
							floor = f32::max(floor, tp_score);
							self.eval(&game.state)
						},
						EntryType::Upperbound => {
							ceil = f32::min(ceil, tp_score);
							self.eval(&game.state)
						},
					}
				} else {
					self.eval(&game.state)
				}
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
					// There could be a better move, but in finding a refutation we at least set a lowerbound.
					tp_table_entry_type = EntryType::Lowerbound;
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
					// There could be a worse move, but in finding a refutation we at least set an upperbound.
					tp_table_entry_type = EntryType::Upperbound;
					break; // Prune remaining moves
				}
			}
		}

		// If no move resulted in a score or checkmate, this is a draw.
		if best_move == game::Move(game::Square(0,0), game::Square(0,0)) {
			if depth == 0 {
				println!("No move resulted in a score or checkmate at depth 0\n{:?}", game.state);
			}
			return (best_move, 0.0);
		}

		transposition_table.set(game, best_score, (self.max_depth - depth) as usize, tp_table_entry_type);

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
		let white_attack = self.attack_score(game_state, game::Color::White);
		let white_control = self.control_score(game_state, game::Color::White);
		let white_check = self.check_score(game_state, game::Color::White);
		let white_mobility = self.mobility_score(game_state, game::Color::White);
		let white_can_castle_bonus = self.can_castle_bonus(game_state, game::Color::White);
		let white_pawn_advance = self.pawn_advance_score(game_state, game::Color::White);

		// Calculate Black's advantages (negative, since we're from White's perspective)
		let black_material = self.material_score(game_state, game::Color::Black);
		let black_attack = self.attack_score(game_state, game::Color::Black);
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
		let mut pieces_bitboard = if color == game::Color::White {
			game_state.bitboards.white_material
		} else {
			game_state.bitboards.black_material
		};

		while pieces_bitboard != 0 {
			let piece_idx = pieces_bitboard.trailing_zeros() as usize;
			let piece = game_state.board[piece_idx / 8][piece_idx % 8];
			if piece.color == color && piece.role != game::Role::Blank {
				score += self.piece_weights[piece.role as usize];
			}
			pieces_bitboard ^= 1u64 << piece_idx;
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
		if color == game::Color::White {
			(game_state.bitboards.white_pawns & CENTER_CONTROL_MASK).count_ones() as f32 * CENTER_CONTROL_WEIGHT
		} else {
			(game_state.bitboards.black_pawns & CENTER_CONTROL_MASK).count_ones() as f32 * CENTER_CONTROL_WEIGHT
		}
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
		let pawns_bitboard = if color == game::Color::White {
			game_state.bitboards.white_pawns
		} else {
			game_state.bitboards.black_pawns
		};

		let mut score = 0.0;
		if color == game::Color::White {
			// For white: rows 1-6 map to weights[5] down to weights[0]
			for row in 0..6 {
				let row_mask = ROW_MASKS[row];
				let pawns_in_row = (pawns_bitboard & row_mask).count_ones() as f32;
				let weight_idx = 5 - row;
				score += pawns_in_row * self.pawn_advance_weights[weight_idx];
			}
		} else {
			// For black: rows 1-6 map to weights[0] up to weights[5]
			for row in 0..6 {
				let row_mask = ROW_MASKS[row];
				let pawns_in_row = (pawns_bitboard & row_mask).count_ones() as f32;
				score += pawns_in_row * self.pawn_advance_weights[row];
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
		let mut bot = Bot::new(3, 0.1, CENTER_CONTROL_WEIGHT, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		let (mv, score) = bot.evaluate_position(&mut game, &mut [TranspositionTable::new(), TranspositionTable::new()]);
		assert_eq!(mv, game::Move(game::Square(1, 3), game::Square(5, 7)));
		assert_eq!(score, -100000.0);
	}

	#[test]
	fn test_check_for_repetition() {
		let mut game = game::Game::new();
		let mut bot = Bot::new(3, 0.1, CENTER_CONTROL_WEIGHT, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
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