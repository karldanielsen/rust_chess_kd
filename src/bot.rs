use crate::game;
use crate::transposition_tables::{TranspositionTable, EntryType};
use crate::constants;
use std::cmp::Ordering;
use std::fmt;
use once_cell::unsync::OnceCell;
use std::time::Duration;
use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};
use std::sync::{Arc, mpsc};
use std::thread;

#[cfg(target_arch = "wasm32")]
use web_sys::window;

#[cfg(target_arch = "wasm32")]
fn get_time_now() -> f64 {
    window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
}

#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

#[cfg(not(target_arch = "wasm32"))]
fn get_time_now() -> f64 {
    Instant::now().elapsed().as_secs_f64() * 1000.0 // Convert to milliseconds to match performance.now()
}

// Index is depth, left is total prunable moves (move_count - 1), right is # of moves actually checked before pruning
pub static mut PRUNE_COUNTS: [(usize, usize); 30] = [(0, 0); 30];
// // TP table hit rate: (hits, total)
pub static mut TP_TABLE_HIT_RATE: (usize, usize) = (0, 0);
// // Debug: track set/get operations
pub static mut TP_TABLE_SETS: usize = 0;
pub static mut TP_TABLE_GETS: usize = 0;

pub enum AsyncEvaluation {
	Eval(game::Move, f32),
	Cancel,
}

#[derive(Clone)]
pub struct Bot {
	max_depth: u32,
	pub mobility_weight: f32,
	pub center_control_weight: f32,
	pub castle_bonus: f32,
	pub can_castle_bonus: f32,
	pub piece_weights: [f32; 6],
	pub attack_weights: [[f32; 6]; 6],
	pub check_weight: f32,
	pub pawn_advance_weights: [f32; 6],
}

impl Bot {
	pub fn new(max_depth: u32, mobility_weight: f32, center_control_weight: f32, castle_bonus: f32, can_castle_bonus: f32, piece_weights: [f32; 6], attack_weights: [[f32; 6]; 6], check_weight: f32, pawn_advance_weights: [f32; 6]) -> Bot {
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
	0b11111111u64 << 8,    // Row 1 (x=1)
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

// Attack weights array: attack_weights[attacking_piece][attacked_piece]
// Piece types: [Queen=0, King=1, Rook=2, Bishop=3, Knight=4, Pawn=5]
pub const DEFAULT_ATTACK_WEIGHTS: [[f32; 6]; 6] = [
	[4.0, 4.0, 2.5, 1.5, 1.5, 0.5], // Queen attacking
	[4.0, 4.0, 2.5, 1.5, 1.5, 0.5], // King attacking
	[2.5, 2.5, 2.0, 1.2, 1.2, 0.4], // Rook attacking
	[1.5, 1.5, 1.2, 1.0, 1.0, 0.3], // Bishop attacking
	[1.5, 1.5, 1.2, 1.0, 1.0, 0.3], // Knight attacking
	[0.5, 0.5, 0.4, 0.3, 0.3, 0.1], // Pawn attacking
];
pub const DEFAULT_PAWN_ADVANCE_WEIGHTS: [f32; 6] = [0.0, 0.0, 0.0, 0.1, 0.3, 0.5];

// TODO:
// - Iterative Deepening
// - Aggregate a list of "acceptable" moves the calling fn can pick from
// - Add Static Exchange Evaluation
// - Add Quiescence Search
//   - I don't think this requires us to implement iterative deepening/branch caching.
// - Most likely we just need boring old game state simplification and move calculation optimization.
//   - Make game state static, and store a history of reversible moves (move + check states + maybe piece taken)

const TRANSPOSITION_TABLE_DEPTH: u32 = 4;
impl Bot {

	// Synchronous version for WebAssembly - runs for approximately time_limit_seconds
	// Optionally calls a callback after each depth with the current depth
	pub fn evaluate_position_with_time_limit_sync<F>(&self, game: &mut game::Game, transposition_table: &mut TranspositionTable, time_limit_seconds: u64, mut depth_callback: Option<&mut F>) -> (game::Move, f32, u32) 
	where
		F: FnMut(u32),
	{
		let time_limit_ms = time_limit_seconds * 1000;
		let start_time_ms = get_time_now();
		let cancel_flag = AtomicBool::new(false);
		
		let mut best_move = game::Move(game::Square(0,0), game::Square(0,0));
		let mut best_score = f32::MIN;
		let mut depth = 1;
		let mut max_depth_reached = 0;

		// Iterative deepening with time limit
		loop {
			// Check if time limit exceeded before starting next depth
			let elapsed_ms = get_time_now() - start_time_ms;
			if elapsed_ms >= time_limit_ms as f64 {
				cancel_flag.store(true, AtomicOrdering::Relaxed);
				break;
			}

			match self.minimax_eval(game, depth, f32::MIN, f32::MAX, transposition_table, self.max_depth, Some(&cancel_flag)) {
				AsyncEvaluation::Eval(mv, score) => {
					best_move = mv;
					best_score = score;
					max_depth_reached = depth;
					
					// Call callback to update UI with current depth
					if let Some(ref mut cb) = depth_callback {
						cb(depth);
					}
					
					// Check time after getting a result
					let elapsed_ms = get_time_now() - start_time_ms;
					if elapsed_ms >= time_limit_ms as f64 {
						cancel_flag.store(true, AtomicOrdering::Relaxed);
						break;
					}
				}
				AsyncEvaluation::Cancel => {
					// Time limit exceeded during evaluation
					break;
				}
			}

			depth += 1;
			if depth > self.max_depth {
				break;
			}
		}

		(best_move, best_score, max_depth_reached)
	}

	pub fn minimax_eval(&self, game: &mut game::Game, depth: u32, mut floor: f32, mut ceil: f32, transposition_table: &mut TranspositionTable, max_depth: u32, cancel_flag: Option<&AtomicBool>) -> AsyncEvaluation {
		if depth >= TRANSPOSITION_TABLE_DEPTH {
			if let Some((score, entry_type, stored_depth, best_move)) = transposition_table.get_depth(game, depth as usize) {
				match entry_type {
					EntryType::Exact => return AsyncEvaluation::Eval(best_move, score),
					EntryType::Lowerbound => floor = score,
					EntryType::Upperbound => ceil = score,
				}
			}
		}

		let original_floor = floor;
		let original_ceil = ceil;
		
		// Get move count without holding a reference
		let move_count = {
			let moves_ref = game.get_move_list();
			moves_ref.len()
		};
		
		// For move ordering in higher depths, score each move by getting it individually
		let move_indices: Vec<usize> = if depth > TRANSPOSITION_TABLE_DEPTH + 1 {
			// Check cancel flag at the start of higher depth searches
			if let Some(flag) = cancel_flag {
				if flag.load(AtomicOrdering::Relaxed) {
					return AsyncEvaluation::Cancel;
				}
			}
		
			let is_white_turn = game.get_turn() == game::Color::White;
			let mut moves_with_scores: Vec<(usize, Option<f32>, f32)> = (0..move_count).map(|idx| {
				// Get move at this index, use it, then drop reference before mutation
				let mv = {
					let moves_ref = game.get_move_list();
					moves_ref[idx]
				};

				let is_capture =
					game.bitboards.white_material & (1u64 << (mv.1.0 * 8 + mv.1.1)) != 0u64 ||
					game.bitboards.black_material & (1u64 << (mv.1.0 * 8 + mv.1.1)) != 0u64;

				// MVV - LVA Scoring, Most valuable victim, least valuable attacker
				if is_capture {
					let to_piece = game.board[mv.1.0][mv.1.1];
					let from_piece = game.board[mv.0.0][mv.0.1];
					if to_piece.role != game::Role::Blank {
						let to_piece_value = self.piece_weights[to_piece.role as usize];
						let from_piece_value = self.piece_weights[from_piece.role as usize];
						return (idx, None, to_piece_value * 20.0 - from_piece_value);
					}
				};
				game.make_move(mv);

				unsafe { 
					TP_TABLE_HIT_RATE.1 += 1;
					TP_TABLE_GETS += 1;
				}

				// Check for position at depth-1 (the depth we'll evaluate this position at)
				if let Some((score, _entry_type, _stored_depth, _best_move)) = transposition_table.get_depth(game, (depth - 2)  as usize) {
					unsafe { TP_TABLE_HIT_RATE.0 += 1; }
					game.reverse();
					return (idx, Some(score), 0.0);
				}
				game.reverse();
				(idx, None, 0.0)
			}).collect();

			moves_with_scores.sort_unstable_by(|(_, a_score, a_capture_score), (_, b_score, b_capture_score)| {
				// If move involves a capture, sort by MVV - LVA score
				if *a_capture_score > 0.0 || *b_capture_score > 0.0 {
					// TODO: Fix, capture_score is not colored so I think should always be maximized?
					return b_capture_score.partial_cmp(a_capture_score).unwrap_or(Ordering::Equal);
				}

				// Else, sort by previous evaluation from transposition table
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

			moves_with_scores.into_iter().map(|(idx, _, capture_score)| idx).collect()
		} else {
			(0..move_count).collect()
		};

		let mut tp_table_entry_type = EntryType::Exact;
		let mut best_move = game::Move(game::Square(0,0), game::Square(0,0));
		let mut best_score = if game.get_turn() == game::Color::White { f32::MIN } else { f32::MAX };
		let mut moves_checked = 0; // Track actual moves checked (accounting for continue statements)
		let mut pruned = false; // Track if we pruned

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
			// Check if castling move is illegal (king or intermediate squares are attacked)
			if game::Move::is_castle(&mv) {
				let opponent_mobility = match game.turn {
					game::Color::White => game.bitboards.black_mobility,
					game::Color::Black => game.bitboards.white_mobility,
					game::Color::Blank => 0,
				};
				
				// Create bitboard mask for squares involved in castling
				let castle_squares_mask = match mv {
					// White kingside: e1(7,4) -> g1(7,6), check e1, f1(7,5), g1
					game::Move(game::Square(7, 4), game::Square(7, 6)) => {
						(1u64 << (7 * 8 + 4)) | (1u64 << (7 * 8 + 5)) | (1u64 << (7 * 8 + 6))
					}
					// White queenside: e1(7,4) -> c1(7,2), check e1, d1(7,3), c1
					game::Move(game::Square(7, 4), game::Square(7, 2)) => {
						(1u64 << (7 * 8 + 4)) | (1u64 << (7 * 8 + 3)) | (1u64 << (7 * 8 + 2))
					}
					// Black kingside: e8(0,4) -> g8(0,6), check e8, f8(0,5), g8
					game::Move(game::Square(0, 4), game::Square(0, 6)) => {
						(1u64 << (0 * 8 + 4)) | (1u64 << (0 * 8 + 5)) | (1u64 << (0 * 8 + 6))
					}
					// Black queenside: e8(0,4) -> c8(0,2), check e8, d8(0,3), c8
					game::Move(game::Square(0, 4), game::Square(0, 2)) => {
						(1u64 << (0 * 8 + 4)) | (1u64 << (0 * 8 + 3)) | (1u64 << (0 * 8 + 2))
					}
					_ => 0, // Should never happen if is_castle returned true
				};
				
				// If any castle squares are attacked, skip this move
				if opponent_mobility & castle_squares_mask > 0 {
					continue;
				}
			}
			
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
				// Checkmate ends the search. Count prunable moves: moves_checked (we haven't incremented yet, so this move is included)
				// But we need to subtract 1 because the first move is always checked (not prunable)
				unsafe { PRUNE_COUNTS[depth as usize].1 += moves_checked; }
				unsafe { PRUNE_COUNTS[depth as usize].0 += move_count - 1; }
				return AsyncEvaluation::Eval(mv, checkmate_score);
			}

			moves_checked += 1; // Count this move as checked

			let score = if self.check_for_repetition(game) {
				0.0
			} else if depth == max_depth {
				match self.minimax_eval(game, depth - 1, floor, ceil, transposition_table, max_depth, cancel_flag) {
					AsyncEvaluation::Eval(_, s) => s + self.do_castle_bonus(game, &mv),
					AsyncEvaluation::Cancel => {
						game.reverse();
						return AsyncEvaluation::Cancel;
					}
				}
			} else if depth > 0 {
				match self.minimax_eval(game, depth - 1, floor, ceil, transposition_table, max_depth, cancel_flag) {
					AsyncEvaluation::Eval(_, s) => s,
					AsyncEvaluation::Cancel => {
						game.reverse();
						return AsyncEvaluation::Cancel;
					}
				}
			} else { // At max depth, evaluate the position.
				self.eval(&game.state)			
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
					pruned = true;
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
					pruned = true;
					break; // Prune remaining moves
				}
			}
		}

		// Count prunable moves checked: moves_checked - 1 (excluding the first move which is always checked)
		// Only count once, whether we pruned or not
		unsafe { PRUNE_COUNTS[depth as usize].1 += moves_checked.saturating_sub(1); }

		// // If no move resulted in a score or checkmate, this is a draw.
		// if best_move == game::Move(game::Square(0,0), game::Square(0,0)) {
		// 	if depth == 0 {
		// 		println!("No move resulted in a score or checkmate at depth 0\n{:?}", game.state);
		// 	}
		// 	return (best_move, 0.0);
		// }

		// Store position at current depth (before making moves) for transposition table lookup
		if depth >= TRANSPOSITION_TABLE_DEPTH {
			unsafe { TP_TABLE_SETS += 1; }
			transposition_table.set(game, best_score, depth as usize, tp_table_entry_type, best_move);
		}

		unsafe { PRUNE_COUNTS[depth as usize].0 += move_count - 1; }
		return AsyncEvaluation::Eval(best_move, best_score);
	}

	pub fn check_for_repetition(&self, game: &game::Game) -> bool {
		if game.since_last_non_reversible_move == 50 {
			return true;
		}

		let original_game_state = &game.state;

		// In theory this can be optimized by only checking every other move,
		// but it's not worth the complexity for now.
		let mut look_back_to = game.since_last_non_reversible_move as i16;
		let mut current_game_state = &game.state; // &Box<GameState>
		while let Some(next_game_state) = &current_game_state.parent { // Option<Box<GameState>>
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
			score += self.piece_weights[piece.role as usize];
			pieces_bitboard ^= 1u64 << piece_idx;
		}
		score
	}

	fn attack_score(&self, game_state: &game::GameState, color: game::Color) -> f32 {
		let (enemy_material, piece_moves_array) = if color == game::Color::White {
			(
				game_state.bitboards.black_material,
				[
					game_state.bitboards.white_queen_moves,  // Queen
					game_state.bitboards.white_king_moves,   // King
					game_state.bitboards.white_rook_moves,   // Rook
					game_state.bitboards.white_bishop_moves, // Bishop
					game_state.bitboards.white_knight_moves, // Knight
					game_state.bitboards.white_pawn_moves,   // Pawn
				]
			)
		} else {
			(
				game_state.bitboards.white_material,
				[
					game_state.bitboards.black_queen_moves,  // Queen
					game_state.bitboards.black_king_moves,   // King
					game_state.bitboards.black_rook_moves,   // Rook
					game_state.bitboards.black_bishop_moves, // Bishop
					game_state.bitboards.black_knight_moves, // Knight
					game_state.bitboards.black_pawn_moves,   // Pawn
				]
			)
		};

		let mut score = 0.0;
		for attacking_piece_idx in 0..6 {
			let attacked_squares = piece_moves_array[attacking_piece_idx] & enemy_material;
			let mut attacked_bitboard = attacked_squares;
			
			while attacked_bitboard != 0 {
				let square_idx = attacked_bitboard.trailing_zeros() as usize;
				let attacked_piece = game_state.board[square_idx / 8][square_idx % 8];
				let attacked_piece_idx = attacked_piece.role as usize;
				
				score += self.attack_weights[attacking_piece_idx][attacked_piece_idx];
				
				attacked_bitboard ^= 1u64 << square_idx;
			}
		}

		score
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
			move_list: OnceCell::new(),
			parent: None,
			since_last_non_reversible_move: 0,
			move_count: 0,
			zobrist_hash: game::ZobristHash::new(),
		};
		starting_game_state.bitboards = game::Bitboards::from(&starting_game_state);

		let mut game = game::Game::from(starting_game_state);
		let mut bot = Bot::new(3, 0.1, CENTER_CONTROL_WEIGHT, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		let (mv, score) = bot.evaluate_position_max_depth(&mut game, &mut TranspositionTable::new());
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

	#[test]
	fn test_tp_tables() {
		use crate::transposition_tables::TranspositionTable;
		
		let mut game_with_tp = game::Game::new();
		let mut game_without_tp = game::Game::new();
		let mut tp_table = TranspositionTable::new();
		
		let bot1 = Bot::new(4, 0.1, CENTER_CONTROL_WEIGHT, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		let bot2 = Bot::new(4, 0.1, CENTER_CONTROL_WEIGHT, 5.0, 2.0, DEFAULT_PIECE_WEIGHTS, DEFAULT_ATTACK_WEIGHTS, 0.5, DEFAULT_PAWN_ADVANCE_WEIGHTS);
		
		let mut move_count = 0;
		const MAX_MOVES: usize = 50; // Limit to prevent infinite loops
		
		loop {
			if game_with_tp.checkmate || game_without_tp.checkmate {
				assert_eq!(game_with_tp.checkmate, game_without_tp.checkmate, "Checkmate mismatch at move {}", move_count);
				break;
			}
			
			if move_count >= MAX_MOVES {
				break;
			}
			
			// Get moves from both bots
			let (move_with_tp, _) = bot1.evaluate_position_max_depth(&mut game_with_tp, &mut tp_table);
			let mut empty_tp = TranspositionTable::new();
			let (move_without_tp, _) = bot1.evaluate_position_max_depth(&mut game_without_tp, &mut empty_tp);
			
			// Moves should be the same (transposition table only affects ordering, not outcome)
			assert_eq!(move_with_tp, move_without_tp, "Move mismatch at move {}: with_tp={:?}, without_tp={:?}", move_count, move_with_tp, move_without_tp);
			
			// Make moves
			game_with_tp.make_move(move_with_tp);
			game_without_tp.make_move(move_without_tp);
			
			// Compare game states after move (PartialEq compares board, turn, and castle rights)
			assert_eq!(*game_with_tp.state, *game_without_tp.state, "GameState mismatch at move {}", move_count);
			// Also check derived fields that aren't part of PartialEq
			assert_eq!(game_with_tp.state.white_check, game_without_tp.state.white_check, "White check mismatch at move {}", move_count);
			assert_eq!(game_with_tp.state.black_check, game_without_tp.state.black_check, "Black check mismatch at move {}", move_count);
			assert_eq!(game_with_tp.state.checkmate, game_without_tp.state.checkmate, "Checkmate mismatch at move {}", move_count);
			
			move_count += 1;
			
			// Check for game end conditions after white move
			if game_with_tp.checkmate || game_without_tp.checkmate {
				assert_eq!(game_with_tp.checkmate, game_without_tp.checkmate, "Checkmate mismatch at move {}", move_count);
				break;
			}
			
			// Black's turn
			let (move_with_tp, _) = bot2.evaluate_position_max_depth(&mut game_with_tp, &mut tp_table);
			let mut empty_tp = TranspositionTable::new();
			let (move_without_tp, _) = bot2.evaluate_position_max_depth(&mut game_without_tp, &mut empty_tp);
			
			// Moves should be the same
			assert_eq!(move_with_tp, move_without_tp, "Move mismatch at move {}: with_tp={:?}, without_tp={:?}", move_count, move_with_tp, move_without_tp);
			
			// Make moves
			game_with_tp.make_move(move_with_tp);
			game_without_tp.make_move(move_without_tp);
			
			// Compare game states after move (PartialEq compares board, turn, and castle rights)
			assert_eq!(*game_with_tp.state, *game_without_tp.state, "GameState mismatch at move {}", move_count);
			// Also check derived fields that aren't part of PartialEq
			assert_eq!(game_with_tp.state.white_check, game_without_tp.state.white_check, "White check mismatch at move {}", move_count);
			assert_eq!(game_with_tp.state.black_check, game_without_tp.state.black_check, "Black check mismatch at move {}", move_count);
			assert_eq!(game_with_tp.state.checkmate, game_without_tp.state.checkmate, "Checkmate mismatch at move {}", move_count);
			
			move_count += 1;
			
			// Check for repetition
			if bot2.check_for_repetition(&game_with_tp) || bot2.check_for_repetition(&game_without_tp) {
				assert_eq!(bot2.check_for_repetition(&game_with_tp), bot2.check_for_repetition(&game_without_tp), "Repetition mismatch at move {}", move_count);
				break;
			}
		}
	}
}