use crate::game;
use crate::constants::TRANSPOSITION_TABLE_SIZE;
use std::rc::Rc;

#[derive(Clone, Copy)]
pub enum EntryType {
	Exact,
	Lowerbound,
	Upperbound,
}

#[derive(Clone)]
struct TranspositionTableEntry {
	hash: u64,
	score: f32,
	depth: usize,
	best_move: game::Move,
	entry_type: EntryType,
	// game_state: Rc<game::GameState>,  // Store full game state for collision resolution
}

pub struct TranspositionTable {
	table: Vec<Option<TranspositionTableEntry>>,
}

impl TranspositionTable {
	pub fn new() -> TranspositionTable {
		let table = vec![None; TRANSPOSITION_TABLE_SIZE];
        TranspositionTable { table }
	}

	pub fn get(&self, game: &game::Game) -> Option<f32> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.hash == hash {
				return Some(entry.score);
			}
			return None;
		}
		None
	}

	pub fn get_depth(&self, game: &game::Game, depth: usize) -> Option<(f32, EntryType, usize, game::Move)> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.hash == hash && entry.depth <= depth {
				return Some((entry.score, entry.entry_type, entry.depth, entry.best_move));
			}
            return None;  // Hash collision or different position
		}
		None
	}

	pub fn set(&mut self, game: &game::Game, score: f32, depth: usize, entry_type: EntryType, best_move: game::Move) {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;

		// Early return if we already have an entry at this depth or better.
		if let Some(_) = self.get_depth(game, depth) {
			return;
		}

		self.table[index] = Some(TranspositionTableEntry {
			hash,
			score,
			depth,
			entry_type,
			best_move,
			// game_state: Rc::clone(&game.state),  // Clone the Rc (cheap, just increments ref count)
		});
	}
}