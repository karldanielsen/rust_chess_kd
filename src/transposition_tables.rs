use crate::game;
use crate::constants::TRANSPOSITION_TABLE_SIZE;

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

	pub fn get_depth(&self, game: &game::Game, depth: usize) -> Option<(f32, EntryType, usize, game::Move)> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.hash == hash && entry.depth >= depth {
				return Some((entry.score, entry.entry_type, entry.depth, entry.best_move));
			}
		}
		return None;
	}

	pub fn set(&mut self, game: &game::Game, score: f32, depth: usize, entry_type: EntryType, best_move: game::Move) {
		if let Some(_) = self.get_depth(game, depth) {
			return;
		}

		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;

		self.table[index] = Some(TranspositionTableEntry {
			hash,
			score,
			depth,
			entry_type,
			best_move,
		});
	}
}