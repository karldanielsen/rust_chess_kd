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
	entry_type: EntryType,
	game_state: Rc<game::GameState>,  // Store full game state for collision resolution
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
			// First check hash (fast), then verify with full state comparison
			if entry.hash == hash {
				if entry.game_state.as_ref() == game.state.as_ref() {
					return Some(entry.score);
				}
				else {
					println!("Yep! That's a nasty collision!")
					return None;
				}
			}
			return None;
		}
		None
	}

	pub fn get_depth(&self, game: &game::Game, depth: usize) -> Option<(f32, EntryType)> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.hash == hash && entry.depth >= depth {
                if entry.game_state.as_ref() == game.state.as_ref() {
					return Some(entry.score);
				}
				else {
					println!("Yep! That's a nasty collision!")
					return None;
				}
			}
            return None;  // Hash collision or different position
		}
		None
	}

	pub fn get_depth(&self, game: &game::Game, depth: usize) -> Option<(f32, EntryType)> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.hash == hash && entry.depth >= depth && entry.game_state.as_ref() == game.state.as_ref() {
                return Some((entry.score, entry.entry_type));
			}
            return None;
		}
		None
	}

	pub fn set(&mut self, game: &game::Game, score: f32, depth: usize, entry_type: EntryType) {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;

		// TODO: Maybe don't evict entries with a higher depth?
		self.table[index] = Some(TranspositionTableEntry {
			hash,
			score,
			depth,
			entry_type,
			game_state: Rc::clone(&game.state),  // Clone the Rc (cheap, just increments ref count)
		});
	}
}