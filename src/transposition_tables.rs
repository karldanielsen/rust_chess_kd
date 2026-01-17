use crate::game;
use crate::constants::TRANSPOSITION_TABLE_SIZE;

#[derive(Clone, Copy)]
pub enum EntryType {
	Exact,
	Lowerbound,
	Upperbound,
}

#[derive(Clone)]
struct TranspositionTableEntry(u64, f32, usize, EntryType);

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
			if entry.0 == hash {
                // println!("Transposition table hit\n");
				return Some(entry.1);
			}
            // println!("Transposition table wrong hash hit\n");
			return None;
		}
		// print!("Transposition table miss\n");
		None
	}

	pub fn get_depth(&self, game: &game::Game, depth: usize) -> Option<(f32, EntryType)> {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;
		if let Some(entry) = &self.table[index] {
			if entry.0 == hash && entry.2 >= depth {
                // println!("Transposition table hit\n");
				return Some((entry.1, entry.3));
			}
            // println!("Transposition table wrong hash hit\n");
			return None;
		}
		// print!("Transposition table miss\n");
		None
	}

	pub fn set(&mut self, game: &game::Game, score: f32, depth: usize, entry_type: EntryType) {
		let hash = game.state.zobrist_hash.hash;
		let index = (hash % TRANSPOSITION_TABLE_SIZE as u64) as usize;

		// TODO: Maybe don't evict entries with a higher depth?
		self.table[index] = Some(TranspositionTableEntry(hash, score, depth, entry_type));
	}
}