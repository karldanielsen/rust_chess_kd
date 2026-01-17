use once_cell::sync::Lazy;
use crate::constants;
use rand::Rng;
use std::array;

static ROOK_MAGIC_MASKS: Lazy<([u64; 64], [Vec<u64>; 64])> = Lazy::new(|| {
	let mut magic_numbers = [0u64; 64];
	let mut lookup_tables: [Vec<u64>; 64] = array::from_fn(|_| Vec::new());

    let mut i = 0;
	while i < 64 {
        // Get the occupancy mask for the square
		let mut mask = constants::ROOK_MAGIC_MASKS[i];
		let mask_clone = mask;
        let mut mask_vec = Vec::with_capacity(12);

        // Convert to a vector of bit indices
        while mask != 0 {
            let idx = mask.trailing_zeros() as usize;
            mask_vec.push(idx);
            mask ^= 1u64 << idx;
        }

        // Get all combinations of the occupancy mask
		let mut occupancy_combinations = Vec::with_capacity(1 << mask_vec.len());

		let mut move_masks = Vec::with_capacity(1 << mask_vec.len());
        let mut j = 0;
		let combination_count = 1 << mask_vec.len();
        while j < combination_count {
            // Get combination
            let mut combination = 0u64;
            let mut k = 0;
            while k < mask_vec.len() {
                if j & (1u64 << k) != 0 {
                    combination |= 1u64 << mask_vec[k];
                }
                k += 1;
            }
            occupancy_combinations.push(combination);

            // Calculate move BB from combination
            let mut k = 1;
            let mut valid_pairs = [true; 4];	
            let mut move_mask = 0u64;
            while k < 8 {
                let mut pair_idx = 0;
                while pair_idx < 4 {
                    if !valid_pairs[pair_idx] {
						pair_idx += 1;
                        continue;
                    }
                    
                    let x = (i / 8) as i8 + (k as i8 * constants::ROOK_MOVE_PAIRS[pair_idx].0);
                    let y = (i % 8) as i8 + (k as i8 * constants::ROOK_MOVE_PAIRS[pair_idx].1);
                    if x >= 0 && x < 8 && y >= 0 && y < 8 {
                        move_mask |= 1u64 << (x as u64 * 8 + y as u64);
                        if combination & 1u64 << (x as u64 * 8 + y as u64) > 0 {
                            valid_pairs[pair_idx] = false;
                        }
                    }
                    pair_idx += 1;
                }
                k += 1;
            }
			move_masks.push(move_mask);
            j += 1;
        }

		loop {
			let mut rng = rand::rng();
			let magic_number_1: u64 = rng.random();
			let magic_number_2: u64 = rng.random();
			let magic_number_3: u64 = rng.random();
			let magic_number: u64 = magic_number_1 & magic_number_2 & magic_number_3;
			let hash_bits = (magic_number.wrapping_mul(mask_clone)) >> (64 - 12);
			if hash_bits.count_ones() < 6 {
				continue;
			}
			let mut iteration_failed = false;
			let mut lookup_table = Vec::with_capacity(1 << 12);
			lookup_table.resize(1 << 12, 0);
			let mut config_index = 0;
			while config_index < occupancy_combinations.len() {
				let table_index = (occupancy_combinations[config_index].wrapping_mul(magic_number) >> (64 - 12)) as usize;
				if lookup_table[table_index] == 0 {
					lookup_table[table_index] = move_masks[config_index];
				} else if lookup_table[table_index] != move_masks[config_index] {
					iteration_failed = true;
					break;
				}
				config_index += 1;
			}
			if !iteration_failed {
				magic_numbers[i] = magic_number;
				lookup_tables[i] = lookup_table;
				break;
			}
		}
		i += 1;
	}
	(magic_numbers, lookup_tables)
});


#[inline(always)]
pub fn get_rook_moves(idx: usize, occupancy: u64) -> u64 {
	let tables = &*ROOK_MAGIC_MASKS;
	let mask = constants::ROOK_MAGIC_MASKS[idx];
	let magic_number = tables.0[idx];
	let table_index = ((occupancy & mask).wrapping_mul(magic_number) >> (64 - 12)) as usize;
	tables.1[idx][table_index]
}


fn compute_bishop_magic_mask(idx: usize) -> u64 {
    let mut mask = 0u64;

    let row = (idx / 8) as i8;
    let col = (idx % 8) as i8;
    let mut idx = 1;
    while idx < 7 {
        let mut pair_idx = 0;
        while pair_idx < 4 {
            let pair = constants::BISHOP_MOVE_PAIRS[pair_idx];
            pair_idx += 1;
            let new_row = row + idx * pair.0;
            let new_col = col + idx * pair.1;
            if new_row >= 0 && new_row < 8 && new_col >= 0 && new_col < 8 {
                // In addition to checking if the new square is on the board,
                // we also need to check if it is the last square in the direction.
                if new_row == 7 && pair.0 == 1 || new_row == 0 && pair.0 == -1 ||
                   new_col == 7 && pair.1 == 1 || new_col == 0 && pair.1 == -1 {
                    continue;
                }

                mask |= 1u64 << (new_row as u64 * 8 + new_col as u64);
            }
        }
        idx += 1;
    }
    mask
}

static BISHOP_MAGIC_MASKS: Lazy<([u64; 64], [Vec<u64>; 64])> = Lazy::new(|| {
	let mut magic_numbers = [0u64; 64];
	let mut lookup_tables: [Vec<u64>; 64] = array::from_fn(|_| Vec::new());

    let mut i = 0;
	while i < 64 {
        // Get the occupancy mask for the square
		let mut mask = constants::ALL_BISHOP_MAGIC_MASKS[i];
		let mask_clone = mask;
        let mut mask_vec = Vec::with_capacity(9);

        // Convert to a vector of bit indices
        while mask != 0 {
            let idx = mask.trailing_zeros() as usize;
            mask_vec.push(idx);
            mask ^= 1u64 << idx;
        }

        // Get all combinations of the occupancy mask
		let mut occupancy_combinations = Vec::with_capacity(1 << mask_vec.len());

		let mut move_masks = Vec::with_capacity(1 << mask_vec.len());
        let mut j = 0;
		let combination_count = 1 << mask_vec.len();
        while j < combination_count {
            // Get combination
            let mut combination = 0u64;
            let mut k = 0;
            while k < mask_vec.len() {
                if j & (1u64 << k) != 0 {
                    combination |= 1u64 << mask_vec[k];
                }
                k += 1;
            }
            occupancy_combinations.push(combination);

            // Calculate move BB from combination
            let mut k = 1;
            let mut valid_pairs = [true; 4];	
            let mut move_mask = 0u64;
            while k < 8 {
                let mut pair_idx = 0;
                while pair_idx < 4 {
                    if !valid_pairs[pair_idx] {
						pair_idx += 1;
                        continue;
                    }
                    
                    let x = (i / 8) as i8 + (k as i8 * constants::BISHOP_MOVE_PAIRS[pair_idx].0);
                    let y = (i % 8) as i8 + (k as i8 * constants::BISHOP_MOVE_PAIRS[pair_idx].1);
                    if x >= 0 && x < 8 && y >= 0 && y < 8 {
                        move_mask |= 1u64 << (x as u64 * 8 + y as u64);
                        if combination & 1u64 << (x as u64 * 8 + y as u64) > 0 {
                            valid_pairs[pair_idx] = false;
                        }
                    }
                    pair_idx += 1;
                }
                k += 1;
            }
			move_masks.push(move_mask);
            j += 1;
        }

		loop {
			let mut rng = rand::rng();
			let magic_number_1: u64 = rng.random();
			let magic_number_2: u64 = rng.random();
			let magic_number_3: u64 = rng.random();
			let magic_number: u64 = magic_number_1 & magic_number_2 & magic_number_3;
			let hash_bits = (magic_number.wrapping_mul(mask_clone)) >> (64 - 9);
			if hash_bits.count_ones() < 6 {
				continue;
			}
			let mut iteration_failed = false;
			let mut lookup_table = Vec::with_capacity(1 << 9);
			lookup_table.resize(1 << 9, 0);
			let mut config_index = 0;
			while config_index < occupancy_combinations.len() {
				let table_index = (occupancy_combinations[config_index].wrapping_mul(magic_number) >> (64 - 9)) as usize;
				if lookup_table[table_index] == 0 {
					lookup_table[table_index] = move_masks[config_index];
				} else if lookup_table[table_index] != move_masks[config_index] {
					iteration_failed = true;
					break;
				}
				config_index += 1;
			}
			if !iteration_failed {
				magic_numbers[i] = magic_number;
				lookup_tables[i] = lookup_table;
				break;
			}
		}
		i += 1;
	}
	(magic_numbers, lookup_tables)
});


#[inline(always)]
pub fn get_bishop_moves(idx: usize, occupancy: u64) -> u64 {
	let tables = &*BISHOP_MAGIC_MASKS;
	let mask = constants::ALL_BISHOP_MAGIC_MASKS[idx];
	let magic_number = tables.0[idx];
	let table_index = ((occupancy & mask).wrapping_mul(magic_number) >> (64 - 9)) as usize;
	tables.1[idx][table_index]
}