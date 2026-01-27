use rand::Rng;

pub const TRANSPOSITION_TABLE_SIZE: usize = 16384;

pub fn format_mask(mask: u64) -> String {
    let mut s = String::from("\n");
    let mut i = 0;
    while i < 8 {
        let mut j = 0;
        while j < 8 {
            if mask & (1u64 << (i * 8 + j)) != 0 {
                s.push('1');
            } else {
                s.push('0');
            }
            j += 1;
        }
        s.push('\n');
        i += 1;
    }
    s
}

const fn compute_knight_mask(idx: usize) -> u64 {
    let row = idx / 8;
    let col = idx % 8;
    let mut mask = 0u64;
    let moves = [
        (1, 2), (2, 1), (-1, 2), (2, -1),
        (1, -2), (-2, 1), (-1, -2), (-2, -1),
    ];
    let mut i = 0;
    while i < 8 {
        let (dx, dy) = moves[i];
        let new_x = row as i8 + dx;
        let new_y = col as i8 + dy;
        if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
            mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
        }
        i += 1;
    }
    mask
}

// Pre-computed knight masks for all squares
pub const ALL_KNIGHT_MASKS: [u64; 64] = {
    let mut masks = [0u64; 64];
    let mut i = 0;
    while i < 64 {
        masks[i] = compute_knight_mask(i);
        i += 1;
    }
    masks
};


const KING_MOVE_PAIRS: [(i8, i8); 8] = [(0,1), (0,-1), (1,1), (1,0), (1,-1), (-1,1), (-1,0), (-1,-1)];

const fn compute_king_mask(idx: usize) -> u64 {
	let row = idx / 8;
	let col = idx % 8;
	let mut mask = 0u64;

	let mut i = 0;
    while i < 8 {
        let (dx, dy) = KING_MOVE_PAIRS[i];
        let new_x = row as i8 + dx;
        let new_y = col as i8 + dy;
        if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
            mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
        }
        i += 1;
    }
    mask
}
pub const ALL_KING_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_king_mask(i);
		i += 1;
	}
	masks
};

pub const ROOK_MOVE_PAIRS: [(i8, i8); 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];

const fn compute_rook_mask(idx: usize) -> u64 {
	let row = idx / 8;
	let col = idx % 8;
	let mut mask = 0u64;

	let mut i = 0;
	while i < 4 {
		let mut j = 0;
        while j < 8 {
            let (dx, dy) = ROOK_MOVE_PAIRS[i];
            let new_x = row as i8 + dx * j as i8;
            let new_y = col as i8 + dy * j as i8;
            if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
                mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
            }
            j += 1;
        }
		i += 1;
    }
    mask
}

pub const ALL_ROOK_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_rook_mask(i);
		i += 1;
	}
	masks
};


pub const BISHOP_MOVE_PAIRS: [(i8, i8); 4] = [(1, 1), (1, -1), (-1, 1), (-1, -1)];

const fn compute_bishop_mask(idx: usize) -> u64 {
	let row = idx / 8;
	let col = idx % 8;
	let mut mask = 0u64;

	let mut i = 0;
	while i < 4 {
        let mut j = 0;
        while j < 8 {
            let (dx, dy) = BISHOP_MOVE_PAIRS[i];
            let new_x = row as i8 + dx * j as i8;
            let new_y = col as i8 + dy * j as i8;
            if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
                mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
            }
            j += 1;
        }
        i += 1;
    }
    mask
}

pub const ALL_BISHOP_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_bishop_mask(i);
		i += 1;
	}
	masks
};


const fn compute_black_pawn_mask(idx: usize) -> u64 {
	let row = idx / 8;
	let col = idx % 8;
	let mut mask = 0u64;

	let new_x = row as i8 + 1;
	let new_y = col as i8 + 1;
	if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
		mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
    }

    let new_x = row as i8 - 1;
	let new_y = col as i8 + 1;
	if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
		mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
    }
    mask
}

pub const ALL_BLACK_PAWN_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_black_pawn_mask(i);
		i += 1;
	}
	masks
};


const fn compute_white_pawn_mask(idx: usize) -> u64 {
	let row = idx / 8;
	let col = idx % 8;
	let mut mask = 0u64;

	let new_x = row as i8 - 1;
	let new_y = col as i8 - 1;
	if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
		mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
    }

    let new_x = row as i8 - 1;
	let new_y = col as i8 + 1;
	if new_x >= 0 && new_x < 8 && new_y >= 0 && new_y < 8 {
		mask |= 1u64 << (new_x as usize * 8 + new_y as usize);
    }
    mask
}

pub const ALL_WHITE_PAWN_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_white_pawn_mask(i);
		i += 1;
	}
	masks
};


const fn compute_rook_magic_mask(idx: usize) -> u64 {
    let mut mask = 0u64;

    let row = (idx / 8) as i8;
    let col = (idx % 8) as i8;
    let mut idx = 1;
    while idx < 7 {
        let mut pair_idx = 0;
        while pair_idx < 4 {
            let pair = ROOK_MOVE_PAIRS[pair_idx];
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

pub const ROOK_MAGIC_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_rook_magic_mask(i);
		i += 1;
	}
	masks
};

const fn compute_bishop_magic_mask(idx: usize) -> u64 {
    let mut mask = 0u64;

    let row = (idx / 8) as i8;
    let col = (idx % 8) as i8;
    let mut idx = 1;
    while idx < 7 {
        let mut pair_idx = 0;
        while pair_idx < 4 {
            let pair = BISHOP_MOVE_PAIRS[pair_idx];
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

pub const ALL_BISHOP_MAGIC_MASKS: [u64; 64] = {
	let mut masks = [0u64; 64];
	let mut i = 0;
	while i < 64 {
		masks[i] = compute_bishop_magic_mask(i);
		i += 1;
	}
	masks
};