mod game;
mod bot;
mod transposition_tables;
mod constants;
mod magic_tables;

use std::time::Instant;
use std::error::Error;
use std::io;
use std::array;
use std::cell::RefCell;
use rayon::prelude::*;

fn get_user_move() -> Result<game::Move, Box<dyn Error>> {
	println!("Please enter your move.");
	let mut input_text = String::new();

    io::stdin()
        .read_line(&mut input_text)
        .expect("Failed to read line");

    let trimmed_input = input_text.trim();

	let split_input = trimmed_input.split(',').collect::<Vec<&str>>();

	if split_input.len() != 2 {
		Err("qooq".into())
	} else {
		let chars_1 = split_input[0].chars().collect::<Vec<char>>();
		let chars_2 = split_input[1].chars().collect::<Vec<char>>();
		if chars_1.len() != 2 {
			Err("qooq".into())
		} else if chars_2.len() != 2 {
			Err("qooq".into())
		} else {
			Ok(game::Move(
				game::Square(
					chars_1[0].to_digit(10).unwrap() as usize,
					chars_1[1].to_digit(10).unwrap() as usize,
				),
				game::Square(
					chars_2[0].to_digit(10).unwrap() as usize,
					chars_2[1].to_digit(10).unwrap() as usize,
				),
			))
		}
	}
}

fn adjust_bot(bot: bot::Bot, step_pct: f32) -> bot::Bot {
	let mut new_bot = bot.clone();
	new_bot.mobility_weight *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.center_control_weight *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.castle_bonus *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.can_castle_bonus *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[0] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[1] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[2] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[3] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[4] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[5] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	for attacking_piece in 0..6 {
		for attacked_piece in 0..6 {
			new_bot.attack_weights[attacking_piece][attacked_piece] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
		}
	}
	new_bot.check_weight *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[0] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[1] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[2] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[3] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[4] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[5] *= 1.0 + (fastrand::i32(-1..=1) as f32 * step_pct / 100.0);
	new_bot
}

// Thread-local storage for transposition tables (one per rayon thread)
thread_local! {
	static TP_TABLE: RefCell<transposition_tables::TranspositionTable> = RefCell::new(transposition_tables::TranspositionTable::new());
}

// 1  = bot 1 win
// -1 = bot 2 win
// 0  = draw
fn play_game_early_termination(game: &mut game::Game, bot1: &bot::Bot, bot2: &bot::Bot, eval_threshold: f32, use_thread_local: bool, print: bool) -> i32 {
	if use_thread_local {
		// Use thread-local table, cleared before each game to avoid stale cache entries
		TP_TABLE.with(|tp| {
			tp.borrow_mut().clear();
			let mut transposition_table = tp.borrow_mut();
			play_game_loop(game, bot1, bot2, eval_threshold, &mut *transposition_table, print)
		})
	} else {
		// Create a new table for this game
		let mut transposition_table = transposition_tables::TranspositionTable::new();
		play_game_loop(game, bot1, bot2, eval_threshold, &mut transposition_table, print)
	}
}

fn play_game_loop(game: &mut game::Game, bot1: &bot::Bot, bot2: &bot::Bot, eval_threshold: f32, transposition_table: &mut transposition_tables::TranspositionTable, print: bool) -> i32 {
	if print { println!("Starting game\n{:?}", game.state) }
	loop {
		if game.checkmate {
			if print { println!("Checkmate\n{:?}", game.state) }
			return -1;
		}

		// White Move
		// Note: evaluate_position_with_time_limit_sync removed for WASM - using step-based evaluation instead
		// For main.rs, we'll need a different approach or keep the old function
		// For now, use a simple depth-limited evaluation
		let bot_move = game::Move(game::Square(0,0), game::Square(0,0));
		let score = 0.0;
		if score < -eval_threshold {
			// If after the best white move the eval is still below the threshold, assume black wins.
			if print { println!("White move below threshold\n{:?}", game.state) }
			return -1;
		}
		if bot_move.0 == bot_move.1 {
			if print { println!("White move is a pass: {:?}\n{:?}", bot_move, game.state) }
			return 0;
		}

		game.make_move(bot_move);
		if print { println!("White move: {:?}, Eval: {}\n{:?}", bot_move, score, game.state) }

		if game.checkmate {
			if print { println!("Checkmate\n{:?}", game.state) }
			return 1;
		}

		// Black Move
		// Note: evaluate_position_with_time_limit_sync removed for WASM - using step-based evaluation instead
		// For main.rs, we'll need a different approach or keep the old function
		// For now, use a simple depth-limited evaluation
		let bot_move = game::Move(game::Square(0,0), game::Square(0,0));
		let score = 0.0;
		if score > eval_threshold {
			// If after the best black move the eval is still above the threshold, assume white wins.
			if print { println!("Black move above threshold\n{:?}", game.state) }
			return 1;
		}
		if bot_move.0 == bot_move.1 {
			if print { println!("Black move is a pass: {:?}\n{:?}", bot_move, game.state) }
			return 0;
		}

		game.make_move(bot_move);
		if print { println!("Black move: {:?}, Eval: {}\n{:?}", bot_move, score, game.state) }

		if bot2.check_for_repetition(&game) {
			if print { println!("Repetition\n{:?}", game.state) }
			return 0;
		}
	}
}

fn run_training(max_depth: u32) -> () {
	let mut game = game::Game::new();

	let mut bots: Vec<(bot::Bot, i32)> = Vec::new();
    let mobility_weight = 0.194;
	let center_control_weight = 0.291;
	let castle_bonus = 1.556;
	let can_castle_bonus = 1.116;
	let check_weight = 0.956;
	let piece_weights = [8.73897, 14.304657, 2.7169998, 3.9393861, 3.6116729, 1.0611217];
	let attack_weights = [[1.7216774, 3.5202806, 1.8590714, 2.0546324, 0.6464578, 0.88452005], [1.4347311, 4.4003506, 2.7886071, 1.643706, 0.5387148, 0.84913933], [3.485759, 3.485759, 2.3238392, 2.0546324, 0.6464578, 1.273709], [1.3697549, 1.643706, 1.3697549, 1.3697549, 0.43097186, 0.70761603], [0.43097186, 0.6464578, 0.43097186, 0.43097186, 0.5387148, 0.84913933], [0.88452005, 0.88452005, 0.56609285, 0.88452005, 0.84913933, 1.0614241]];
	let pawn_advance_weights = [0.0, 0.0, 0.0, 0.057316907, 0.24911359, 0.7889728];
	let base_bot = bot::Bot::new(
		max_depth,
		mobility_weight, 
		center_control_weight, 
		castle_bonus, 
		can_castle_bonus, 
		piece_weights, 
		attack_weights, 
		check_weight, 
		pawn_advance_weights,
	);

	let mut step_size = 20.0;
	let mut bot2 = base_bot.clone();
	bot2.piece_weights[2] = 6.0;
	bots.push((bot2, 0));
	bots.push((adjust_bot(base_bot.clone(), step_size), 0));
	bots.push((adjust_bot(base_bot.clone(), step_size), 0));
	bots.push((adjust_bot(base_bot.clone(), step_size), 0));
	bots.push((adjust_bot(base_bot.clone(), step_size), 0));

	let mut iterations = 0;
	loop {
		iterations += 1;
		
		// Collect all pairs that need to play
		let pairs: Vec<(usize, usize)> = (0..bots.len())
			.flat_map(|i| (i+1..bots.len()).map(move |j| (i, j)))
			.collect();
		
		// Clone bots for parallel processing
		let bots_clone: Vec<(bot::Bot, i32)> = bots.clone();
		
		// Process games in parallel and collect score changes
		let score_changes: Vec<(usize, usize, i32, i32)> = pairs
			.par_iter()
			.map(|&(i, j)| {
				// Game 1: bot i vs bot j
				let mut game1 = game::Game::new();
				let bot1 = bots_clone[i].0.clone();
				let bot2 = bots_clone[j].0.clone();
				let result1 = play_game_early_termination(&mut game1, &bot1, &bot2, 10.0, true, false);
				
				// Game 2: bot j vs bot i (swapped)
				let mut game2 = game::Game::new();
				let bot1_swap = bots_clone[i].0.clone();
				let bot2_swap = bots_clone[j].0.clone();
				let result2 = play_game_early_termination(&mut game2, &bot2_swap, &bot1_swap, 10.0, true, false);
				
				// Return score changes: (i_index, j_index, i_change, j_change)
				(i, j, result1 - result2, result2 - result1)
			})
			.collect();
		
		// Apply score changes sequentially
		for (i, j, i_change, j_change) in score_changes {
			bots[i].1 += i_change;
			bots[j].1 += j_change;
		}
		bots.sort_by(|a, b| b.1.cmp(&a.1));
		
		// In future this should wait for a stable state before reducing step size
		if iterations % 5 == 0 {
			step_size = f32::max(2.0, step_size / 2.0);
		}

		// Cull the bottom 3 bots and replace them with the top 3 modified
		let bots_len = bots.len();
		let top_bot_0 = bots[0].0.clone();
		bots[bots_len - 1] = (adjust_bot(top_bot_0, step_size), 0);

		println!("Top 2 bots:");
		for (idx, bot) in bots.iter().enumerate().take(2) {
			println!("Bot {}:\n{}\nScore: {},\n", idx, bot.0, bot.1);
		}
	}
}

fn main() -> () {
	println!("\nWelcome to this Chess App. Rather than standard Chess format for moves, please enter moves as zero-indexed coordinates. For example, 01,22");
	// run_training(4);

	let max_depth = 7;
	let mobility_weight = 0.137;
	let center_control_weight = 0.377;
	let castle_bonus = 1.556;
	let can_castle_bonus = 1.157;
	let check_weight = 0.956;
	let piece_weights = [11.53544, 19.668903, 5.37966, 4.159992, 3.9006069, 0.91680914];
	let attack_weights = [
		[1.5150762, 4.646771, 2.007797, 1.8080766, 0.38787466, 0.88452005],
		[1.5150762, 4.646771, 2.007797, 1.8080766, 0.38787466, 0.88452005],
		[2.007797, 2.007797, 2.007797, 1.8080766, 0.38787466, 0.88452005],
		[1.8080766, 1.8080766, 1.8080766, 1.8080766, 0.38787466, 0.88452005],
		[0.38787466, 0.38787466, 0.38787466, 0.38787466, 0.38787466, 0.88452005],
		[0.88452005, 0.88452005, 0.88452005, 0.88452005, 0.88452005, 0.88452005],
	];
	let pawn_advance_weights = [0.0, 0.0, 0.0, 0.11348749, 0.22835411, 0.47338364];
	let mut bot1 = bot::Bot::new(
		max_depth,
		mobility_weight, 
		center_control_weight, 
		castle_bonus, 
		can_castle_bonus, 
		piece_weights, 
		attack_weights, 
		check_weight, 
		pawn_advance_weights,
	);

	let mobility_weight2 = 0.243;
	let center_control_weight2 = 0.437;
	let castle_bonus2 = 1.297;
	let can_castle_bonus2 = 0.893;
	let check_weight2 = 0.797;
	let piece_weights2 = [6.991176, 11.920547, 4.0755, 3.9393861, 3.0097272, 0.88426805];
	let attack_weights2 = [
		[1.7934139, 5.280421, 2.3238392, 1.7121936, 0.67339355, 1.0614241],
		[1.7934139, 5.280421, 2.3238392, 1.7121936, 0.67339355, 1.0614241],
		[2.3238392, 2.3238392, 2.3238392, 1.7121936, 0.67339355, 1.0614241],
		[1.7121936, 1.7121936, 1.7121936, 1.7121936, 0.67339355, 1.0614241],
		[0.67339355, 0.67339355, 0.67339355, 0.67339355, 0.67339355, 1.0614241],
		[1.0614241, 1.0614241, 1.0614241, 1.0614241, 1.0614241, 1.0614241],
	];
	let pawn_advance_weights2 = [0.0, 0.0, 0.0, 0.085975364, 0.17299554, 0.52598184];
	let mut bot2 = bot::Bot::new(
		max_depth,
		mobility_weight2, 
		center_control_weight2, 
		castle_bonus2, 
		can_castle_bonus2, 
		piece_weights2, 
		attack_weights2, 
		check_weight2, 
		pawn_advance_weights2,
	);

	let mut game = game::Game::new();
	play_game_early_termination(&mut game, &mut bot1, &mut bot2, 25.0, false, true);
}
