mod game;
mod bot;
mod transposition_tables;
mod constants;
mod magic_tables;

use std::time::Instant;
use std::error::Error;
use std::io;
use std::array;
use rand::Rng;
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
	let mut rng = rand::rng();
	new_bot.mobility_weight *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.center_control_weight *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.castle_bonus *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.can_castle_bonus *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[0] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[1] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[2] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[3] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[4] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.piece_weights[5] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[0] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[1] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[2] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[3] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[4] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.attack_weights[5] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.check_weight *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[0] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[1] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[2] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[3] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[4] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot.pawn_advance_weights[5] *= 1.0 + (rng.random_range(-1..=1) as f32 * step_pct / 100.0);
	new_bot
}

// 1  = bot 1 win
// -1 = bot 2 win
// 0  = draw
fn play_game_early_termination(game: &mut game::Game, bot1: &bot::Bot, bot2: &bot::Bot, eval_threshold: f32, print: bool) -> i32 {
	let mut transposition_table = transposition_tables::TranspositionTable::new();

	if print { println!("Starting game\n{:?}", game.state) }
	loop {
		if game.checkmate {
			if print { println!("Checkmate\n{:?}", game.state) }
			return -1;
		}

		// White Move
		let (bot_move, score) = bot1.evaluate_position(game, &mut transposition_table);
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
		let (bot_move, score) = bot2.evaluate_position(game, &mut transposition_table);
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
	let mobility_weight = 0.243;
	let center_control_weight = 0.364;
	let castle_bonus = 1.621;
	let can_castle_bonus = 1.116;
	let check_weight = 0.797;
	let piece_weights = [8.73897, 14.900683, 3.3962498, 3.2828217, 3.762159, 0.88426805];
	let attack_weights = [1.7934139, 4.4003506, 2.904799, 1.7121936, 0.67339355, 0.88452005];
	let pawn_advance_weights = [0.0, 0.0, 0.0, 0.07164613, 0.17299554, 0.6574773];
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
	bots.push((adjust_bot(base_bot.clone(), step_size), 0));
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
				let result1 = play_game_early_termination(&mut game1, &bot1, &bot2, 10.0, false);
				
				// Game 2: bot j vs bot i (swapped)
				let mut game2 = game::Game::new();
				let bot1_swap = bots_clone[i].0.clone();
				let bot2_swap = bots_clone[j].0.clone();
				let result2 = play_game_early_termination(&mut game2, &bot2_swap, &bot1_swap, 10.0, false);
				
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
		if iterations % 20 == 0 {
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
	// run_training(5);

	let max_depth = 7;
	let mobility_weight = 0.137;
	let center_control_weight = 0.377;
	let castle_bonus = 1.556;
	let can_castle_bonus = 1.157;
	let check_weight = 0.956;
	let piece_weights = [11.53544, 19.668903, 5.37966, 4.159992, 3.9006069, 0.91680914];
	let attack_weights = [1.5150762, 4.646771, 2.007797, 1.8080766, 0.38787466, 0.88452005];
	let pawn_advance_weights = [0.0, 0.0, 0.0, 0.11348749, 0.22835411, 0.47338364];
	let bot1 = bot::Bot::new(
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
	let attack_weights2 = [1.7934139, 5.280421, 2.3238392, 1.7121936, 0.67339355, 1.0614241];
	let pawn_advance_weights2 = [0.0, 0.0, 0.0, 0.085975364, 0.17299554, 0.52598184];
	let bot2 = bot::Bot::new(
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
	play_game_early_termination(&mut game, &bot1, &bot2, 50.0, true);
}
