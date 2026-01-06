mod game;
mod bot;

use std::error::Error;
use std::io;


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
	
fn main() -> () {
    let mut game = game::Game::new();
	println!("\nWelcome to this Chess App. Rather than standard Chess format for moves, please enter moves as zero-indexed coordinates. For example, 01,22");

	// loop {
	// 	println!("{:?}", game);
	// 	if game.get_is_checkmate() {
	// 		println!("{} is in Checkmate. Game over.", game.get_turn());
	// 		break;
	// 	}
	// 	let mut mv = get_user_move();
	// 	loop {
	// 		match mv {
	// 			Err(_e) => { mv = get_user_move(); }
	// 			Ok(m) => {
	// 				if game.get_is_valid_move(m) {
	// 					game.make_move(mv.unwrap());
	// 					break
	// 				}
	// 				println!("Invalid move");
	// 				mv = get_user_move();
	// 			}
	// 		}
	// 	}


	// 	if game.get_is_checkmate() {
	// 		println!("{} is in Checkmate. Game over.", game.get_turn());
	// 		break;
	// 	}

	// 	let (bot_move, _score) = bot::minimax_eval(&mut game, 0);
	// 	game.make_move(bot_move);
	// }

	// [Queen, King, Rook, Bishop, Knight, Pawn]
	let bot1_attack_weights = [3.0, 2.0, 1.0, 0.5, 0.5, 0.3];

	// max_depth, mobility_weight, square_control_weights, castle_bonus, can_castle_bonus, piece_weights, attack_weights, check_weight, minimax_prune_threshold
	let bot1 = bot::Bot::new(5, 0.1, bot::DEFAULT_SQUARE_CONTROL_WEIGHTS, 5.0, 3.0, bot::DEFAULT_PIECE_WEIGHTS, bot1_attack_weights, 1.0, 3.0);
	let bot2 = bot::Bot::new(5, 0.2, bot::DEFAULT_SQUARE_CONTROL_WEIGHTS, 10.0, 1.0, bot::DEFAULT_PIECE_WEIGHTS, bot::DEFAULT_ATTACK_WEIGHTS, 2.0, 5.1);
		
	loop {
		println!("{:?}", game);
		if game.checkmate {
			println!("{} is in Checkmate. Game over.", game.get_turn());
			break;
		}

		let (bot_move, score) = bot1.minimax_eval(&mut game, 0, f32::MIN, f32::MAX);
		if bot_move.0 == bot_move.1 {
			println!("Draw. Game over.");
			break;
		}

		println!("Bot 1 move: {:?}, score: {}", bot_move, score);
		game.make_move(bot_move);

		println!("{:?}", game);
		if game.checkmate {
			println!("{} is in Checkmate. Game over.", game.get_turn());
			break;
		}


		let (bot_move, score) = bot2.minimax_eval(&mut game, 0, f32::MIN, f32::MAX);
		if bot_move.0 == bot_move.1 {
			println!("Draw. Game over.");
			break;
		}

		println!("Bot 2 move: {:?}, score: {}", bot_move, score);
		game.make_move(bot_move);

		if bot2.check_for_repetition(&game) {
			println!("Draw by repetition. Game over.");
			break;
		}
	}
}
