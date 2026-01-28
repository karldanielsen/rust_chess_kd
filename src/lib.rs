mod game;
mod bot;
mod transposition_tables;
mod constants;
mod magic_tables;

use wasm_bindgen::prelude::*;
use std::sync::Mutex;

// Set up panic hook to show better error messages in browser console
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

// Single global game instance
static GAME: Mutex<Option<(game::Game, bot::Bot, transposition_tables::TranspositionTable)>> = Mutex::new(None);

fn init_game() {
    let mut game_guard = GAME.lock().unwrap();
    if game_guard.is_none() {
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
        
        *game_guard = Some((
            game::Game::new(),
            bot::Bot::new(
                max_depth,
                mobility_weight,
                center_control_weight,
                castle_bonus,
                can_castle_bonus,
                piece_weights,
                attack_weights,
                check_weight,
                pawn_advance_weights,
            ),
            transposition_tables::TranspositionTable::new(),
        ));
    }
}

fn board_state_to_json(game: &game::Game, bot_depth: Option<u32>) -> String {
    let state = &game.state;
    let mut board_json = String::from("[");
    for row in 0..8 {
        board_json.push_str("[");
        for col in 0..8 {
            let piece = state.board[row][col];
            let role = match piece.role {
                game::Role::Queen => "Queen",
                game::Role::King => "King",
                game::Role::Rook => "Rook",
                game::Role::Bishop => "Bishop",
                game::Role::Knight => "Knight",
                game::Role::Pawn => "Pawn",
                game::Role::Blank => "Blank",
            };
            let color = match piece.color {
                game::Color::White => "White",
                game::Color::Black => "Black",
                game::Color::Blank => "Blank",
            };
            board_json.push_str(&format!("{{\"role\":\"{}\",\"color\":\"{}\"}}", role, color));
            if col < 7 { board_json.push_str(","); }
        }
        board_json.push_str("]");
        if row < 7 { board_json.push_str(","); }
    }
    board_json.push_str("]");
    
    let mut json = format!("{{\"board\":{},\"turn\":\"{}\",\"checkmate\":{},\"white_check\":{},\"black_check\":{}",
        board_json,
        match state.turn {
            game::Color::White => "White",
            game::Color::Black => "Black",
            game::Color::Blank => "Blank",
        },
        state.checkmate,
        state.white_check,
        state.black_check
    );
    
    if let Some(depth) = bot_depth {
        json.push_str(&format!(",\"bot_depth\":{}", depth));
    }
    
    json.push_str("}");
    json
}

// Function 1: Try user move, returns updated board state JSON (or error)
#[wasm_bindgen]
pub fn try_move(from_row: usize, from_col: usize, to_row: usize, to_col: usize) -> Result<String, JsValue> {
    init_game();
    let mut game_guard = GAME.lock().unwrap();
    let (game, _, _) = game_guard.as_mut().unwrap();
    
    let mv = game::Move(
        game::Square(from_row, from_col),
        game::Square(to_row, to_col),
    );
    
    // Check if move is in the move list (basic check)
    let move_list = game.get_move_list();
    if !move_list.contains(&mv) {
        return Err(JsValue::from_str("Invalid move: not in move list"));
    }
    
    // Capture the current player's turn BEFORE making the move
    let player_who_moved = game.turn;
    
    // Make the move and validate it properly (like minimax_eval does)
    game.make_move(mv);
    
    // After making the move, check if the player who just moved is now in check (invalid move)
    // This matches the logic in minimax_eval: check the player who moved, not the current turn
    match player_who_moved {
        game::Color::White => {
            if game.white_check {
                game.reverse();
                return Err(JsValue::from_str("Invalid move: cannot move into check"));
            }
        }
        game::Color::Black => {
            if game.black_check {
                game.reverse();
                return Err(JsValue::from_str("Invalid move: cannot move into check"));
            }
        }
        game::Color::Blank => {}
    }
    
    // Move is valid, return updated board state
    Ok(board_state_to_json(game, None))
}

// Function 2: Make bot move (only if bot's turn), returns updated board state JSON
// Takes an optional callback that gets called with depth after each iterative deepening step
#[wasm_bindgen]
pub fn make_bot_move(depth_callback: Option<js_sys::Function>) -> String {
    init_game();
    let mut game_guard = GAME.lock().unwrap();
    let (game, bot, transposition_table) = game_guard.as_mut().unwrap();
    
    // Only make move if it's Black's turn (bot plays Black)
    let mut bot_depth = None;
    if game.state.turn == game::Color::Black && !game.state.checkmate {
        // Use 1 second for now to prevent freezing - can be increased later
        let (bot_move, score, depth) = match depth_callback {
            Some(ref cb) => {
                // Create a closure that wraps the JavaScript callback
                let cb = cb.clone();
                let mut callback_closure = move |depth: u32| {
                    let _ = cb.call1(&JsValue::NULL, &JsValue::from(depth));
                };
                bot.evaluate_position_with_time_limit_sync(game, transposition_table, 1, Some(&mut callback_closure))
            }
            None => {
                bot.evaluate_position_with_time_limit_sync::<fn(u32)>(game, transposition_table, 1, None)
            }
        };
        bot_depth = Some(depth);
        
        // Validate move before applying it - panic with clear error if invalid
        if bot_move.0 == bot_move.1 {
            panic!("Bot returned invalid move: from and to squares are the same ({:?}). Score: {}", bot_move, score);
        }
        
        if !game.get_is_valid_move(bot_move) {
            panic!("Bot returned invalid move: {:?}. Score: {}.", bot_move, score);
        }
        
        game.make_move(bot_move);
    }
    
    board_state_to_json(game, bot_depth)
}
