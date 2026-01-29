# Rust Chess - WebAssembly

A chess game built in Rust, compiled to WebAssembly, with a simple drag-and-drop frontend.

## Building

```bash
./build.sh
```

This creates a `pkg/` directory with the compiled WebAssembly module.

## Running Locally

WebAssembly modules must be served over HTTP/HTTPS (not `file://`). For local development, use any simple HTTP server:

```bash
# Python
python3 -m http.server 8000

# Node.js
npx http-server -p 8000

# PHP
php -S localhost:8000

# Or use VS Code's Live Server extension
```

Then open `http://localhost:8000/index.html`

## Static Hosting

The built files (`pkg/` directory and `index.html`) can be hosted on **any static hosting service**:

- **GitHub Pages**: Push to a repo, enable Pages
- **Netlify**: Drag and drop the `pkg/` folder and `index.html`
- **Vercel**: `vercel --prod`
- **Cloudflare Pages**: Connect repo or upload files
- **Any web server**: Just upload the files

No backend needed - it's 100% static!

## Architecture

**Rust Backend** (`src/lib.rs`):
- Only exposes 2 functions to JavaScript:
  - `try_move(from_row, from_col, to_row, to_col)` - Validates and applies user move
  - `make_bot_move()` - Makes bot move and returns updated state

**Frontend** (`index.html`):
- Drag-and-drop chess board
- Calls the two Rust functions
- All UI logic in JavaScript

The Rust game engine stays completely internal - no types or structs exposed to JS.
