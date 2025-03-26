initialize_db <- function() {
    conn <- dbConnect(SQLite(), "./db/data.sqlite")

    # Create tables if they don't exist
    dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password TEXT,
      email TEXT,
      full_name TEXT,
      role TEXT DEFAULT 'user',
      is_active INTEGER DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      last_login DATETIME
    )")

    dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS roles (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      role_name TEXT UNIQUE,
      permissions TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

    dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS user_roles (
      user_id INTEGER,
      role_id INTEGER,
      FOREIGN KEY(user_id) REFERENCES users(id),
      FOREIGN KEY(role_id) REFERENCES roles(id)
    )")

    # Create default admin user if not exists
    if (!nrow(dbGetQuery(conn, "SELECT * FROM users WHERE username = 'admin'"))) {
        hashed_password <- sodium::password_store("admin123")
        dbExecute(
            conn,
            "INSERT INTO users (username, password, role, full_name, email)
       VALUES ('admin', ?, 'admin', 'Admin User', 'admin@example.com')",
            list(hashed_password)
        )
    }

    dbDisconnect(conn)
}

# Initialize database when app starts
initialize_db()

# Authentication functions
validate_password <- function(stored_hash, input_password) {
    tryCatch(
        {
            sodium::password_verify(stored_hash, input_password)
        },
        error = function(e) FALSE
    )
}

hash_password <- function(password) {
    sodium::password_store(password)
}



#INSERT INTO roles (role_name, permissions) VALUES
#  ('admin', 'user_management,role_management,data_preview,settings,dash_explore_clean'),
#  ('user', 'data_preview,dash_explore_clean');