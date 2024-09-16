from flask import Flask, request, send_from_directory, session, redirect
from pathlib import Path
import sqlite3
import secrets


app = Flask(__name__, static_url_path='/static')

app.secret_key = secrets.token_hex(16)

db = sqlite3.connect("/data/users.sqlite")

db.executescript("""CREATE TABLE IF NOT EXISTS user(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE NOT NULL,
  password TEXT NOT NULL
);""")


@app.route('/')
def index():
    if 'username' in session:
        return send_from_directory('./static/', 'loggedin.html')
    return send_from_directory('./static/', 'index.html')


@app.route('/register')
def register_form():
    return send_from_directory('./static/', 'register.html')


@app.route('/register', methods=['POST'])
def register():
    user = request.form['user']
    pw = request.form['pw']
    with db:
        db.execute('INSERT INTO user(username, password) VALUES (?, ?)', (user, pw))

        Path("/data/user").with_name(user).mkdir()
        session['username'] = user
        return redirect("/")


@app.route('/login')
def login_form():
    return send_from_directory('./static/', 'login.html')


@app.route('/login', methods=['POST'])
def login():
    user = request.form['user']
    pw = request.form['pw']
    with db:
        query = db.execute('SELECT username FROM user WHERE username = ? AND password = ?', (user, pw))

        res = query.fetchone()

        if not res:
            return 'Invalid login', 400

        session['username'] = res[0]
        return redirect("/")


@app.route('/logout', methods=['POST'])
def logout():
    del session['username']
    return redirect("/")


@app.route('/upload', methods=['POST'])
def upload():
    if 'username' not in session:
        return '', 403

    f = request.files['file']
    name = request.form['mission-name']
    if '/' in name:
        return '', 400

    savepath = Path("/data/user").with_name(session['username']) / name
    f.save(savepath)
    savepath.chmod(0o755)
    return redirect("/")


@app.route('/files')
def files():
    if 'username' not in session:
        return '', 403

    return [path.name for path in Path("/data/user").with_name(session['username']).iterdir()]


@app.get('/download/<f>')
def download(f):
    if 'username' not in session:
        return '', 403

    return send_from_directory(Path("/data/user").with_name(session['username']), f)
