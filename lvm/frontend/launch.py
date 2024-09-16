from pathlib import Path
import sqlite3
import sys
import os

username = input("Username: ")
password = input("Password: ")

db = sqlite3.connect("/data/users.sqlite")
query = db.execute('SELECT username FROM user WHERE username = ? AND password = ?', (username, password))

res = query.fetchone()
if not res:
    print("Invalid login")
    sys.exit(0)

filename = input("File to run: ")
if '/' in filename:
    print("Invalid file")
    sys.exit(0)

path = Path("/data/user").with_name(username) / filename

os.execve("/opt/lvm/lisp", ["/opt/lvm/lisp", path], {})
