#!/usr/bin/env python3

import sys
import time
import threading


output_lock = threading.Lock()
log_lock = threading.Lock()

log_fp = open("pynb.log", "a+")
def log(msg, *args):
    if args:
        msg = msg % args
    with log_lock:
        log_fp.write(msg + "\n")
        log_fp.flush()

def respond(req_id, output):
    with output_lock:
        sys.stdout.write("cmd-output %s: %d\n%s" % (req_id, len(output), output))
        sys.stdout.flush()

def executor(req_id, content):
    log("executor for req_id %r started. content:\n%s\n####" % (req_id, content))
    time.sleep(2)
    respond(req_id, "this is reponse for %r\ndone" % content)

def read_line():
    """Read a line (bytes) from stdin.buffer and decode UTF-8."""
    line = bytearray()
    while True:
        ch = sys.stdin.buffer.read(1)
        if not ch or ch == b'\n':
            break
        line.extend(ch)
    return line.decode('utf-8')

log("this is pynb executor! isatty: %r" % sys.stdin.isatty())

while True:
    line = read_line()
    if not line:
        log("empty line or EOF, exiting...")
        break
    log("got line: %r" % line)

    if line.startswith("cmd "):
        req, cmd = line.split(": ", 1)
        _, req_id = req.split(" ")

        if cmd.startswith("execute "):
            n_bytes = int(cmd.split(" ", 1)[1])
            log("n_bytes: %d" % n_bytes)

            content = sys.stdin.buffer.read(n_bytes).decode("utf-8")
            """
            content = []
            for i in range(n_bytes):
                c = sys.stdin.buffer.read(1)
                log("got char [%d]: %r" % (i, c))
                content.append(c)
            content = b"".join(content)
            content = content.decode("utf-8")
            """
            log("content: %r\n" % content)
            th = threading.Thread(target=executor, args=(req_id, content))
            th.daemon = True
            th.start()
            continue
    log("error: unknown command %r" % line)
