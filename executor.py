#!/usr/bin/env python3

import os
import sys
import time
import threading


output_lock = threading.Lock()
log_lock = threading.Lock()

executing_jobs = {} # cmd-id -> Job
cancelled_jobs = {} # cmd-id -> Job

log_fp = open("pynb.log", "a+")
def log(msg, *args):
    if args:
        msg = msg % args
    with log_lock:
        log_fp.write(msg + "\n")
        log_fp.flush()

def respond(req_id, output):
    with output_lock:
        if req_id in cancelled_jobs:
            raise Exception("this job cmd-id %r is cancelled!" % req_id)
        log("sending output for %r: %r" % (req_id, output))
        sys.stdout.write("cmd-output %s: %d\n%s" % (req_id, len(output), output))
        sys.stdout.flush()

class Job:
    def __init__(self, cmd_id, content):
        self._aborted = False
        self.cmd_id = cmd_id
        self.content = content

        self.thread = threading.Thread(target=self.runner)
        self.thread.daemon = True
        self.thread.start()

    def runner(self):
        log("executor for self.cmd_id %r started. content:\n%s\n####" % (self.cmd_id, content))
        self.sleep(1)
        self.output("this is reponse for %r -- started!\n2nd line, no newline" % self.cmd_id)
        self.sleep(1)
        self.output("after 2s, should be on 2nd line. now newline:\n")
        self.sleep(1)
        self.output("after 3s with newline\n")
        self.sleep(1)
        self.output("done - no newline")
        log("did send response")

    def output(self, text):
        respond(self.cmd_id, text)

    def abort(self):
        self._aborted = True

    def sleep(self, t):
        deadline = time.time() + t
        while time.time() < deadline:
            if self._aborted:
                raise Exception("job %r aborted." % self.cmd_id)
            remaining = deadline - time.time()
            if remaining < 0:
                break
            to_sleep = min(remaining, 0.5)
            time.sleep(to_sleep)

def read_line():
    """Read a line (bytes) from stdin.buffer and decode UTF-8."""
    line = bytearray()
    while True:
        ch = sys.stdin.buffer.read(1)
        if not ch or ch == b'\n':
            break
        line.extend(ch)
    return line.decode('utf-8')

log("this is pynb executor %d! isatty: %r" % (os.getpid(), sys.stdin.isatty()))

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
            log("content: %r\n" % content)
            executing_jobs[req_id] = Job(req_id, content)
            continue
        if cmd == "abort":
            if req_id in executing_jobs:
                job = executing_jobs.pop(req_id)
                with output_lock:
                    cancelled_jobs[req_id] = job
                job.abort()
                log("did abort cmd-id %r" % req_id)
            else:
                log("can not abort unknown cmd-id %r" % req_id)
            continue

    log("error: unknown command %r" % line)
