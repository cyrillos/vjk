#!/usr/bin/python3

import argparse
import logging
import pprint
import socket
import json
import time
import sys
import os
import re

def get_loglevel(num):
    lvl_nums = {
        4: logging.DEBUG,
        3: logging.DEBUG,
        2: logging.INFO,
        1: logging.WARNING,
        0: logging.ERROR,
    }
    if num in lvl_nums:
        return lvl_nums[num]
    return logging.ERROR

parser = argparse.ArgumentParser()

parser.add_argument('cmd',
                    nargs = '*',
                    help = 'activity')
parser.add_argument('--conf', dest = 'conf',
                    default = 'conf/vjk.json',
                    help = 'configuration file in JSON format')
parser.add_argument('-v', dest = 'verb',
                    default = 1,
                    help = 'verbosity level [0-4]')
args = parser.parse_args()

conf = []

if len(args.cmd) < 1:
        logging.error("Supply command")
        sys.exit(1)

if args.conf != None and os.path.isfile(args.conf):
    with open(args.conf) as f:
        conf = json.load(f)
else:
    if 'VJKCONF' in os.environ:
        with open(os.environ['VJKCONF']) as f:
            conf = json.load(f)
    else:
        logging.error("Provide server configuration")
        sys.exit(1)

if 'loglevel' not in conf:
    loglevel = logging.DEBUG
else:
    loglevel = get_loglevel(conf['loglevel'])

logging.basicConfig(format = '%(asctime)s %(filename)s %(funcName)s %(message)s',
                    datefmt = '%m/%d/%Y %H:%M:%S', level = loglevel)

logging.debug('Configuration')
logging.debug(pprint.pformat(conf))
logging.debug(pprint.pformat(args))

class Vjk:
    def __init__(self, log, conf):
        self.log = log
        self.conf = conf
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.addr, self.port = conf["address"].split(":")
        self.sock.connect((self.addr, int(self.port)))
    def start(self, args):
        self.log.debug("starting %s" % (repr(args)))
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        vals = re.split("@|,", args[0])
        if len(vals) > 1:
            obj = {'cmd': 'start',          \
                   'activity': vals[0],     \
                   'category': vals[1],     \
                   'start': int(time.time())}
            if len(vals) == 3:
                obj['comment'] = vals[2]
            jv = json.dumps(obj)
            self.log.debug("\t%s" % (jv))
            self.sock.send(jv.encode())
            self.sock.close()
    def stop(self):
        self.log.debug("stopping")
        obj = {'cmd': 'stop',               \
               'stop': int(time.time())}
        jv = json.dumps(obj)
        self.log.debug("\t%s" % (jv))
        self.sock.send(jv.encode())
        self.sock.close()
    def list(self):
        self.log.debug("listing")
        self.sock.close()

vjkcli = Vjk(logging, conf)
if args.cmd[0] == 'start':
    vjkcli.start(args.cmd[1:])
elif args.cmd[0] == 'stop':
    vjkcli.stop()
elif args.cmd[0] == 'list':
    vjkcli.list()
else:
    logging.error("Unknown command %s" % (args.cmd[0]))
    sys.exit(1)
