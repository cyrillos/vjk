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
        try:
            self.sock.connect((self.addr, int(self.port)))
            self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            self.log.debug("Connected")
        except:
            self.log.error("Can't connect")
            sys.exit(1)

    def receive(self):
        try:
            seq = self.sock.recv(512)
            return json.loads(seq.decode('utf8').replace("'", '"'))
        except:
            return None

    def send(self, obj):
        seq = json.dumps(obj).encode()
        hdr = ("{\"size\":%7d}" % (len(seq))).encode('utf-8')
        self.log.debug("send: %s %s" % (hdr, seq))
        self.sock.send(hdr)
        self.sock.send(seq)
        recv = self.receive()
        self.log.debug("recv: %s" % (repr(recv)))
        return recv

    def start(self, args):
        self.log.debug("starting %s" % (repr(args)))
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        vals = re.split("@|,", args[0])
        if len(vals) > 1:
            obj = {'cmd': 'start',                  \
                   'data': {                        \
                            'activity': vals[0],    \
                            'category': vals[1],    \
                            'time': int(time.time())\
                            }                       \
                   }
            self.send(obj)
            return

    def stop(self):
        self.log.debug("stopping")
        obj = {'cmd': 'stop', 'data':       \
               { 'time': int(time.time()) }}
        self.send(obj)
        return

    def exit(self):
        self.log.debug("exiting")
        obj = {'cmd': 'exit'}
        self.send(obj)
        return

    def dts(self, s):
        h = s // 3600
        s -= h * 3600
        m = s // 60
        s -= m * 60
        return (h, m, s)

    def list_report(self, data):
        catlen = 8
        namelen = 4
        for x in data:
            if len(x['category']) > catlen:
                catlen = len(x['category'])
            if len(x['name']) > namelen:
               namelen = len(x['name'])
        #
        # id | name | category | lentgh
        fmt = "{1:<{0:}}{3:<{2:}}{5:<{4:}}{7:<{6:}}"
        print(fmt.format(6, 'ID', (namelen + 2), 'Name',
                         catlen + 2, 'Category', 10, 'Duration'))
        for x in data:
            h, m, s = self.dts(int(x['stop']) - int(x['start']))
            hms = "{0:04d}:{1:02d}:{2:02d}".format(h, m, s)
            print(fmt.format(6, x['id'], (namelen + 2), x['name'],
                             catlen + 2, x['category'], 10, hms))

    def list(self):
        self.log.debug("listing")
        obj = {'cmd': 'list'}
        recv = self.send(obj)
        if recv['status'] == 200 and 'data' in recv:
            self.list_report(recv['data'])
        return

vjkcli = Vjk(logging, conf)

if args.cmd[0] == 'start':
    vjkcli.start(args.cmd[1:])
elif args.cmd[0] == 'stop':
    vjkcli.stop()
elif args.cmd[0] == 'exit':
    vjkcli.exit()
elif args.cmd[0] == 'list':
    vjkcli.list()
else:
    logging.error("Unknown command %s" % (args.cmd[0]))
    sys.exit(1)
