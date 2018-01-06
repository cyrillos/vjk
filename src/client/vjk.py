#!/usr/bin/python3

import argparse
import datetime
import logging
import pprint
import socket
import json
import time
import sys
import os
import re

from datetime import datetime

def today_starts_unix_time():
    t = time.gmtime()
    return int(time.mktime((t.tm_year, t.tm_mon, t.tm_mday, 0, 0, 0,
                            t.tm_wday, t.tm_yday, t.tm_isdst)))

def unix_time():
    return int(time.time())

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

parser = argparse.ArgumentParser(prog='vjk.py')

parser.add_argument('--conf', dest = 'conf',
                    default = 'conf/vjk.json',
                    help = 'configuration file in JSON format')

sp = parser.add_subparsers(dest = 'cmd')
for cmd in ['start']:
    spp = sp.add_parser(cmd, help = 'Start new activity')

for cmd in ['stop']:
    spp = sp.add_parser(cmd, help = 'Stop current activity')

for cmd in ['list']:
    spp = sp.add_parser(cmd, help = 'List activities/categories')
    spp.add_argument('--category', dest = 'category',
                     help = 'Categories mode', action = 'store_true')
    spp.add_argument('--from', dest = 'start',
                     help = 'Time to report from. Format [-|+]number[d|h|m])')
    spp.add_argument('--to', dest = 'stop',
                     help = 'Time to report until. Format [-|+]number[d|h|m])')

for cmd in ['add']:
    spp = sp.add_parser(cmd, help = 'Add activities/categories')
    spp.add_argument('--category', dest = 'category',
                     help = 'Categories mode', action = 'store_true')
    spp.add_argument('--start', dest = 'start',
                     help = 'Date and time activity started at. \
                     Format [YYYY-MM-DD] hh:mm:ss.')
    spp.add_argument('--stop', dest = 'stop',
                     help = 'Date and time activity stopped at. \
                     Format [YYYY-MM-DD] hh:mm:ss.')

for cmd in ['edit']:
    spp = sp.add_parser(cmd, help = 'Edit activity/category')
    spp.add_argument('--category', dest = 'category',
                     help = 'Categories mode', action = 'store_true')
    spp.add_argument('--id', dest = 'id', help = 'Activity/category ID',
                     required = True)

for cmd in ['delete']:
    spp = sp.add_parser(cmd, help = 'Delete activity/category')
    spp.add_argument('--category', dest = 'category',
                     help = 'Categories mode', action = 'store_true')
    spp.add_argument('--id', dest = 'id', help = 'Activity/category ID',
                     required = True)

for cmd in ['exit']:
    spp = sp.add_parser(cmd, help = 'Stop server')

args, unknown_args = parser.parse_known_args()
if args.cmd == None:
    parser.print_help()
    sys.exit(1)

conf = []

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
            # FIXM 16K might be not enough
            seq = self.sock.recv(16 << 10)
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

    def activity_start(self, args):
        self.log.debug("starting %s" % (repr(args)))
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        vals = re.split("@|,", args[0])
        if len(vals) >= 2:
            data = {
                'activity': vals[0],
                'category': vals[1],
                'time': int(time.time())
            }
            if len(vals) > 2:
                data['comment'] = vals[2]
            obj = { 'cmd': 'activity-start', 'data': data }
            self.send(obj)
            return

    def activity_stop(self):
        self.log.debug("stopping")
        obj = {
            'cmd': 'activity-stop',
            'data': {
                'time': int(time.time())
            }
        }
        self.send(obj)
        return

    def activity_edit(self, entry_id, args):
        self.log.debug("activity editing id %s %s" % (entry_id, repr(args)))
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        vals = re.split("@|,", args[0])
        if len(vals) >= 2:
            data = {
                'id': int(entry_id),
                'activity': vals[0],
                'category': vals[1],
            }
            if len(vals) > 2:
                data['comment'] = vals[2]
            if len(vals) > 3:
                data['time-start'] = vals[3]
            if len(vals) > 4:
                data['time-stop'] = vals[4]
            obj = { 'cmd': 'activity-update', 'data': data }
            self.send(obj)
            return

    def category_edit(self, entry_id, args):
        self.log.debug("category editing id %s %s" % (entry_id, repr(args)))
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        data = {
            'id': int(entry_id),
            'category': args[0],
        }
        obj = { 'cmd': 'category-update', 'data': data }
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

    def strftime(self, ts):
        return datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')

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
        fmt = "{1:<{0:}}{3:<{2:}}{5:<{4:}}{7:<{6:}}{9:<{8:}}"
        print(fmt.format(6, 'ID', (namelen + 2), 'Name',
                         catlen + 2, 'Category',
                         22, 'Date',
                         14, 'Duration',
                         16, 'Comment'))
        for x in data:
            if (not 'stop' in x) or x['stop'] == None:
                x['stop'] = int(time.time())
                sign = ' *'
            else:
                sign = ' '
            if (not 'comment' in x) or x['comment'] == None:
                x['comment'] = ''
            h, m, s = self.dts(int(x['stop']) - int(x['start']))
            hms = "{0:04d}:{1:02d}:{2:02d}{3:2s}".format(h, m, s, sign)
            print(fmt.format(6, x['id'], (namelen + 2), x['name'],
                             catlen + 2, x['category'],
                             22, self.strftime(int(x['start'])),
                             14, hms,
                             16, x['comment']))

    def tf_to_ts(self, ts):
        if ts != None:
            m = re.search('([\+\-]?)([0-9]+)([wdhm]?)', ts)
            if m:
                if not m.group(2):
                    return None
                val = int(m.group(2))
                if m.group(3) == 'w':
                    step = 7 * 24 * 60 * 60
                elif m.group(3) == 'd':
                    step = 24 * 60 * 60
                elif m.group(3) == 'h':
                    step = 60 * 60
                elif m.group(3) == 'm':
                    step = 60
                else:
                    step = 1
                current = unix_time()
                if m.group(1) == '-':
                    ts = current - val * step
                else:
                    ts = current + val * step
        return ts

    def activity_list(self, start, stop):
        self.log.debug("listing")
        data = { }
        start = self.tf_to_ts(start)
        if start == None:
            start = today_starts_unix_time()
        data['time-start'] = start
        stop = self.tf_to_ts(stop)
        if stop != None:
            data['time-stop'] = stop
        obj = {'cmd': 'activity-list', 'data': data}
        recv = self.send(obj)
        if recv['status'] == 200 and 'data' in recv:
            self.list_report(recv['data'])
        return

    def list_categories(self, data):
        catlen = 8
        for x in data:
            if len(x['category']) > catlen:
               catlen = len(x['category'])
        #
        # id | name | category | lentgh
        fmt = "{1:<{0:}}{3:<{2:}}"
        print(fmt.format(6, 'ID', catlen + 2, 'Category'))
        for x in data:
            print(fmt.format(6, x['id'], catlen + 2, x['category']))

    def category_list(self):
        self.log.debug("category_list")
        obj = {'cmd': 'category-list'}
        recv = self.send(obj)
        if recv['status'] == 200 and 'data' in recv:
            self.list_categories(recv['data'])
        return

    def category_add(self, args):
        self.log.debug("category_add")
        if len(args) < 1:
            self.log.error("No arguments passed")
            return
        obj = {'cmd': 'category-add', 'data': {'category': args[0]}}
        recv = self.send(obj)
        return

    def delete(self, entry_id, is_category):
        self.log.debug("delete")
        obj = { 'data': { 'id': int(entry_id) }}
        if is_category:
            obj['cmd'] = 'category-delete'
        else:
            obj['cmd'] = 'activity-delete'
        self.send(obj)
        return

vjkcli = Vjk(logging, conf)

if args.cmd == 'start':
    vjkcli.activity_start(unknown_args)

if args.cmd == 'stop':
    vjkcli.activity_stop()

if args.cmd == 'exit':
    vjkcli.exit()

if args.cmd == 'list':
    if args.category == False:
        vjkcli.activity_list(args.start, args.stop)
    else:
        vjkcli.category_list()

if args.cmd == 'add':
    if args.category == False:
        vjkcli.activity_add(args.id, unknown_args)
    else:
        vjkcli.category_add(unknown_args)

if args.cmd == 'edit':
    if args.category == False:
        vjkcli.activity_edit(args.id, unknown_args)
    else:
        vjkcli.category_edit(args.id, unknown_args)

if args.cmd == 'delete':
    vjkcli.delete(args.id, args.category)
