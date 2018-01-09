#!/usr/bin/python3

import socket
import json
import sys

class Vjk:
    def __init__(self, log, conf):
        self.log = log
        self.conf = conf
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.addr, self.port = conf["address"].split(":")
        try:
            self.sock.connect((self.addr, int(self.port)))
            self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
            self.log.debug("Vjk: Connected")
        except:
            self.sock.close()
            self.sock = None
            self.log.error("Vjk: Can't connect %s:%s" % (self.addr, self.port))

    def connected(self):
        return self.sock != None

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
        self.log.debug("Vjk: send: %s %s" % (hdr, seq))
        self.sock.send(hdr)
        self.sock.send(seq)
        recv = self.receive()
        self.log.debug("Vjk: recv: %s" % (repr(recv)))
        return recv

    def server_exit(self):
        self.log.debug("Vjk: server_exit")
        obj = { 'cmd': 'exit' }
        return self.send(obj)

    def activity_add(self, time_start, time_stop,
                     activity, category, comment):
        self.log.debug("Vjk: activity_add: %s %s %s %s %s" %
                       (repr(time_start), repr(time_stop),
                        repr(activity), repr(category), repr(comment)))
        if time_start == None or activity == None or category == None:
            return None
        data = {
                'activity': activity,
                'category': category,
                'time-start': time_start,
        }
        if time_stop:
            data['time-stop'] = time_stop
        if comment:
            data['comment'] = comment
        obj = { 'cmd': 'activity-start', 'data': data }
        return self.send(obj)

    def activity_stop(self, time_stop):
        self.log.debug("Vjk: activity_stop: %s" % (repr(time_stop)))
        if time_stop == None:
            return None
        data = { 'time-stop': time_stop }
        obj = { 'cmd': 'activity-stop', 'data': data }
        return self.send(obj)

    def activity_update(self, eid, time_start, time_stop,
                        activity, category, comment):
        self.log.debug("Vjk: activity_update: %s %s %s %s %s %s" %
                       (repr(eid), repr(time_start), repr(time_stop),
                        repr(activity), repr(category), repr(comment)))
        if eid == None:
            return None
        data = { 'id': eid }
        if activity:
            data['activity'] = activity
        if category:
            data['category'] = category
        if time_start:
            data['time-start'] = time_start
        if time_stop:
            data['time-stop'] = time_stop
        if comment:
            data['comment'] = comment
        obj = { 'cmd': 'activity-update', 'data': data }
        return self.send(obj)

    def activity_list(self, time_start, time_stop):
        self.log.debug("Vjk: activity_list: %s %s" %
                       (repr(time_start), repr(time_stop)))
        data = { }
        if time_start:
            data['time-start'] = time_start
        if time_stop:
            data['time-sttop'] = time_stop
        obj = { 'cmd': 'activity-list', 'data': data }
        return self.send(obj)

    def activity_delete(self, eid):
        self.log.debug("Vjk: activity_delete: %s" %
                       (repr(eid)))
        if eid == None:
            return None
        data = { 'id': eid }
        obj = { 'cmd': 'activity-delete', 'data': data }
        return self.send(obj)

    def category_add(self, category):
        if category == None:
            return None
        data = { 'category': category }
        obj = { 'cmd': 'category-add', 'data': data }
        return self.send(obj)

    def category_update(self, eid, category):
        if eid == None or category == None:
            return None
        data = { 'id': eid, 'category': category }
        obj = { 'cmd': 'category-update', 'data': data }
        return self.send(obj)

    def category_list(self):
        obj = { 'cmd': 'category-list' }
        return self.send(obj)

    def category_delete(self, eid):
        if eid == None:
            return None
        data = { 'id': eid }
        obj = { 'cmd': 'category-delete', 'data': data }
        return self.send(obj)
