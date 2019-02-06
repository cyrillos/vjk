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

    def fini(self):
        if self.connected():
            obj = { 'cmd': 'fini' }
            self.send(obj)
            self.sock.close()
            self.sock = None
            self.log.debug("Vjk: Disconnected")

    def connected(self):
        return self.sock != None

    def receive(self):
        try:
            data = self.sock.recv(16)
            m = json.loads(data.decode('utf8').replace("'", '"'))
            if m.get('size'):
                self.log.debug("Vjk: recv size: %d" % (m.get('size')))
                data = self.sock.recv(int(m.get('size')))
                return json.loads(data.decode('utf8').replace("'", '"'))
            else:
                self.log.error("Vjk: no size obtained %s" % (repr(data)))
        except:
            return None

    def send_only(self, obj):
        try:
            seq = json.dumps(obj).encode()
            hdr = ("{\"size\":%7d}" % (len(seq))).encode('utf-8')
            self.log.debug("Vjk: send: %s %s" % (hdr, seq))
            self.sock.send(hdr)
            self.sock.send(seq)
        except:
            return None

    def send(self, obj):
        self.send_only(obj)
        try:
            recv = self.receive()
            self.log.debug("Vjk: recv: %s" % (repr(recv)))
            return recv
        except:
            return None

    def server_exit(self):
        self.log.debug("Vjk: server_exit")
        obj = { 'cmd': 'exit' }
        self.send_only(obj)

    def activity_add(self, ts_start, tz_start,
                     ts_stop, tz_stop,
                     activity, category, comment):
        self.log.debug("Vjk: activity_add: %s %s %s %s %s %s %s" %
                       (repr(ts_start), repr(tz_start),
                        repr(ts_stop), repr(tz_stop),
                        repr(activity), repr(category), repr(comment)))
        if ts_start == None or tz_start == None or \
                activity == None or category == None:
            return None
        data = {
                'activity': activity,
                'category': category,
                'tsstart': ts_start,
                'tzstart': tz_start,
        }
        if ts_stop:
            if not tz_stop:
                return None
            data['tsstop'] = ts_stop
        if tz_stop:
            if not ts_stop:
                return None
            data['tzstop'] = tz_stop
        if comment:
            data['comment'] = comment
        obj = { 'cmd': 'activity-start', 'data': data }
        return self.send(obj)

    def activity_last(self):
        self.log.debug("Vjk: activity_last")
        obj = { 'cmd': 'activity-last' }
        return self.send(obj)

    def activity_stop(self, ts_stop, tz_stop):
        self.log.debug("Vjk: activity_stop: %s %s" %
                       (repr(ts_stop), repr(tz_stop)))
        if ts_stop == None:
            return None
        data = { 'tsstop': ts_stop, 'tzstop': tz_stop }
        obj = { 'cmd': 'activity-stop', 'data': data }
        return self.send(obj)

    def activity_update(self, eid, ts_start, tz_start,
                        ts_stop, tz_stop,
                        activity, category, comment):
        self.log.debug("Vjk: activity_update: %s %s %s %s %s %s %s %s" %
                       (repr(eid), repr(ts_start), repr(tz_start),
                        repr(ts_stop), repr(tz_stop),
                        repr(activity), repr(category), repr(comment)))
        if eid == None:
            return None
        data = { 'id': eid }
        if activity:
            data['activity'] = activity
        if category:
            data['category'] = category
        if ts_start:
            data['tsstart'] = ts_start
        if tz_start:
            data['tzstart'] = tz_start
        if ts_stop:
            data['tsstop'] = ts_stop
        if tz_stop:
            data['tzstop'] = tz_stop
        if comment:
            data['comment'] = comment
        obj = { 'cmd': 'activity-update', 'data': data }
        return self.send(obj)

    def activity_list(self, ts_start=None, ts_stop=None, activity_id=None):
        self.log.debug("Vjk: activity_list: %s %s %s" %
                       (repr(ts_start), repr(ts_stop), repr(activity_id)))
        data = { }
        if activity_id:
            data['id'] = activity_id
        else:
            if ts_start:
                data['tsstart'] = ts_start
            if ts_stop:
                data['tsstop'] = ts_stop
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
