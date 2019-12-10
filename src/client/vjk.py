#!/usr/bin/python3

import socket
import json
import sys

import vjktz

class Vjk:
    def __init__(self, log, conf):
        '''Setup logger from @log and configuration from @conf.
        Then connect to a server.'''

        self.vjktz = vjktz.VjkTz()
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
        '''Finalize Vjk. Close connection to a server.'''

        if self.connected():
            obj = { 'cmd': 'fini' }
            self.send(obj)
            self.sock.close()
            self.sock = None
            self.log.debug("Vjk: Disconnected")

    def connected(self):
        '''Test if connection to a server is established.'''
        return self.sock != None

    def receive(self):
        '''Receive packet from a server.'''

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
        '''Send packet to a server without receiving reply.'''

        try:
            seq = json.dumps(obj).encode()
            hdr = ("{\"size\":%7d}" % (len(seq))).encode('utf-8')
            self.log.debug("Vjk: send: %s %s" % (hdr, seq))
            self.sock.send(hdr)
            self.sock.send(seq)
        except:
            return None

    def send(self, obj):
        '''Send packet to a server and receive a reply.'''

        self.send_only(obj)
        try:
            recv = self.receive()
            self.log.debug("Vjk: recv: %s" % (repr(recv)))
            return recv
        except:
            return None

    def server_exit(self):
        '''Force a server to exit.'''

        self.log.debug("Vjk: server_exit")
        obj = { 'cmd': 'exit' }
        self.send_only(obj)

    def activity_add(self, ts_start, ts_stop,
                     activity, category, comment):
        '''Send new activity to a sever.'''

        self.log.debug("Vjk: activity_add: %s %s %s %s %s" %
                       (repr(ts_start), repr(ts_stop),
                        repr(activity), repr(category),
                        repr(comment)))

        if ts_start == None:
            self.log.error("Vjk: activity_add: no start epoch")
            return None

        if activity == None or category == None:
            self.log.error("Vjk: activity_add: no activity/category")
            return None

        data = {
                'activity': activity,
                'category': category,
                'tsstart': ts_start,
                'tzstart': self.vjktz.tzoff()
        }

        if ts_stop:
            data['tsstop'] = ts_stop
            data['tzstop'] = self.vjktz.tzoff()

        if comment:
            data['comment'] = comment

        obj = { 'cmd': 'activity-start', 'data': data }
        return self.send(obj)

    def activity_last(self, filter_category=None):
        '''Fetch last activity from a server.'''

        self.log.debug("Vjk: activity_last: filter_category %s" %
                       repr(filter_category))
        obj = { 'cmd': 'activity-last' }

        if filter_category:
            reply = self.category_list(name=filter_category)
            if reply.get('status') == 200 and len(reply['data']) > 0:
                catid=reply['data'][0].get('id', None)
                if catid:
                    data = {'catid': catid}
                    obj['data'] = data

        return self.send(obj)

    def activity_stop(self, ts_stop):
        '''Send stop activity record.'''

        self.log.debug("Vjk: activity_stop: %s"
                       % (repr(ts_stop)))

        if ts_stop == None:
            self.log.error("Vjk: activity_stop: no stop epoch")
            return None

        data = { 'tsstop': ts_stop, 'tzstop': self.vjktz.tzoff() }
        obj = { 'cmd': 'activity-stop', 'data': data }
        return self.send(obj)

    def activity_update(self, eid, ts_start, ts_stop,
                        activity, category, comment):
        '''Update existing activity.'''

        self.log.debug("Vjk: activity_update: %s %s %s %s %s %s" %
                       (repr(eid), repr(ts_start), repr(ts_stop),
                        repr(activity), repr(category), repr(comment)))
        if eid == None:
            self.log.error("Vjk: activity_update: no ID")
            return None

        data = { 'id': eid }

        if activity:
            data['activity'] = activity
        if category:
            data['category'] = category
        if ts_start:
            data['tsstart'] = ts_start
            data['tzstart'] = self.vjktz.tzoff()
        if ts_stop:
            data['tsstop'] = ts_stop
            data['tzstop'] = self.vjktz.tzoff()
        if comment:
            data['comment'] = comment

        obj = { 'cmd': 'activity-update', 'data': data }
        return self.send(obj)

    def activity_list(self, ts_start=None, ts_stop=None,
                      activity_id=None, catid=None):
        '''Fetch activities with params passed.'''

        self.log.debug("Vjk: activity_list: %s %s %s %s" %
                       (repr(ts_start), repr(ts_stop),
                        repr(activity_id), repr(catid)))
        data = { }

        if activity_id:
            data['id'] = activity_id
        else:
            if ts_start:
                data['tsstart'] = ts_start
            if ts_stop:
                data['tsstop'] = ts_stop

        if catid:
            data['catid'] = catid

        obj = { 'cmd': 'activity-list', 'data': data }
        return self.send(obj)

    def activity_delete(self, eid):
        '''Delete activity.'''

        self.log.debug("Vjk: activity_delete: %s" %
                       (repr(eid)))
        if eid == None:
            self.log.error("Vjk: activity_delete: no ID")
            return None

        data = { 'id': eid }
        obj = { 'cmd': 'activity-delete', 'data': data }

        return self.send(obj)

    def category_add(self, category):
        '''Add category.'''

        if category == None:
            self.log.error("Vjk: category_add: no category")
            return None

        data = { 'category': category }
        obj = { 'cmd': 'category-add', 'data': data }

        return self.send(obj)

    def category_update(self, eid, category):
        '''Edit category.'''

        if eid == None or category == None:
            self.log.error("Vjk: category_update: no ID or category")
            return None

        data = { 'id': eid, 'category': category }
        obj = { 'cmd': 'category-update', 'data': data }

        return self.send(obj)

    def category_list(self, name=None):
        '''List categories.'''

        obj = { 'cmd': 'category-list' }
        if name:
            obj['data'] = { 'name': name }

        return self.send(obj)

    def category_delete(self, eid):
        '''Delete category.'''

        if eid == None:
            self.log.error("Vjk: category_delete: no ID")
            return None

        data = { 'id': eid }
        obj = { 'cmd': 'category-delete', 'data': data }

        return self.send(obj)
