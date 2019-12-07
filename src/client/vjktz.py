#!/usr/bin/python3

import datetime
import calendar
import time
import re

from datetime import datetime

#
# The database stores time in four columns
#
# 'tsstart': 1575649404
# 'tzstart': 10800
#
# 'tsstop': 1575652702
# 'tzstop': 10800
#
# where tsstart and tsstop are star and stop
# times of activity in plain unix timestamp
# and tzstart, tzstop are local timezone offset.
#
# For example above it is
#
# 2019-12-06 19:23:24 as start
#
#   tsstart: 1575649404 -> Friday, December 6, 2019 16:23:24
#   tzstart: 10800 -> GMT+03:00
#
# 2019-12-06 20:18:22 as stop
#
#   tsstop: 1575652702 -> Friday, December 6, 2019 17:18:22
#   tzstop: 10800 -> GMT+03:00
#

class VjkTz:
    def __init__(self):
        '''A class for managing unix epoch taking into
        account local time offset.

        At start it caches timezone offset and operates with
        it on demand.
        '''
        #
        # Cache timezone offset
        now = self.epoch()
        utc = time.gmtime(now)
        loc = time.localtime(now)
        self.tz_offset = int(time.mktime(loc) - time.mktime(utc))

        #
        # relative time
        # -1ds, 1ds, 1ws
        self.relative_re = re.compile(r'^([\-]?)(\d+)([wdh])([s]?)$')

        #
        # long date format yyyy-mm-dd hh:mm:ss
        self.long_date_re = re.compile(r'^(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)$')

        #
        # short date format yyyy-mm-dd
        self.short_date_re = re.compile(r'^(\d\d\d\d)-(\d\d)-(\d\d)$')

    def epoch_local_relative(self, s: str) -> int:
        '''Takes offset in format [-]num[wdh][s] and returns
        a timestamp for it or -1 on error. Timezone offset
        is substracted.

        @- negative offset
        @num number (including zero)
        @w weeks
        @d days
        @h hours
        @s use today's start of a day as relative point
        to count offset from, otherwise current time is
        used'''

        if s[0] == '\\': # bash escaping before negative number
            m = self.relative_re.search(s[1:])
        else:
            m = self.relative_re.search(s)
        if not m:
            return -1

        if m.group(4) == 's':
            epoch = self.epoch_local_000000()
        else:
            epoch = self.epoch()

        num = int(m.group(2))
        if m.group(3) == 'w':       # weeks
            seconds = 7 * 24 * 60 * 60
        elif m.group(3) == 'd':     # days
            seconds = 24 * 60 * 60
        elif m.group(3) == 'h':     # hours
            seconds = 60 * 60
        else:
            seconds = 1

        return epoch - num * seconds

    def seconds_to_hms(self, seconds) -> (int,int,int):
        '''Convert seconds to hours, minutes, seconds tuple.'''

        hours = seconds // 3600
        seconds -= hours * 3600
        minutes = seconds // 60
        seconds -= minutes * 60
        return (hours, minutes, seconds)

    def epoch(self) -> int:
        '''Returns plain unix timestamp, no timezone accounted.'''

        return int(time.time())

    def tzoff(self) -> int:
        '''Returns local timezone offset.'''

        return self.tz_offset

    def epoch_local_month_day_hms(self, local_time: time.struct_time,
                                  month: int, day: int,
                                  hours: int, minutes: int, seconds: int) -> int:
        '''Adjust struct_time by  month, day, hours, minutes, seconds
        and convert it into unix timestamp substracting timezone.'''

        epoch = int(calendar.timegm((local_time.tm_year, month, day,
                                     hours, minutes, seconds,
                                     local_time.tm_wday,
                                     local_time.tm_yday,
                                     local_time.tm_isdst)))
        return epoch - self.tzoff()

    def epoch_local_hms(self, hours: int, minutes: int, seconds: int) -> int:
        '''Fetches local time and converts today's hours:minutes:seconds
        into unix timestamp substracting timezone.'''

        t = time.localtime()
        return self.epoch_local_month_day_hms(t, t.tm_mon, t.tm_mday,
                                              hours, minutes, seconds)

    def epoch_local_month(self) -> int:
        '''Fetches local time and returns timestamp for a month
        start moment substracting timezone.'''

        t = time.localtime()
        return self.epoch_local_month_day_hms(t, t.tm_mon, 1, 0, 0, 0)

    def epoch_local_week(self) -> int:
        '''Fetches local time and returns timestamp for a week
        start moment substracting timezone.'''

        # tw_day counts from 0: 0 - Mon, ..., 6 - Sun
        t = time.localtime()
        return self.epoch_local_000000() - t.tm_wday * 3600 * 24

    def epoch_local_000000(self) -> int:
        '''Fetches local time and converts today's 00:00:00
        into a unix timestamp substracting timezone.

        For example, localtime is Friday, December 6, 2019 00:00:01 GMT+03:00
        then it returns 1575579600 which is Thursday, December 5, 2019 21:00:00
        in GMT format.
        '''

        return self.epoch_local_hms(0, 0, 0)

    def epoch_local_235959(self) -> int:
        '''Fetches local time and converts today's 23:59:59
        into a unix timestamp substracting timezone.
        
        For example, localtime is Friday, December 6, 2019 21:22:22 GMT+03:00
        then it returns 1575665999 which is Friday, December 6, 2019 20:59:59
        in GMT format.
        '''

        return self.epoch_local_hms(23, 59, 59)

    def epoch_strftime(self, timestamp: int, fmt: str) -> str:
        '''Converts unix timestamp to string with by fmt.'''

        return datetime.utcfromtimestamp(timestamp).strftime(fmt)

    def epoch_strftime_long(self, timestamp: int) -> str:
        '''epoch_strftime with fmt %Y-%m-%d %H:%M:%S.'''

        return self.epoch_strftime(timestamp, '%Y-%m-%d %H:%M:%S')

    def local_strftime_long(self, timestamp: int) -> str:
        '''epoch_strftime_long but adding timezone offset'''

        return self.epoch_strftime_long(timestamp + self.tzoff())

    def epoch_strftime_short(self, timestamp: int):
        '''epoch_strftime with fmt %H:%M:%S.'''

        return self.epoch_strftime(timestamp, '%H:%M:%S')

    def local_strftime_short(self, timestamp: int):
        '''epoch_strftime but adding timezone offset'''

        return self.epoch_strftime_short(timestamp + self.tzoff())

    def epoch_local_strptime_long(self, s: str) -> int:
        '''Converts local time string %Y-%m-%d %H:%M:%S to unix
        timestamp substracting timezone. Returns -1 on error.'''

        try:
            d = datetime.strptime(s, '%Y-%m-%d %H:%M:%S')
            return int(calendar.timegm(d.timetuple())) - self.tzoff()
        except:
            return -1

    def epoch_local_strptime_short(self, s: str) -> int:
        '''Converts local time string %Y-%m-%d 00:00:00 to unix
        timestamp substracting timezone. Returns -1 on error.'''

        return self.epoch_local_strptime_long(s + " 00:00:00")

if __name__ == "__main__":
    tz = VjkTz()
    print('epoch',                      tz.epoch())

    print('epoch_local_month',          tz.epoch_local_month())
    print('epoch_local_week',           tz.epoch_local_week())
    print('epoch_local_000000',         tz.epoch_local_000000())
    print('epoch_local_235959',         tz.epoch_local_235959())

    print('epoch_local_relative -1ds',  tz.epoch_local_relative("-1ds"))
    print('epoch_local_relative -1d',   tz.epoch_local_relative("-1d"))
    print('epoch_local_relative -0ds',  tz.epoch_local_relative("-0ds"))
    print('epoch_local_relative -0d',   tz.epoch_local_relative("-0d"))

    #
    # GMT:      Friday, December 6, 2019 14:53:19
    # Local:    Friday, December 6, 2019 17:53:19 GMT+03:00
    epoch = 1575643999
    date_long_local = "2019-12-06 17:53:19"
    print('epoch_strftime_long',        tz.epoch_strftime_long(epoch))
    print('epoch_strftime_short',       tz.epoch_strftime_short(epoch))
    print('epoch_local_strptime_long',  tz.epoch_local_strptime_long(date_long_local))

    # GMT:      Thursday, December 5, 2019 21:00:00
    # Local:    Friday, December 6, 2019 0:00:00 GMT+03:00
    epoch = 1575579600
    date_short_local = "2019-12-06"
    print('epoch_local_strptime_short', tz.epoch_local_strptime_short(date_short_local))
