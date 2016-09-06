#! /usr/bin/python3

import pyowm
import datetime
from pprint import pprint


def to_human_time(unix_time):
    return datetime.datetime.fromtimestamp(
            int(unix_time)
    ).strftime('%Y-%m-%d %H:%M:%S')


owm = pyowm.OWM('2642ecf7132b8918b8f073910006483c', language='ru')

w = owm.weather_at_place('Voronezh,RU').get_weather()
print("""
{}
Temperature: {}-{}C
Clouds: {}%
Rain: {}
Humidity: {}%
Wind speed: {}m/s
Time: {}
""".format(
    w.get_detailed_status(),
    w.get_temperature('celsius')['temp_min'],
    w.get_temperature('celsius')['temp_max'],
    w.get_clouds(),
    w.get_rain(),
    w.get_humidity(),
    w.get_wind()['speed'],
    w.get_reference_time('iso')
))
