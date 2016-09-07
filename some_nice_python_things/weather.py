#! /usr/bin/python3

import pyowm
import datetime


owm = pyowm.OWM('2642ecf7132b8918b8f073910006483c', language='ru')
now = pyowm.timeutils.now().date()
tomorrow = pyowm.timeutils.tomorrow().date()


def to_human_time(unix):
    return datetime.datetime.fromtimestamp(unix)


def weather_date(weather):
    return to_human_time(weather.get_reference_time()).date()


def temperature_to_str(weather):
    rain = weather.get_rain()
    if not rain:
        rain = 'no rain'
    return "{}: {}, {}C, {}\n".format(
        to_human_time(weather.get_reference_time()).time(),
        weather.get_detailed_status(),
        weather.get_temperature('celsius')['temp'],
        rain
    )


def forecast():
    f = owm.three_hours_forecast('Voronezh,RU')
    weathers = f.get_forecast().get_weathers()

    if weather_date(weathers[0]) == now:
        print('Сегодня:\n')

        for w in (weather for weather in weathers if weather_date(weather) == now):
            print(temperature_to_str(w))

    print('Завтра:\n')

    for w in (weather for weather in weathers if weather_date(weather) == tomorrow):
        print(temperature_to_str(w))


def current_weather():
    w = owm.weather_at_place('Voronezh,RU').get_weather()

    print("""
    {}
    Temperature: {}C -- {}C  ({}C)
    Clouds: {}%
    Rain: {}
    Humidity: {}%
    Wind speed: {}m/s
    Time: {}
    """.format(
        w.get_detailed_status(),
        w.get_temperature('celsius')['temp_min'],
        w.get_temperature('celsius')['temp_max'],
        w.get_temperature('celsius')['temp'],
        w.get_clouds(),
        w.get_rain(),
        w.get_humidity(),
        w.get_wind()['speed'],
        w.get_reference_time('iso')
    ))


if __name__ == '__main__':
    import sys
    arg = '' if len(sys.argv) == 1 else sys.argv[1]
    if arg == '' or arg == '-n' or arg == '--now':
        current_weather()
    elif arg == '-f' or arg == '--forecast':
        forecast()
    else:
        print('Wrong argument')
