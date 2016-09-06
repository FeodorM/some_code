#! /usr/bin/python3

import pyowm


owm = pyowm.OWM('2642ecf7132b8918b8f073910006483c', language='ru')

forecast = owm.weather_at_place('Воронеж,Россия')
print(forecast.get_weather())
