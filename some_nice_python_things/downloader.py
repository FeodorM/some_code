from urllib.request import urlretrieve
from re import findall

with open('bl.htm') as f:
    urls = findall(
        r'<input id="[a-z0-9_]+" value="([^"]+)"',
        f.read()
    )
names = []      # Сюда введи названия песен так, чтобы получилось ["группа 1", "second band"]
bands = []      # А сюда аналогично названия групп
folder = r""    # А здесь название папки (между кавычками)
song_name = '{folder}\\{band} - {name}.mp3'
for name, band, url in zip(names, bands, urls):
    urlretrieve(url, song_name.format(
        band=band, name=name, folder=folder))
