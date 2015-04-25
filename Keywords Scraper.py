import urllib
import re
import csv
import pandas
import bs4
import nltk

urls = []
http = []

f = open('C:\Users\mdrub_000\Desktop\Data Science Project\\scrape.csv') ##Change path
data = csv.reader(f)

urls = []

for row in data:
    urls.append(row[0]) ##Change column depending on where it is

urls.pop(0)

text = []

for i in urls:
    try:
        html = urllib.urlopen(i).read()
    except IOError:
        pass
    try:
        soup = bs4.BeautifulSoup(html)
        soup.get_text().encode('utf-8')
        text.append(soup)
    except UnicodeEncodeError:
        pass
    
df = pandas.DataFrame()
df['website'] = urls
df['text'] = text

df.to_csv(path_or_buf = "C:\Users\mdrub_000\Desktop\Data Science Project\htmlscrape.csv", sep = ",", index = 'FALSE') 

