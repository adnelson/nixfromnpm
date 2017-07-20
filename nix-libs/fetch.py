import os
import requests
out = os.environ['out']
url = os.environ['url']
headers = {"User-Agent": "nix-fetchurl"}
header_names = os.environ.get("headerNames", "")
for name in header_names.split():
    if "__HTTP_HEADER_{}".format(name) not in os.environ:
        exit("FATAL: no corresponding value set for header {}"
             .format(name))
    headers[name] = os.environ["__HTTP_HEADER_{}".format(name)]
print('GET {} with headers {}'.format(url, headers))
response = requests.get(url, headers=headers)
if response.status_code != 200:
    exit("Received a {} response. :(\nContent: {}"
         .format(response.status_code, response.content))
else:
    print('Response: {} ({} bytes)'
          .format(response.status_code, len(response.content)))
with open(out, 'wb') as f:
    f.write(response.content)
