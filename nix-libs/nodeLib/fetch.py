import os
import requests
out = os.environ['out']
url = os.environ['url']
headers = {"User-Agent": "nix-fetchurl"}
if os.getenv("auth"):
    headers["Authorization"] = "Bearer {}".format(os.environ["auth"])
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
