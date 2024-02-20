#!/usr/bin/env python3
# Inspired by
# https://github.com/modm-io/modm-devices/blob/develop/tools/generator/raw-data-extractor/extract-stm32.py
from pathlib import Path
from multiprocessing import Pool
import urllib.request
import zipfile
import shutil
import re
import io
import os

data_path = "../raw-device-data/stm32-devices/"
# First check STMUpdaterDefinitions.xml from this zip
update_url = "http://sw-center.st.com/packs/resource/utility/updaters.zip"
# Then Release="MX.6.2.0" maps to this: -win, -lin, -mac
cube_url = "http://sw-center.st.com/packs/resource/library/stm32cube_mx_v{}-lin.zip"

# Set the right headers
hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       'Accept-Language': 'en-US,en;q=0.8',
       'Connection': 'keep-alive'}

print("Downloading Update Info...")
print(update_url)
with urllib.request.urlopen(urllib.request.Request(update_url, headers=hdr)) as content:
    z = zipfile.ZipFile(io.BytesIO(content.read()))
    with io.TextIOWrapper(z.open("STMUpdaterDefinitions.xml"), encoding="utf-8") as defs:
        version = re.search(r'Release="MX\.(.*?)"', defs.read())
        version = version.group(1).replace(".", "")

shutil.rmtree(data_path, ignore_errors=True)
Path(data_path).mkdir(exist_ok=True, parents=True)

print("Downloading CubeMX...")
print(cube_url.format(version))
dlurl = cube_url.format(version)

os.system("nix-prefetch-url {} > cmx.hash".format(dlurl))

with open("cmx.hash") as h:
    hash = h.read()

with open("cmx.nix", "w") as f:
    f.write("{\n")
    f.write("  url = \"{}\";\n".format(dlurl))
    f.write("  sha256 = \"{}\";\n".format(hash.strip()))
    f.write("}")

os.unlink("cmx.hash")
print("Wrote new cmx.nix")
