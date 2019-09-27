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

cubeurlOld = "https://www.st.com/content/st_com/en/products/development-tools/"\
    "software-development-tools/stm32-software-development-tools/"\
    "stm32-configurators-and-code-generators/stm32cubemx.html"

cubeurl = "https://www.st.com/en/development-tools/stm32cubemx.html"

with urllib.request.urlopen(cubeurl) as response:
    html = response.read().decode("utf-8")
    dlurl = re.search(r'data-download-path="(/content/ccc/resource/.*?\.zip)"', html).group(1)
    dlurl = "https://www.st.com" + dlurl
    print("Downloading CubeMX...")
    print(dlurl)

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
