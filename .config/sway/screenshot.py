#!/usr/bin/env python3

from pathlib import Path
import argparse
import os
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--full', help="full screen rather than region", action='store_true')
parser.add_argument('--save', help="save rather than copy to clipboard", action='store_true')
parser.add_argument('--delay', help="delay", action='store_true')
parser.add_argument('--hires', help="match hightest output scaling factor", action='store_true')
args = parser.parse_args()

screenshot_dir = str(Path.home()) + '/pictures/screenshots'
os.environ['GRIM_DEFAULT_DIR'] = screenshot_dir

command = 'grim '

if not args.full:
    command += '-g "$(slurp)" '

if not args.hires:
    command += '-s 1 '

if not args.save:
    command += '- | wl-copy'

# todo: delay

subprocess.run(command, shell=True)
