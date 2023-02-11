#!/usr/bin/env python3

#NOTE: Highly inspired by https://gitlab.com/tsoding/porth/-/blob/master/test.py

import sys
import os
from os import path
import subprocess
from dataclasses import dataclass
from typing import List, BinaryIO

def read_str_field(f: BinaryIO, name: bytes) -> bytes:
    line = f.readline()
    field = b':b ' + name + b' '
    assert line.startswith(field)
    assert line.endswith(b'\n')
    size = int(line[len(field):-1])
    blob = f.read(size)
    assert f.read(1) == b'\n'
    return blob


def read_int_field(f: BinaryIO, name: bytes) -> int:
    line = f.readline()
    field = b':i ' + name + b' '
    return int(line[len(field):-1])

def write_int_field(f: BinaryIO, name: bytes, value: int):
    f.write(b':i %s %d\n' % (name, value))

def write_str_field(f: BinaryIO, name: bytes, blob: bytes):
    f.write(b':b %s %d\n' % (name, len(blob)))
    f.write(blob)
    f.write(b'\n')

@dataclass
class Case:
    exit_code: int = 0
    stdout: bytes = bytes()
    stderr: bytes = bytes()

def read_case(path: str) -> Case:
    try:
        with open(path, "rb") as f:
            exit_code = read_int_field(f, b'exit_code')
            stdout = read_str_field(f, b'stdout')
            stderr = read_str_field(f, b'stderr')
            return Case(exit_code, stdout, stderr)
    except FileNotFoundError:
        return None

def write_case(path: str, exit_code: int, stdout: bytes, stderr: bytes):
    with open(path, "wb") as f:
        write_int_field(f, b'exit_code', exit_code)
        write_str_field(f, b'stdout', stdout)
        write_str_field(f, b'stderr', stderr)

@dataclass
class Stats:
    failed: int
    failed_files: List[str]

def run_for_file(path: str, stats: Stats = Stats(0, [])):
    print(f"testing {path}")
    tc_path = path[:-len(".pcp")] + ".txt"
    tc = read_case(tc_path)

    if tc is not None:
        stdout = b''
        stderr = b''
        exit_code = 0
        com = subprocess.run(["./pcp", path], capture_output=True)
        stderr = com.stderr
        if com.returncode != 0:
            exit_code = com.returncode
        else:
            run = subprocess.run(["tcc", "-run", "tests/out.c"], capture_output=True)
            exit_code = run.returncode
            stdout = run.stdout
        if exit_code != tc.exit_code or stdout != tc.stdout or stderr != tc.stderr:
            print("unexpected output")
            print("  expected:")
            print("    return code: %s" % tc.exit_code)
            print("    stdout: \n%s" % tc.stdout.decode("utf-8"))
            print("    stderr: \n%s" % tc.stderr.decode("utf-8"))
            print("  actual:")
            print("    return code: %s" % exit_code)
            print("    stdout: \n%s" % stdout.decode("utf-8"))
            print("    stderr: \n%s" % stderr.decode("utf-8"))
            stats.failed += 1
            stats.failed_files.append(path)

def run_for_folder(folder: str):
    stats = Stats(0, [])
    for entry in os.scandir(folder):
        if entry.path.endswith(".pcp"):
            run_for_file(entry.path, stats)
    print()
    print(f"failed: {stats.failed}")
    if stats.failed != 0:
        print("failed files:\n")
        for failed_file in stats.failed_files:
            print(failed_file)
        exit(1)

def update_output_for_file(path: str):
    tc_path = path[:-len(".pcp")] + ".txt"
    tc = read_case(tc_path) or Case()
    stdout = b''
    stderr = b''
    exit_code = 0
    com = subprocess.run(["./pcp", path], capture_output=True)
    stderr = com.stderr
    if com.returncode != 0:
        exit_code = com.returncode
    else:
        run = subprocess.run(["tcc", "-run", "tests/out.c"], capture_output=True)
        exit_code = run.returncode
        stdout = run.stdout
    print(f"saving output to {tc_path}")
    write_case(tc_path, exit_code, stdout, stderr)

def update_output_for_folder(folder: str):
    for entry in os.scandir(folder):
        if entry.path.endswith(".pcp"):
            update_output_for_file(entry.path)


if __name__ == "__main__":
    exe, *argv = sys.argv

    subcommand = "run"

    if len(argv) > 0:
        subcommand, *argv = argv

    if subcommand == "update":
        update_output_for_folder("tests/")
    elif subcommand == "run":
        run_for_folder("tests/")

