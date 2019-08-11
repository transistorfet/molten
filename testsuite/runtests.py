#!/usr/bin/python3

import re
import os
import os.path
import argparse
import subprocess

CLEAR   = "\x1B[0m"
RED     = "\x1B[1;31m"
GREEN   = "\x1B[32m"
YELLOW   = "\x1B[93m"

rootdir = ".."
testdir = "testsuite"

def main():
    global rootdir
    rootdir = find_rootdir()
    os.chdir(rootdir)

    parser = argparse.ArgumentParser(prog='runtests.py', formatter_class=argparse.ArgumentDefaultsHelpFormatter, description='Run molten test suite')
    parser.add_argument("directory", nargs='?', default=testdir)
    parser.add_argument('-f', '--force', action='store_true', help='Force a recompile of all files')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print extended info on why a test fails')
    parser.add_argument('-n', '--no-color', action='store_true', help='Remove color codes from output')
    parser.add_argument('-F', '--failed-only', action='store_true', help='Show only the failed tests')
    parser.add_argument('-s', '--show-output', action='store_true', help='Show the output of the compiler')
    args = parser.parse_args()

    if args.no_color:
        global CLEAR, RED, GREEN, YELLOW
        (CLEAR, RED, GREEN, YELLOW) = ('', '', '', '')

    if args.directory == "clean":
        os.system("./molten cleanall {directory}".format(directory=testdir))
    elif os.path.isfile(args.directory):
        test = Test(args.directory, args)
        test.run_test(args.force)
    else:
        os.system("date")
        run_all(args)


def run_all(args):
    #dirname = os.path.join(testdir, args.directory)
    dirname = args.directory

    testfiles = [ ]
    for (basedir, dirnames, filenames) in os.walk(dirname, topdown=True):
        dirnames.sort()
        for filename in sorted(filenames):
            if filename.endswith(".ml") or filename.endswith(".mol"):
                testfiles.append( (os.path.join(basedir, filename), len(dirname)+1) )

    total = 0
    passed = 0
    for i, (path, short) in enumerate(testfiles):
        total += 1
        test = Test(path, args)
        if not args.failed_only:
            print("[{:02.0f}/{:02.0f}] ".format(i + 1, len(testfiles)), end="")
        if test.run_test(args.force, short=short):
            passed += 1
    print("{G}{}{C} tests passed, {R}{}{C} tests failed, {} total".format(passed, total - passed, total, R=RED, G=GREEN, C=CLEAR))


def find_rootdir():
    for rootdir in [ "..", "." ]:
        if os.path.exists(os.path.join(rootdir, "Cargo.toml")):
            return rootdir
    raise Exception("Unable to locate compiler source code directory")


class Test (object):
    def __init__(self, path, args):
        self.verbose = args.verbose
        self.failed_only = args.failed_only
        self.show_output = args.show_output
        self.path = path
        self.expected_ret = True
        self.expected_out = []
        self.expected_err = []
        self.expected_dec = []
        self.load_expected()

    def load_expected(self):
        with open(self.path, 'r') as f:
            for line in f.readlines():
                #line = line.lstrip()
                if line.startswith("//!should_fail"):
                    self.expected_ret = False
                elif line.startswith("//! "):
                    self.expected_out.append(line[4:].rstrip())
                elif line.startswith("//!err "):
                    self.expected_err.append(line[7:].rstrip())
                elif line.startswith("//!dec "):
                    self.expected_dec.append(line[7:].rstrip())

    def load_dec(self):
        self.dec_output = b""
        (path, ext) = os.path.splitext(self.path)
        try:
            with open(path + ".dec", 'rb') as f:
                self.dec_output = f.read()
        except:
            pass

    def compare_lines(self, name, output):
        expected = getattr(self, name)
        output = output.decode("utf-8").rstrip('\n').split('\n')
        #print(output, expected)
        if len(expected) > 0:
            if len(output) != len(expected):
                if self.verbose:
                    print(output, expected)
                    print(YELLOW, "Expected:", len(expected), "lines of", name, ", found:", len(output), CLEAR)
                return False
            for (exp, out) in zip(expected, output):
                if re.fullmatch(exp, out) is None:
                    if self.verbose:
                        print(YELLOW, "Expected:", repr(exp), ", found:", repr(out), CLEAR)
                    return False
        return True

    def print_diff(self, name, output):
        output = output.decode("utf-8").split('\n')
        for (exp, out) in zip(getattr(self, name), output):
            print(out, " " * (64 - len(out)), exp)

    def check_result(self, retcode, stdout, stderr):
        if self.expected_ret and retcode == 0 and self.compare_lines('expected_out', stdout) and self.compare_lines('expected_dec', self.dec_output):
            return True
        elif not self.expected_ret and retcode != 0 and self.compare_lines('expected_err', stdout):
            return True
        else:
            return False

    def print_name(self, short):
        name = self.path[short:]
        print(name + ("." * (64 - len(name))), end="")

    def run_test(self, force, short=0):
        target = os.path.splitext(self.path)[0] + ".bin"
        (retcode, stdout, stderr) = runcmd("./molten {flags} run {file} -o {target}".format(flags='-f' if force else '', file=self.path, target=target), shell=True)
        self.load_dec()

        if self.check_result(retcode, stdout, stderr):
            if not self.failed_only:
                self.print_name(short)
                print(GREEN + "success" + CLEAR)
            return True
        else:
            self.print_name(short)
            print(RED + "FAIL" + CLEAR)
            if self.show_output:
                print("StdOut:\n")
                print(stdout.decode('utf-8'))
                print("StdError:\n")
                print(stderr.decode('utf-8'))
            #if retcode == 0:
            #    print("Output:", " " * (64 - len("Output:")), "Expected:")
            #    if self.expected_out:
            #        self.print_diff('expected_out', stdout)
            #    if self.expected_dec:
            #        self.print_diff('expected_dec', self.dec_output)
            return False



def runcmd(cmd, shell=False):
    result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=shell)
    return (result.returncode, result.stdout, result.stderr)


if __name__ == "__main__":
    main()

