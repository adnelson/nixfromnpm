# Test the external functionality of nixfromnpm
import os
from os.path import join, realpath, dirname
import unittest
import tempfile
import shutil
import sys
from subprocess import PIPE, Popen

def printerr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

class Spec:
    def __init__(self, package, namespace=None, version=None):
        self.package = package
        self.namespace = namespace
        self.version = version

    def to_gen(self):
        result = self.package
        if self.namespace:
            result = "@" + self.namespace + "/" + result
        if self.version:
            result += "%" + self.version
        return result

    def to_build(self):
        result = self.package
        if self.namespace:
            result = "namespaces." + self.namespace + "." + result
        if self.version:
            result += "_" + self.version.replace(".", "-")
        return result


class NixFromNpmTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.run_command(["nix-build", join(dirname(__file__), "release.nix"),
                         "-A", "nixfromnpm"], "Building nixfromnpm")
        os.environ["PATH"] = join(realpath("result"), "bin") + ":" + os.environ["PATH"]

    def setUp(self):
        self.working_dir = tempfile.mkdtemp()
        self.output = "output"
        os.chdir(self.working_dir)

    def tearDown(self):
        if os.getenv("NO_CLEANUP"):
            printerr("Retaining tempdir {}".format(self.working_dir))
        else:
            shutil.rmtree(self.working_dir, ignore_errors=True)

    @staticmethod
    def run_command(cmd, comment=None):
        if comment:
            print(comment)
        print("Running command: {} ...".format(" ".join(cmd)), end="")
        proc = Popen(cmd, stdout=PIPE, stderr=PIPE)
        out, err = proc.communicate()
        if proc.wait() == 0:
            print("OK")
        else:
            print("ERROR")
            print(out.decode() or "(no stdout)")
            print(err.decode() or "(no stderr)")
            raise Exception("Command {} failed".format(" ".join(cmd)))

    def generate_and_build(self, packages, gen_args=None):
        gen_cmd = ["nixfromnpm", "-o", self.output]
        for p in packages:
            if isinstance(p, Spec):
                p = p.to_gen()
            gen_cmd.extend(("-p", p))
        if gen_args is not None:
            gen_cmd.extend(gen_args)
        self.run_command(gen_cmd)

        build_cmd = ["nix-build", self.output]
        for p in packages:
            if isinstance(p, Spec):
                p = p.to_build()
            build_cmd.extend(("-A", "nodePackages." + p))
        self.run_command(build_cmd)

    def test_help(self):
        """Test that the --help command works"""
        self.run_command(["nixfromnpm", "--help"])

    def test_package_no_dependencies(self):
        """Build a package with no dependencies"""
        self.generate_and_build(["lodash"])

    def test_package_few_dependencies(self):
        """Build a package with a few dependencies"""
        self.generate_and_build(["optimist"])

    def test_package_dev_deps(self):
        """Build a package with a dev dependencies"""
        self.generate_and_build(["coffee-script"], gen_args=["--dev-depth", "1"])

    def test_package_namespace(self):
        """Build a package in a namespace"""
        self.generate_and_build([Spec("node", namespace="types")])

    def test_extension(self):
        """Test that we can generate an extension"""
        self.generate_and_build([Spec("browserify", version="1.18.0")],
                                gen_args=["-e", "extension"])

if __name__ == "__main__":
    unittest.main()
