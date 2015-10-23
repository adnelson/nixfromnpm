import os, sys
unpack_dir = sys.argv[1]
name = sys.argv[2]
for directory, _, paths in os.walk(unpack_dir):
    if 'package.json' in paths:
        print(directory)
        exit()
exit("Couldn't find package.json file in source for {}".format(name))
