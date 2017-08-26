#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# acv2gimp.py: convert photoshop curve(.acv) to gimp curve
#              base on acv2gimp.pl
#              (from http://www.big-bubbles.fluff.org/scripts/acv2gimp)

# Copyright (c) 2008 Homin Lee <ff4500@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import os, sys, glob
from struct import unpack

def _err(errStr):
    sys.stderr.write('ERR!! : '+errStr+'\n')
    sys.exit(1)

def _warn(errStr):
    sys.stderr.write('WARN!! : '+errStr+'\n')


def acv2gimp(acvPath, outPath='', destDir=''):
    acvPath = os.path.realpath(acvPath)
    destDir = os.path.realpath(destDir)
    if not outPath:
        if destDir and os.path.isdir(destDir):
            p = destDir
        else:
            p = os.path.dirname(acvPath)
        n = os.path.basename(acvPath)
        newName = n[:n.rfind('.')] #+'.crv'
        outName = os.path.join(p, newName)
        wp = open(outName, 'w')
    elif outPath == '-':
        wp = sys.stdout
    else:
        wp = open(outPath, 'w')

    rp = open(acvPath, 'rb')

    _, curveCnt = unpack(">hh", rp.read(4))
    if curveCnt > 5:
        _warn("More than 5 curves, output may be invalid!")

    wp.write("# GIMP Curves File\n")
    for i in range(1, curveCnt+1):
        pointCnt = unpack(">h", rp.read(2))[0]
        if pointCnt > 17:
            _warn("%d in curve %d, some points will be ignored"%(pointCnt, i))
        for p in range(1, pointCnt):
            y, x = unpack(">hh", rp.read(4))
            wp.write("%d %d "%(x,y))
        # pad with null pairs up to 16 points
        for p in range(17-pointCnt):
            wp.write("-1 -1 ")
        # final point
        y, x = unpack(">hh", rp.read(4))
        wp.write("%d %d\n"%(x,y))

    wp.close()
    if not len(rp.read()) == 0:
        _warn("Unread data left in file")
    rp.close()

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options] phtoshop_curve.acv"
    optPsr = OptionParser(usage)
    optPsr.add_option('-o', '--output', type='string', default='', help='by default output is "input_acv"(removing ext). you can also use "-" by meaning stdout')
    optPsr.add_option('-d', '--destdir', type='string', default='', help='output destination(directory)')
    (opts, args) = optPsr.parse_args()

    acvs = []
    for arg in args:
        acvs += glob.glob(arg)

    if opts.output and len(acvs) > 1:
        _err("can't specify a output file with multiple input acv")

    for acv in acvs:
        acv2gimp(acv, opts.output, opts.destdir)
    
    
if __name__ == '__main__':
    main()
