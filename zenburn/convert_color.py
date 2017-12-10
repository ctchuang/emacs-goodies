#!/usr/bin/env python

str = '#3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC:#3F3F3F3F3F3F:#CCCC93939393:#7F7F9F9F7F7F:#E3E3CECEABAB:#DFDFAFAF8F8F:#CCCC93939393:#8C8CD0D0D3D3:#DCDCDCDCCCCC'

color_values = [x[1:] for x in str.split(':')]

def hex(hex_str):
    return int('0x' + hex_str, 16)

output = []
for c in color_values:
    r = c[0:2]
    g = c[4:6]
    b = c[8:10]
    output.append("'rgb(%d,%d,%d)'" % (hex(r), hex(g), hex(b)))

print '"[' + ','.join(output) + ']"'
    
