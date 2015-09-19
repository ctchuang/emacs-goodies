#!/usr/bin/env python

import fileinput
import re

def cleanup_blank_lines(lines):
    """Clean consecutive blank lines."""
    output = []
    blank_pattern = re.compile(r'^\s*$')
    is_blank_line = False
    for line in lines:
        if blank_pattern.match(line):
            if is_blank_line:
                continue
            else:
                output.append('')
                is_blank_line = True
        else:
            is_blank_line = False
            output.append(line)
    return output


def prettify():
    max_values_by_level = dict()
    current_level = 1
    title_pattern = re.compile(r'^([*]+ )(.*)$')
    lines = []
    for line in fileinput.input():
        match = title_pattern.match(line)
        if match:
            level = len(match.group(1)) - 1
            title = match.group(2)
            if level > current_level:
                max_value = 1
            else:
                max_value = max_values_by_level.get(level, 0) + 1
            max_values_by_level[level] = max_value
            current_level = level
            output = ''
            for l in range(level):
                output += ('.' if l > 0 else '') + str(max_values_by_level.get(l + 1, 1))
            if level == 1:
                output += '.'
            lines.append('')
            lines.append(output + ' ' + title)
            lines.append('')
        else:
            lines.append(' ' * 3 + line.rstrip())
    lines = cleanup_blank_lines(lines)
    print '\n'.join(lines)


def main():
    prettify()


if __name__ == '__main__':
  main()
