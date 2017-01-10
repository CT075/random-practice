#!/usr/bin/python

# Cameron Wong
# Senior-5
# Phillips Academy

def main():
    num, length, dec = input().split(', ')
    length = int(length)
    dec = int(dec)
    big, little = num.split('.')
    if length - (len(big)+1) < dec:
        result = ['#']*length
        result[-(dec+1)] = '.'
        return ''.join(result)
    sign = '+' if num[0]=='+' else ''
    num = round(float(num)+10**(-(dec+1)), dec)
    return '{}{}'.format(sign, num).rjust(length, '#')

for i in range(5):
    print(main())
