#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import config as cfg
import codecs
import csv

display = codecs.getdecoder(cfg.codepage)

def yieldRecords( aFile, recSize ):
    recBytes = aFile.read(recSize)
    while recBytes:
        yield recBytes
        recBytes = aFile.read(recSize)


def addDecimalPoint(number, scale):
    if scale > 0:
        if isinstance(number, (int, float, complex)):
            result = int(number) / int('1'.ljust(scale+1, '0'))
        else:
            result = '{}.{}'.format(number[:-scale], number[-scale:])
    else:
        result = number
    return result


def unpack_zd(bytes, scale):
    if not bytes:
        return ""
    last_hexbyte = bytes[-1].encode(cfg.codepage).hex()
    zd_sign = {'f': '+', 'c': '+', 'd': '-'}
    sign = zd_sign.get(last_hexbyte[0].lower(), '+')
    unpacked_val = '{}{}{}'.format (sign, bytes[:-1], last_hexbyte[1])
    return addDecimalPoint(unpacked_val, scale)


def unpack_comp(bytes, disp_size, scale):
    if bytes:
        comp_dec = int.from_bytes(bytes, byteorder='big', signed=True)
    else:
        comp_dec = 0
    comp_dec = f'{comp_dec:+0{disp_size}.0f}'
    return addDecimalPoint(comp_dec, scale)


def unpack_comp3(bytes, scale):
    if not bytes:
        return ""

    hexbytes = bytes.hex()
    if hexbytes[-1].lower() in ('b', 'd', 'B', 'D'):
        unpacked = "-{}".format(hexbytes[:-1])
    else:
        unpacked = "+{}".format(hexbytes[:-1])
    return addDecimalPoint(unpacked, scale)


def handle_non_printable(data, mask):
    out_data = []
    for char in data:
        if char.isprintable():
            out_data.append(char)
        else:
            out_data.append(mask)
    return ''.join(out_data)


def convert_mfdata_to_csv(input_datafile, reclen, layout, output_datafile):

    cobolFile = open(input_datafile, 'rb')
    outFile = csv.writer(open(output_datafile, 'w', newline=''), delimiter=',', quoting=csv.QUOTE_NONNUMERIC)

    header_list = []
    for name, start, size, disp_size, usage, sign, scale in layout:
        header_list.append(name)
    outFile.writerow(header_list)

    for recBytes in yieldRecords(cobolFile, reclen):
        record_list = []
        for name, start, size, disp_size, usage, sign, scale in layout:
            field = recBytes[start:start+size]
            if usage == 'COMP-3':
                comp3_unpacked = unpack_comp3(field, scale)
                if not comp3_unpacked.replace('.','',1).replace('-','',1).replace('+','',1).replace(',','').isnumeric() and comp3_unpacked:
                    comp3_unpacked = '0x{}'.format(str(field.hex()).upper())  ## Sending the hex value of EBCDIC data
                record_list.append(comp3_unpacked)
            elif usage == 'COMP':
                comp_unpacked = unpack_comp(field, disp_size, scale)
                if not comp_unpacked.replace('.','',1).replace('-','',1).replace('+','',1).replace(',','').isnumeric() and comp_unpacked:
                    comp_unpacked = '0x{}'.format(str(field.hex()).upper())  ## Sending the hex value of EBCDIC data
                record_list.append(comp_unpacked)
            else:
                disp = list(display(field))[0]
                if sign == 'SIGNED':
                    disp = unpack_zd(disp, scale)
                if not disp.isprintable():
                    #disp = handle_non_printable(disp, '.') ## Replaces non-printable char with '.'
                    disp = '0x{}'.format(str(field.hex()).upper())  ## Sending the hex value of EBCDIC data
                record_list.append(disp)

        outFile.writerow(record_list)


def get_cpy_layout(parse_dict):

    lrecl = 0
    layout = []

    for item_1 in parse_dict:
        if 'lrecl_max' in parse_dict[item_1][0].keys():
            lrecl = parse_dict[item_1][0]['lrecl_max']

        for item_2 in parse_dict[item_1]:
            
            if 'usage' in parse_dict[item_1][item_2].keys():
                usage = parse_dict[item_1][item_2]['usage']
            else:
                usage = 'DISPLAY'
            
            data_name = parse_dict[item_1][item_2]['data_name'].replace('-','_')
            offset =  parse_dict[item_1][item_2]['offset']
            
            if 'storage_length' in parse_dict[item_1][item_2].keys():
                length = parse_dict[item_1][item_2]['storage_length']
            else:
                length = 0

            if 'disp_length' in parse_dict[item_1][item_2].keys():
                disp_length = parse_dict[item_1][item_2]['disp_length']
            else:
                disp_length = 0
            
            sign = 'UNSIGNED'
            if 'signed' in parse_dict[item_1][item_2].keys():
                if parse_dict[item_1][item_2]['signed']:
                    sign = 'SIGNED'
            
            if 'scale' in parse_dict[item_1][item_2].keys():
                scale = parse_dict[item_1][item_2]['scale']
            else:
                scale = 0

            if length > 0:
                layout.append((data_name, offset, length, disp_length, usage, sign, scale))

    return lrecl, layout



