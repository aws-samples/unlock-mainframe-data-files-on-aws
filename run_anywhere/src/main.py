#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import cpy_tokenizer as cpytkn
import cpy_parser as cpyparsr
import cobdata_to_csv as cobdata2csv
import config as cfg
from pathlib import Path
import os, json

script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(script_path)
project_dir = os.path.split(script_dir)[0]

def convert_cobdata2csv(input_copybook, input_datafile, output_datafile):

    cpy_fname = Path(input_copybook).stem
    cpy_data = open(input_copybook,"r").read()
    tokenizer = cpytkn.CpyTokenizer(cpy_data)
    cpy_parser = cpyparsr.CpyParser()
    cpyparser_out = cpy_parser.cpyparser(tokenizer)
    cpyparser_dict = {cpy_fname: cpyparser_out}
    # cpyparser_json = json.dumps(cpyparser_dict, sort_keys=True, indent=4)
    with open("{}.json".format(input_copybook), 'w') as jsonfile:
        json.dump(cpyparser_dict, jsonfile, sort_keys=True, indent=4)
    lrecl, layout = cobdata2csv.get_cpy_layout(cpyparser_dict)

    print ('')
    print ('Input EBCDIC Data File     : {}'.format(input_datafile))
    print ('Input Copybook Layout      : {}'.format(input_copybook))
    print ('Calculated Max. LRECL      : {}'.format(lrecl))
    print ('Output ASCII CSV Data File : {}'.format(output_datafile))
    print ('')

    cobdata2csv.convert_mfdata_to_csv(input_datafile, lrecl, layout, output_datafile)

    return


def main():

    ## Convert TEST.EMP.RECORD.FILE.dat to csv using copybook EMPREC01.cpy
    input_copybook = os.path.join(project_dir, 'mfdata', 'copybook', 'EMPREC01.cpy')
    input_datafile = os.path.join(project_dir, 'mfdata', 'input', 'TEST.EMP.RECORD.FILE.dat')
    output_datafile = os.path.join(project_dir, 'mfdata', 'output', 'TEST.EMP.RECORD.FILE.csv')
    convert_cobdata2csv(input_copybook, input_datafile, output_datafile)

    return

if __name__ == "__main__":
    main() 
