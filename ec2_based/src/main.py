#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import cpy_tokenizer as cpytkn
import cpy_parser as cpyparsr
import cobdata_to_csv as cobdata2csv
import mfdata_transfer as mfdata
import s3_transfer as s3data
from logger import logger
import config as cfg
from pathlib import Path
import os, json, csv

script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(script_path)
project_dir = os.path.split(script_dir)[0]

def delete_file(file_name):
    logger.info("Deleting local file '{}' ...".format(file_name))
    os.remove(file_name)
    return


def convert_cobdata2csv(input_copybook, input_datafile, output_datafile):

    cpy_fname = Path(input_copybook).stem
    cpy_data = open(input_copybook,"r").read()
    tokenizer = cpytkn.CpyTokenizer(cpy_data)
    cpy_parser = cpyparsr.CpyParser()
    cpyparser_out = cpy_parser.cpyparser(tokenizer)
    cpyparser_dict = {cpy_fname: cpyparser_out}
    # cpyparser_json = json.dumps(cpyparser_dict, sort_keys=True, indent=4)
    with open(json_file_path, 'w') as jsonfile:
        json.dump(cpyparser_dict, jsonfile, sort_keys=True, indent=4)
    lrecl, layout = cobdata2csv.get_cpy_layout(cpyparser_dict)

    logger.info('Input EBCDIC Data File     : {}'.format(input_datafile))
    logger.info('Input Copybook Layout      : {}'.format(input_copybook))
    logger.info('Calculated Max. LRECL      : {}'.format(lrecl))
    logger.info('Output ASCII CSV Data File : {}'.format(output_datafile))
    logger.info("Converting EBCDIC data file '{}' to ASCII CSV file format using Copybook '{}' ...".format(input_datafile, input_copybook))
    cobdata2csv.convert_mfdata_to_csv(input_datafile, lrecl, layout, output_datafile)

    return

def start_to_unlock_mf_data(data_dsn, cpy_dsn):

    global json_file_path

    cpy_file_path = mfdata.ftp_download(cpy_dsn, cfg.ec2_work_dir, 'ASCII')
    json_file_path = "{}.json".format(os.path.splitext(cpy_file_path)[0])
    dat_file_path = mfdata.ftp_download(data_dsn, cfg.ec2_work_dir, 'BINARY')
    csv_file_path = os.path.splitext(dat_file_path)[0]+'.csv'
    convert_cobdata2csv(cpy_file_path, dat_file_path, csv_file_path)

    s3data.upload_to_s3(cpy_file_path, cfg.s3_mfdata_work_dir)
    s3data.upload_to_s3(json_file_path, cfg.s3_mfdata_work_dir)
    s3data.upload_to_s3(dat_file_path, cfg.s3_mfdata_work_dir)
    s3data.upload_to_s3(csv_file_path, cfg.s3_mfdata_output_dir)

    delete_file(cpy_file_path)
    delete_file(json_file_path)
    delete_file(dat_file_path)
    delete_file(csv_file_path)

    return



def main():

    s3_dataset_mapping_filepath = os.path.join(cfg.s3_mfdata_input_dir, cfg.s3_dataset_mapping_file)
    dataset_mapping_file_path = s3data.download_from_s3(s3_dataset_mapping_filepath, cfg.ec2_work_dir)

    logger.info("")

    input_file = open(dataset_mapping_file_path, "r")
    reader = csv.reader(input_file)
    total_count = len(list(reader))-1

    input_file = open(dataset_mapping_file_path, "r")
    reader = csv.reader(input_file)
    next(reader, None)
    for line in reader:
        data_dsn = line[0]
        cpy_dsn = line[1]
        logger.info("--------------- Processing ({} of {}) -----------------".format(reader.line_num-1, total_count))
        start_to_unlock_mf_data(data_dsn, cpy_dsn)
        logger.info("")

    delete_file(dataset_mapping_file_path)

    return

if __name__ == "__main__":

    main()

