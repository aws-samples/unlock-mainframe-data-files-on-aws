#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import boto3
import os, re, sys
from logger import logger
import traceback
import config as cfg

s3_bucket_name = cfg.s3_bucket_name

def upload_to_s3(pc_file_path, s3_folder):
    
    try:
        s3_file_path = os.path.join(s3_folder, os.path.basename(pc_file_path))
        logger.info("Uploading file '{}' to S3 path '{}/{}' (size: {} Bytes) ...".format(pc_file_path, s3_bucket_name, s3_folder, os.path.getsize(pc_file_path)))
        s3_client = boto3.client("s3")
        s3_client.upload_file(pc_file_path, s3_bucket_name, s3_file_path)
        return True

    except Exception as e:
        logger.error("Error in uploading file to S3 : {}".format(e))
        logger.error(traceback.format_exc())
        return False


def download_from_s3(s3_file_path, pc_folder_path):
    
    try:
        pc_file_path = os.path.join(pc_folder_path, os.path.basename(s3_file_path))
        logger.info("Downloading S3 file '{}/{}' to local path '{}' ...".format(s3_bucket_name, s3_file_path, pc_file_path))
        s3_client = boto3.client("s3")
        s3_client.download_file (s3_bucket_name, s3_file_path, pc_file_path)
        return pc_file_path

    except Exception as e:
        logger.error("Error in downloading file from S3 : {}".format(e))
        logger.error(traceback.format_exc())
        return False