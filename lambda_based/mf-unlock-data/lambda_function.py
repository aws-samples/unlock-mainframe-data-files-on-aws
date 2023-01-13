#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import os
import json
import main
import s3_transfer as s3data
from logger import logger
import config as cfg
import os, json, csv
        
def lambda_handler(event, context):

    
    main.start_to_unlock_mf_data(event['data_dsn'], event['cpy_dsn'])
    
    message="Lambda Function completed..."
    
    return { 
        'message' : message
    }
    
