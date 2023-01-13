#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import json
import urllib.parse
import boto3
import config as cfg

s3_client = boto3.client('s3')
lambda_client = boto3.client('lambda')

def lambda_handler(event, context):

    # Get the object from the event and show its content type
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = urllib.parse.unquote_plus(event['Records'][0]['s3']['object']['key'], encoding='utf-8')
    try:
        s3obj = s3_client.get_object(Bucket=bucket, Key=key)
        file_content = s3obj["Body"].read().decode('utf-8').splitlines()
        
        for line in file_content[1:]:
            data_dsn = line.split(',')[0]
            cpy_dsn = line.split(',')[1]
            
            print("Triggering lambda 'mf-unlock-data' to process the Mainframe datasets '{}' and '{}' ...".format(data_dsn, cpy_dsn))
            
            lambdaPayload = {}
            lambdaPayload['data_dsn'] = data_dsn
            lambdaPayload['cpy_dsn'] = cpy_dsn
            
            invoke_resp = lambda_client.invoke(FunctionName=cfg.conversion_lambda_name,
                        InvocationType='Event',
                        LogType='Tail',
                        Payload=json.dumps(lambdaPayload))
            print(invoke_resp)
            
        return
    except Exception as e:
        print(e)
        raise e
