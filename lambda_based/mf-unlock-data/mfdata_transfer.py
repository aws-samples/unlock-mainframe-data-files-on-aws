#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import ftplib
import os, traceback
import tempfile
from logger import logger
import config as cfg


ftp_host = cfg.ftp_host
ftp_port = cfg.ftp_port
ftp_timeout = cfg.ftp_timeout
ftp_user = cfg.ftp_user
ftp_passwd = cfg.ftp_passwd
    
def append_ftp_out(ftpout):
    if ftpout:
        for line in ftpout.split('\n'):
            ftp_out.append(line)


def write_data(data):
    print(data, file=outfile)
    #outfile.write(data + '\n')


def ftp_download(dsn, pc_dir, ftp_mode):
    
    logger.info("Downloading mainframe dataset '{}' to local path '{}' ...".format(dsn, pc_dir))
    
    global outfile, pc_dsn, pc_path, ftp_out

    mf_dsn = "'{}'".format(dsn)
    dir_data = []
    ftp_out = []

    if ftp_mode == 'ASCII': 
        pc_ext = 'txt'
    else:
        pc_ext = 'dat'

    pc_dsn = "{}.{}".format(dsn,pc_ext)

    if r'(' in pc_dsn:
        pc_dsn = pc_dsn.replace('(','-').replace(')','')

    if not pc_dir:
        pc_dir = tempfile.gettempdir()

    pc_path = os.path.join(pc_dir,pc_dsn)

    
    os.chdir(pc_dir)
    
    try:
        ftp = ftplib.FTP()
        ftpout = ftp.connect(host=ftp_host, port=ftp_port, timeout=ftp_timeout)
        append_ftp_out(ftpout)
        ftpout = ftp.login(user=ftp_user, passwd=ftp_passwd)
        append_ftp_out(ftpout)
    except Exception as e:
        logger.error("Error in ftp connect : {}".format(e))
        logger.error(traceback.format_exc())
        return False

    try: 
        ftpout = ftp.dir(mf_dsn, dir_data.append)
        append_ftp_out(ftpout)
    except Exception as e:
        logger.error("Error in ftp dir : {}".format(e))
        logger.error(traceback.format_exc())
        return False

    for line in dir_data:
        append_ftp_out(ftpout)

    try:
        ftpcmd = 'RETR {}'.format(mf_dsn)

        if ftp_mode == 'BINARY':
            outfile = open(pc_path,"wb")
            ftpout = ftp.retrbinary(ftpcmd, outfile.write)
            append_ftp_out(ftpout)
            outfile.close()
        else:
            outfile = open(pc_path,"w")
            ftpout = ftp.retrlines(ftpcmd, write_data)
            append_ftp_out(ftpout)
            outfile.close()

    except Exception as e:
        logger.error("Error in ftp download : {}".format(e))
        logger.error(traceback.format_exc())
        return False

    # for line in ftp_out:
    #     logger.info(line)

    ftp.quit()
    
    return pc_path