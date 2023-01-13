import base64
import tempfile
#
# Data Conversion Configuration
#
codepage='cp037'

##
## FTP Configuration
##
ftp_host = '127.0.0.1'
ftp_port = 21
ftp_timeout = 300
ftp_user = 'ftpuser'
ftp_passwd_b64 = 'ZnRwcGFzc3dkCg=='
ftp_passwd = base64.b64decode(ftp_passwd_b64.encode('ascii')).decode('ascii').replace('\n','')

##
## AWS Configuration
##
s3_bucket_name = "mf-test-arunkse-athena"
s3_mfdata_input_dir = "mfdata/input"
s3_mfdata_output_dir = "mfdata/output"
s3_mfdata_work_dir = "mfdata/tmp"
s3_dataset_mapping_file = "dataset_mapping.csv"
ec2_work_dir = tempfile.gettempdir()