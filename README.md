# Unlock Mainframe Data files on AWS Cloud

This solution is designed to help you unlock legacy mainframe data by migrating data files from mainframe systems to AWS. By migrating the data, you can make use of the powerful analytics, machine learning, and other services available in AWS to gain insights and make better decisions based on the data. Additionally, you can also take advantage of the scalability and cost savings of cloud computing. The solution involves transferring data from mainframe datasets, converting it into a CSV format, and then storing the data in AWS. Finally, you can use the AWS services to analyze and process the data to gain insights and derive greater value from your legacy data files.

## Supported Features

**Datatypes:**

* COMP-3 packed-decimal
* COMP binary fields
* Zoned Decimal
* Alphanumeric
* Numeric

**Complex structures:**

* REDEFINES
* OCCURS and Nested OCCURS

**Output Format:**

* CSV (comma separated)


## Current Limitations

* Variable record length (VB) files are not supported
* OCCURS DEPENDING ON is not supported

## Product versions

Current Release

* v1.0 - Initial version


## Steps to setup and execute

This solution offers three different deployment options: Lambda-based, EC2-based, and Run Anywhere. Depending on the deployment method you have selected, you need to follow the instructions for that particular deployment option.

>### *Lambda Based*

For the Lambda-based deployment, you need to follow these steps:

1. Clone this GitLab project
2. Use the folder 'lambda_based'
3. Create a zip file from folder 'lambda_based\mf-unlock-data-init'
4. Create a zip file from folder 'lambda_based\mf-unlock-data'
5. Create a new Lambda called 'mf-unlock-data-init' using AWS Console
6. Upload the source code using 'mf-unlock-data-init.zip' (created in earlier step)
7. Create a new Lambda called 'mf-unlock-data' using AWS Console
8. Upload the source code using 'mf-unlock-data.zip'  (created in earlier step)
9. Modify 'src\config.py'
    - codepage
    - ftp_host
    - ftp_port
    - ftp_timeout
    - ftp_user
    - ftp_passwd_b64
    - s3_bucket_name
    - s3_mfdata_input_dir
    - s3_mfdata_output_dir
    - s3_mfdata_work_dir
    - lambda_work_dir
10. Create S3 bucket and folders for input, output and work data
11. Add S3 upload trigger to Lambda 'mf-unlock-data-init'
12. Upload mainframe dataset/copybook mapping file into S3 input folder (i.e. s3_mfdata_input_dir)
13. The init and conversion process will be triggered automatically if configured correctly. The datasets and copybooks will be downloaded from mainframe using FTP and converted to ASCII CSV file.
14. Verify converted CSV file(s) in S3 output folder (i.e. s3_mfdata_output_dir)


>### *EC2 Based*

For the EC2-based deployment, you need to follow these steps:

1. Clone this GitLab project into an EC2 instance
2. Use the folder 'ec2_based'
3. Modify 'src\config.py'
    - codepage
    - ftp_host
    - ftp_port
    - ftp_timeout
    - ftp_user
    - ftp_passwd_b64
    - s3_bucket_name
    - s3_mfdata_input_dir
    - s3_mfdata_output_dir
    - s3_mfdata_work_dir
    - s3_dataset_mapping_file
    - ec2_work_dir
4. Create S3 bucket and folders for input, output and work data
5. Upload mainframe dataset/copybook mapping file into S3 input folder (i.e. s3_mfdata_input_dir).
6. Execute 'src\main.py'
7. The datasets and copybooks will be downloaded from mainframe using FTP and converted to ASCII CSV file.
8. Verify converted CSV file(s) in S3 output folder (i.e. s3_mfdata_output_dir).


>### *Run Anywhere*

For the Run Anywhere deployment, you need to follow these steps:

1. Clone this GitLab project
2. Use the folder 'run_anywhere'
3. Modify 'src\config.py'
    - codepage
4. Modify 'src\main.py'
    - input_copybook
    - input_datafile
    - output_datafile
5. Place mainframe data file(s) and copybook(s) for conversion
    - mfdata\copybook
    - mfdata\input
6. Execute 'src\main.py'
7. Verify converted CSV file(s)
    - mfdata\output

## Future Enhancements

* Support for Variable record length (VB) files
* Support for COMP-1 and COMP-2 floating point data types 
* Support for Nested Occurs and Occurs Depending on clauses
* CloudFormation Template for automated deployment
* ETL process automation
* Add option to choose output file format(JSON/Parquet etc)

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This library is licensed under the MIT-0 License. See the LICENSE file.

