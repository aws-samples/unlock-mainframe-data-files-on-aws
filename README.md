# Unlock Mainframe Data files on AWS Cloud

This solution is designed to help you unlock legacy mainframe data by migrating data files from mainframe systems to AWS. By migrating the data, you can make use of the powerful analytics, machine learning, and other services available in AWS to gain insights and make better decisions based on the data. Additionally, you can also take advantage of the scalability and cost savings of cloud computing. The solution involves transferring data from mainframe datasets, converting it into a CSV format, and then storing the data in AWS. Finally, you can use the AWS services to analyze and process the data to gain insights and derive greater value from your legacy data files.

## Table of Contents

- [Unlock Mainframe Data files on AWS Cloud](#unlock-mainframe-data-files-on-aws-cloud)
  - [Table of Contents](#table-of-contents)
  - [Supported Features](#supported-features)
  - [Current Limitations](#current-limitations)
  - [Product versions](#product-versions)
  - [Deployment options](#deployment-options)
    - [*Lambda Based*](#lambda-based)
    - [*EC2 Based*](#ec2-based)
    - [*Run Anywhere*](#run-anywhere)
  - [Samples](#samples)
    - [Mapping File](#mapping-file)
    - [Input Copybook](#input-copybook)
    - [Output CSV](#output-csv)
    - [View of CSV Data with an Excel table](#view-of-csv-data-with-an-excel-table)
  - [Future Enhancements](#future-enhancements)
  - [Testing](#testing)
  - [Security](#security)
  - [License](#license)

## Supported Features

Currently, the project supports the following features:

**Datatypes:**

* COMP-3 or Packed Decimal
* COMP or Binary
* Zoned Decimal
* Alphanumeric
* Numeric

**Complex structures:**

* REDEFINES (including at 01 level)
* OCCURS and Nested OCCURS

**Output Format:**

* CSV (comma separated)

This means the solution/script is able to handle and convert mainframe data files that use these datatypes and complex structures, and output the converted data in CSV format.

## Current Limitations

Currently,  the project has the following limitations:

* It does not support variable record length (VB) files
* It does not support OCCURS DEPENDING ON clause in copybook

## Product versions

Current Release

* v1.0 - Initial version


## Deployment options

This solution offers three different deployment options: Lambda-based, EC2-based, and Run Anywhere. Depending on the deployment method you have selected, you need to follow the instructions for that particular deployment option.

### *Lambda Based*

![Alt text](./samples/images/Lambda-based-deployment.png?raw=true "Lambda Based")

In this architecture, two Lambda functions are used to automate the mainframe data conversion process. The user uploads a mapping file, containing the mapping between mainframe datasets and copybooks, to an input folder in an S3 bucket. This triggers the initialization process Lambda function. For each file read by the initialization Lambda function, a conversion process Lambda function is invoked. The conversion function downloads the dataset and copybook files from the mainframe system using FTP, parses the copybook, and converts the dataset to CSV format. The produced output file is then uploaded to an output folder in the S3 bucket. This architecture allows for a scalable and efficient process for converting mainframe datasets to CSV format and provides a way to automate the process.

For the Lambda-based deployment, you need to follow these steps:

1. Clone the GitLab project using the command `git clone <project_url>` in the command line or terminal.
2. Navigate to the folder named `lambda_based` within the cloned project.
3. Use a command line tool or GUI tool to create a zip file of the contents of the folder `lambda_based\mf-unlock-data-init`. This file should be named `mf-unlock-data-init.zip`
4. Repeat step 3 for the folder `lambda_based\mf-unlock-data` and name the zip file `mf-unlock-data.zip`
5. Sign in to the AWS Console, navigate to the Lambda service, and click on the "Create function" button. Select the "Author from scratch" option and give the function a name of `mf-unlock-data-init`. Select a runtime that is compatible with the source code.
6. Under the "Function code" section, select the "Upload a .zip file" option and upload the `mf-unlock-data-init.zip` file created in step 3.
7. Repeat steps 5 and 6 to create a new Lambda function called `mf-unlock-data` and upload the `mf-unlock-data.zip` file.
8. Open the `src\config.py` file and modify the following variables to match your environment:
    * `codepage` - The codepage of the mainframe datasets to be converted
    * `ftp_host` - The hostname or IP address of the mainframe FTP server
    * `ftp_port` - The port number for the mainframe FTP server
    * `ftp_timeout` - The timeout value for the FTP connection
    * `ftp_user` - The username for the FTP account
    * `ftp_passwd_b64` - The base64 encoded password for the FTP account
    * `s3_bucket_name` - The name of the S3 bucket where the files will be stored
    * `s3_mfdata_input_dir` - The directory within the S3 bucket where mainframe dataset/copybook mapping files will be uploaded
    * `s3_mfdata_output_dir` - The directory within the S3 bucket where the converted CSV files will be stored
    * `s3_mfdata_work_dir` - The directory within the S3 bucket used for temporary storage during the conversion process
    * `lambda_work_dir` - The working directory for the Lambda function
9. In the AWS S3 service, create a new S3 bucket with the name specified in `s3_bucket_name` variable. Then create three folders within the bucket with names matching the values of the `s3_mfdata_input_dir`, `s3_mfdata_output_dir`, and `s3_mfdata_work_dir` variables.
10. In the AWS Lambda service, navigate to the `mf-unlock-data-init` function and under the "Designer" section, select "S3" from the "Add triggers" list and configure the trigger to listen for file uploads to the `s3_mfdata_input_dir` folder in the S3 bucket.
11. Upload the mainframe dataset/copybook mapping file to the `s3_mfdata_input_dir` folder in the S3 bucket. This file will be used to map the mainframe dataset and copybook files that need to be converted.
12. Once the mapping file is uploaded, the `mf-unlock-data-init` Lambda function will be triggered automatically if configured correctly. The function will use the FTP connection information and dataset/copybook mapping to download the mainframe files and convert them to ASCII CSV format.
13. The converted CSV files will be stored in the `s3_mfdata_output_dir` folder in the S3 bucket.
14. Verify that the conversion process was successful by checking the contents of the `s3_mfdata_output_dir` folder. Make sure that the CSV files are in the correct format and contain the expected data.


### *EC2 Based*

![Alt text](./samples/images/EB2-based-deployment.png?raw=true "EC2 Based")

In this architecture, an EC2 instance is used for the compute instead of using Lambda functions. The python scripts from the project are downloaded into an EC2 instance. Once the scripts are downloaded, the EC2 instance will be used to perform the conversion process. The script reads the previously uploaded mapping file from an S3 bucket, downloads the dataset and copybook files from the mainframe system using FTP, parses the downloaded copybook and converts the dataset to CSV format using the information from the copybook. The resulting CSV file is then uploaded back to an S3 bucket. This architecture allows for more compute resources to be allocated to the conversion process than would be possible with a Lambda-based approach. 

For the EC2-based deployment, you need to follow these steps:

1. On the EC2 instance, use the command `git clone <project_url>` in the command line or terminal to clone the GitLab project.
2. Navigate to the folder named `ec2_based` within the cloned project.
3. Open the `src\config.py` file and modify the following variables to match your environment:
    * `codepage` - The codepage of the mainframe datasets to be converted
    * `ftp_host` - The hostname or IP address of the mainframe FTP server
    * `ftp_port` - The port number for the mainframe FTP server
    * `ftp_timeout` - The timeout value for the FTP connection
    * `ftp_user` - The username for the FTP account
    * `ftp_passwd_b64` - The base64 encoded password for the FTP account
    * `s3_bucket_name` - The name of the S3 bucket where the files will be stored
    * `s3_mfdata_input_dir` - The directory within the S3 bucket where mainframe dataset/copybook mapping files will be uploaded
    * `s3_mfdata_output_dir` - The directory within the S3 bucket where the converted CSV files will be stored
    * `s3_mfdata_work_dir` - The directory within the S3 bucket used for temporary storage during the conversion process
    * `s3_dataset_mapping_file` - The name of the dataset/copybook mapping file that will be uploaded to S3 input folder
    * `ec2_work_dir` - The working directory for the script on the EC2 instance
4. In the AWS S3 service, create a new S3 bucket with the name specified in `s3_bucket_name` variable. Then create three folders within the bucket with names matching the values of the `s3_mfdata_input_dir`, `s3_mfdata_output_dir`, and `s3_mfdata_work_dir` variables.
5. Using the S3 web interface or AWS CLI, upload the mainframe dataset/copybook mapping file, specified in `s3_dataset_mapping_file` variable, to the `s3_mfdata_input_dir` folder in the S3 bucket.
6. On the EC2 instance, navigate to the `ec2_based` folder and execute the script `src\main.py` using the command `python src/main.py`.
7. The script will use the FTP connection information and dataset/copybook mapping to download the mainframe files and convert them to ASCII CSV format.
8. The converted CSV files will be stored in the `s3_mfdata_output_dir` folder in the S3 bucket.
9. Verify that the conversion process was successful by checking the contents of the `s3_mfdata_output_dir` folder. Make sure that the CSV files are in the correct format and contain the expected data.


### *Run Anywhere*

![Alt text](./samples/images/Run-anywhere-deployment.png?raw=true "Run Anywhere")

The "Run anywhere" deployment option allows you to run this solution/script on any system that has python installed. It processes the already downloaded mainframe copybooks and data files, and converts them into a CSV file. The input and output files are stored on the same system where the script is running, so no external dependencies are required. This option provides flexibility in running the script as it can be run in any environment whether it is on-premises or on cloud. 

For the Run Anywhere deployment, you need to follow these steps:

1. Clone the GitLab project using the command `git clone <project_url>` in the command line or terminal.
2. Navigate to the folder named `run_anywhere` within the cloned project.
3. Open the `src\config.py` file and modify the `codepage` variable to match the codepage of the mainframe datasets to be converted.
4. Open the `src\main.py` file and modify the following variables to match the location of the mainframe dataset and copybook files:
    * `input_copybook` - The path to the mainframe copybook file
    * `input_datafile` - The path to the mainframe dataset file
    * `output_datafile` - The path to the location where the converted CSV file will be saved
5. In the `mfdata` folder, create two subfolders `copybook` and `input` and place the mainframe copybook and dataset files respectively.
6. On the command line or terminal, navigate to the `run_anywhere` folder and execute the script `src\main.py` using the command `python src/main.py`.
7. The script will use the mainframe dataset and copybook files to convert the data to ASCII CSV format and store the output file in the `mfdata\output` folder
8. Verify that the conversion process was successful by checking the contents of the `mfdata\output` folder. Make sure that the CSV files are in the correct format and contain the expected data.

## Samples

### Mapping File

The mapping file used in this solution should be in a comma-separated format and should contain a header with the following columns:

* DatasetName: The name of the mainframe dataset
* CopybookName: The name of the corresponding copybook

This file should have one entry per line, where each entry maps a dataset to its corresponding copybook. This file can have many entries, allowing the script to process multiple datasets and copybooks. The script uses this mapping file to determine which datasets and copybooks to download and convert.

**dataset_mapping.csv**
```
DatasetName,CopybookName
AWS.M2.CARDDEMO.ACCTDATA.PS,AWS.M2.CARDDEMO.CPY(CVACT01Y)
AWS.M2.CARDDEMO.CUSTDATA.PS,AWS.M2.CARDDEMO.CPY(CVCUS01Y)
```

### Input Copybook

```cobol
      * 
      * Sample Employee Record COBOL Layout
      * 
       01 EMP-RECORD.
         05 EMP-ID                      PIC 9(5).
         05 EMP-ID-X REDEFINES EMP-ID   PIC X(5).
         05 EMP-NAME                    PIC X(25).
         05 EMP-DOB                     PIC X(10).
         05 EMP-ADDRESS OCCURS 3 TIMES.
            10 EMP-ADDR-LINE            PIC X(25).
         05 EMP-YOE-CUR                 PIC S9(4) COMP.
         05 EMP-YOE-TOTAL               PIC 9(4)V99 COMP-3.
         05 EMP-SALARY                  PIC S9(4)V99.
         05 EMP-SALARY-DIFF             PIC S9999V99 COMP-3.         
         05 EMP-DEPENDENTS-NUM          PIC S9(2).
         05 FILLER                      PIC X(17).
```


### Output CSV

```
"EMP_ID","EMP_ID_X","EMP_NAME","EMP_DOB","EMP_ADDR_LINE_1","EMP_ADDR_LINE_2","EMP_ADDR_LINE_3","EMP_YOE_CUR","EMP_YOE_TOTAL","EMP_SALARY","EMP_SALARY_DIFF","EMP_DEPENDENTS_NUM","FILLER_X_100"
"00001","00001","Jennifer Cooley          ","07/11/1982","P.O. Box 433, 3031 Sem Av","India                    ","693624                   ","+008","+00017.00","+7354.04","+00612.83","+02","                 "
"00002","00002","Patience Hanson          ","10/04/1985","Ap #839-8598 Nulla Avenue","France                   ","904878                   ","+002","+00016.00","+2155.33","-00179.61","+03","                 "
"00003","00003","Ignacia Lynch            ","12/14/1984","711-6709 Ornare, Rd.     ","Indonesia                ","561128                   ","+002","+00017.00","+9186.54","+00765.54","+03","                 "
"00004","00004","Mariam Dejesus           ","08/18/1978","P.O. Box 914, 6128 Sem, S","Turkey                   ","46307                    ","+001","+00018.00","+1890.32","+00157.52","+04","                 "
"00005","00005","Kevin Anderson           ","07/01/1993","P.O. Box 989, 7831 Mauris","Turkey                   ","474411                   ","+005","+00021.00","+5619.50","-00468.29","+04","                 "

```
### View of CSV Data with an Excel table

|EMP_ID|EMP_ID_X|EMP_NAME|EMP_DOB|EMP_ADDR_LINE_1|EMP_ADDR_LINE_2|EMP_ADDR_LINE_3|EMP_YOE_CUR|EMP_YOE_TOTAL|EMP_SALARY|EMP_SALARY_DIFF|EMP_DEPENDENTS_NUM|FILLER_X_100|
|:-----|:-------|:-------|:------|:--------------|:--------------|:--------------|:----------|:------------|:---------|:--------------|:-----------------|:-----------|
|00001|00001|Jennifer Cooley          |07/11/1982|P.O. Box 433, 3031 Sem Av|India                    |693624                   |+008|+00017.00|+7354.04|+00612.83|+02|                 |
|00002|00002|Patience Hanson          |10/04/1985|Ap #839-8598 Nulla Avenue|France                   |904878                   |+002|+00016.00|+2155.33|-00179.61|+03|                 |
|00003|00003|Ignacia Lynch            |12/14/1984|711-6709 Ornare, Rd.     |Indonesia                |561128                   |+002|+00017.00|+9186.54|+00765.54|+03|                 |
|00004|00004|Mariam Dejesus           |08/18/1978|P.O. Box 914, 6128 Sem, S|Turkey                   |46307                    |+001|+00018.00|+1890.32|+00157.52|+04|                 |
|00005|00005|Kevin Anderson           |07/01/1993|P.O. Box 989, 7831 Mauris|Turkey                   |474411                   |+005|+00021.00|+5619.50|-00468.29|+04|                 |

## Future Enhancements

Future enhancements for this project include:

1. Adding support for variable record length (VB) files
2. Adding support for COMP-1 and COMP-2 floating point data types
3. Adding support for OCCURS DEPENDING ON clause in copybook
4. Automating the deployment process using CloudFormation
5. Adding an option for choosing the output file format, such as JSON or Parquet

## Testing

To test this solution/script, you can simulate mainframe data using the copybooks and data from the AWS CardDemo application, and see if the script can convert it correctly and produce the desired results. This will make it easier for you to assess the script's effectiveness and output quality.

[AWS CardDemo mainframe application](https://github.com/aws-samples/aws-mainframe-modernization-carddemo)

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This library is licensed under the MIT-0 License. See the LICENSE file.

