#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import logging
import sys

logger = logging.getLogger()
logger.setLevel(logging.INFO)
stdout_handler = logging.StreamHandler(sys.stdout)
logger.addHandler(stdout_handler)