#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import re

class CpyTokenizer:

    def __init__(self, cpydata):

        self.line_sep = r'[.,;]?$|[.,;]?\s'
        self.s_quote = r"'[^']*'"
        self.d_quote = r'"[^"]*"'
        self.reg_ex = re.compile("(%s|%s|%s)" % (self.line_sep, self.s_quote, self.d_quote))

        self.tokens = self.cpyTokenize(cpydata)


    def cpyClean(self, cpydata):
        
        cpydata = [ line[6:72].rstrip() for line in cpydata.split('\n') if len(line) > 6 and line[6] not in ('*','/') ]
        cpydata = [ line for line in cpydata if line.strip() not in ("EJECT", "SKIP1", "SKIP2", "SKIP3")]
        cpydata = [ line for line in cpydata if len(line) > 0]
        cpydata = ' '.join(cpydata)
        
        return cpydata


    def cpyTokenize(self, cpydata):

        clean_cpydata = self.cpyClean(cpydata)
        tokens = [token.strip() for token in re.split(self.reg_ex, clean_cpydata) if token.strip()]

        return tokens


    def getToken(self):

        if self.tokens:
            token = self.tokens.pop(0)
        else:
            token = None
        
        return token

    def putToken(self, token):

        if token:
            self.tokens.insert(0,token)

