#
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: MIT-0
#

import re, json

class CpyParser:

    def __init__(self):

        self.keywords = ("FILLER", "REDEFINES", "LIKE", "IS", "TYPEDEF", 
            "EXTERNAL", "BLANK", "WHEN", "ZERO", "ZEROS", "ZEROES", 
            "FORMAT", "OF", 
            "GLOBAL", "JUST", "JUSTIFIED", "RIGHT",
            "OCCURS", "TIMES", "ASCENDING", "DESCENDING", "KEY", "INDEXED", "BY", 
            "UNBOUNDED", "DEPENDING", "ON", 
            "PIC", "PICTURE", "SIZE", "LOCALE",
            "SIGN", "LEADING", "TRAILING", "SEPARATE", "CHARACTER",
            "VALUE", "VALUES", "ARE", "SYNCHRONIZED", "SYNC", "LEFT",
            "TYPE", "USAGE", "GROUP-USAGE", "BINARY", "COMPUTATIONAL", "COMP", 
            "COMPUTATIONAL-1", "COMP-1", "COMPUTATIONAL-2", "COMP-2", "COMPUTATIONAL-3", "COMP-3",
            "COMPUTATIONAL-4", "COMP-4", "COMPUTATIONAL-5", "COMP-5", "DISPLAY", "DISPLAY-1",
            "INDEX", "NATIONAL", "PACKED-DECIMAL", "POINTER", "PROCEDURE-POINTER", "FUNCTION-POINTER", "NATIVE",
            "DYNAMIC", "LENGTH", 
            "DATE",
            "RENAMES", "THROUGH", "THRU",
            "SPACE", "QUOTE", "HIGH-VALUE", "LOW-VALUE", "ALL", "NULL", "NULLS",
            ".")

        SPACE = r"[\s|,|;]+"
        NAME = r"[\w-]+"
        KEY = fr"((?:ASCENDING|DESCENDING){SPACE}(?:KEY{SPACE})?(?:IS{SPACE})?{NAME})*(?:{SPACE}(?:INDEXED){SPACE}(?:BY{SPACE})?{NAME}(?:{SPACE}{NAME})*)"

        CLAUSES = (
            fr"{SPACE}"
            fr"|(?:REDEFINES){SPACE}(?P<redefines>{NAME})"
            fr"|(?:BLANK){SPACE}(WHEN{SPACE})?(?P<blank>ZERO|ZEROES|ZEROS)"
            r"|EXTERNAL"
            r"|GLOBAL"
            fr"|(?:JUSTIFIED|JUST){SPACE}(?P<justified>RIGHT)?"
            fr"|(?:OCCURS){SPACE}(?:(?P<occurs_min>\d+){SPACE}TO{SPACE})?(?P<occurs_max>\d+)(?:{SPACE}TIMES)?{SPACE}DEPENDING{SPACE}(?:ON{SPACE})?(?P<depending_on>{NAME})(?:{SPACE}{KEY})?"
            fr"|(?:OCCURS){SPACE}(?P<occurs_fixed>\d+)(?:{SPACE}TIMES)?(?:{SPACE}{KEY})?"
            fr"|(?:PIC|PICTURE){SPACE}(?:IS{SPACE})?(?P<picture>\S+)"
            fr"|(?:SIGN{SPACE})?(?:IS{SPACE})?(?P<sign>LEADING|TRAILING)(?P<sign_sep>{SPACE}SEPARATE{SPACE}CHARACTER|{SPACE}SEPARATE)"
            fr"|(?:SYNCHRONIZED|SYNC)(?P<synch>{SPACE}LEFT|{SPACE}RIGHT)?"
            fr"|(?:USAGE{SPACE})?(?:IS{SPACE})?(?P<usage>BINARY|COMPUTATIONAL-1|COMPUTATIONAL-2|COMPUTATIONAL-3|COMPUTATIONAL-4|COMPUTATIONAL|COMP-1|COMP-2|COMP-3|COMP-4|COMP|DISPLAY|PACKED-DECIMAL)(?!-)"
            fr"|(?:VALUE{SPACE})(?:IS{SPACE})?(?P<value>'.*'|\".*\"|\S+)"
            # r"|(?P<filler>FILLER)"
            # fr"|(?P<name>{NAME})"
            fr"|(?P<level>{NAME}){SPACE}(?P<data_name>{NAME})"
            )

        self.clause_pattern = re.compile(CLAUSES, re.IGNORECASE)

        self.pic_pattern = re.compile(
            r"(?P<sign>\+|-|S|DB|CR)"
            r"|(?P<char>\$|,|/|\*|B)"
            r"|(?P<decimal>V|\.)"
            # r"|(?P<repeat>[AX9Z0]\(\d+\))"
            r"|(?P<repeat>[AX9Z0]\(\d+\)|[AX9Z0]+)",
            # r"|(?P<digit>[AX9Z0]+)",
            re.IGNORECASE
        )


    def comp3_size(self, length):
        return int((length+2)/2)


    def comp_size(self, length):
        if length <= 4:
            return 2
        elif length <= 9:
            return 4
        else:
            return 8

    def seq_num(self, seed):
        while True:
            yield seed
            seed += 1

    def dict_add_value(self, dict_obj, key, value):
        if key not in dict_obj:
            dict_obj[key] = value
        elif isinstance(dict_obj[key], list):
            dict_obj[key].append(value)
        else:
            dict_obj[key] = [dict_obj[key], value]

    def expand_repeat(self, group_dict):
        
        if "repeat" in group_dict:
            count = ''
            char = ''

            if isinstance(group_dict["repeat"], list):
                for repeat_item in list(group_dict["repeat"]):
                    if re.match(r"[AX9Z0]\(\d+\)", repeat_item):
                        char, left, *count, right = repeat_item
                        digit_val = int("".join(count)) * char
                    else:
                        digit_val = repeat_item
                    self.dict_add_value(group_dict, 'digit', digit_val)
            else:
                repeat_item = group_dict["repeat"] 
                if re.match(r"[AX9Z0]\(\d+\)", repeat_item):
                    char, left, *count, right = repeat_item
                    digit_val = int("".join(count)) * char
                else:
                    digit_val = repeat_item
                self.dict_add_value(group_dict, 'digit', digit_val)


        return group_dict


    def normalize_picture(self, picture:dict):

        matches = list(self.pic_pattern.finditer(picture))
        pic_dict = {}

        for match in matches:
            temp_dict = dict((key, val) for key, val in match.groupdict().items() if val)
            if temp_dict:
                if 'sign' in temp_dict:
                    self.dict_add_value(pic_dict, 'sign', temp_dict['sign'])
                if 'char' in temp_dict:
                    self.dict_add_value(pic_dict, 'char', temp_dict['char'])
                if 'decimal' in temp_dict:
                    self.dict_add_value(pic_dict, 'decimal', temp_dict['decimal'])
                if 'repeat' in temp_dict:
                    self.dict_add_value(pic_dict, 'repeat', temp_dict['repeat'])
                if 'digit' in temp_dict:
                    self.dict_add_value(pic_dict, 'digit', temp_dict['digit'])

        normalized = self.expand_repeat(pic_dict)
        return normalized


    def calc_offset(self, copy_book_list):

        offset = 0
        curr_length = 0
        prev_length = 0
        lrecl = 0
        lrecl_max = 0

        for idx, copy_item in enumerate(copy_book_list):
            if 'level' in copy_item:
                if copy_item['level'] == '01':
                    offset = 0
                    prev_length = 0

                if copy_item['level'] in ('88','66', '77'):
                    continue

            if 'redefines' in copy_item:
                fil_dict = self.filter_dict(copy_book_list, 'data_name', copy_item['redefines'])
                if fil_dict:
                    if 'offset' in fil_dict:
                        offset = fil_dict['offset']
                else:
                    raise Exception("Redefines data name not found :", copy_item['redefines'])
            else:
                offset = offset + prev_length

            copy_book_list[idx]['offset'] = offset

            if 'storage_length' in copy_item:
                curr_length = copy_item['storage_length']
            else:
                curr_length = 0

            lrecl = offset + curr_length
            if lrecl_max < lrecl:
                lrecl_max = lrecl

            prev_length = curr_length

        copy_book_list[0].update({'lrecl_max': lrecl_max})

        return copy_book_list


    def getchild_items(self, copy_book_list, occurs_level_depth):

        copy_book_list_2 = []

        for idx, copy_item in enumerate(copy_book_list):
            if copy_item["level_depth_9"] > occurs_level_depth:
                copy_book_list_2.append(copy_item)
            else:
                break
        return copy_book_list_2


    def repeat_items(self, copy_book_list, occurs_level_depth):

        copy_book_list_2 = []
        idx_skip = 0
        idx_skip_no = 0

        for idx, copy_item in enumerate(copy_book_list):
            if copy_item["level_depth_9"] > occurs_level_depth:

                if idx_skip > idx:
                    continue

                if 'occurs' in copy_item:
                    if copy_item['occurs'] == True:
                        if 'group' in copy_item:
                            if copy_item['group'] == True:
                                occurs_level_depth = copy_item['level_depth_9']
                                copy_book_list_2.append(copy_item.copy())
                                child_items = self.getchild_items(copy_book_list[idx+1:], occurs_level_depth)
                                idx_skip = idx+1 + len(child_items)
                                repeat_items = self.repeat_items(child_items, occurs_level_depth)

                                for seq in range(copy_item['occurs_max']):
                                    for idx, child_copy_item in enumerate(repeat_items):
                                        idx_skip_no = idx_skip_no + 1
                                        new_data_name = '{}-{}'.format(child_copy_item['data_name'], seq+1)
                                        copy_book_list_2.append(child_copy_item.copy())
                                        copy_book_list_2[-1]['data_name'] = new_data_name
                                        copy_book_list_2[-1]['occurs_now'] = seq+1

                        elif 'picture' in copy_item:
                            for seq in range(copy_item['occurs_max']):
                                idx_skip_no = idx_skip_no + 1
                                new_data_name = '{}-{}'.format(copy_item['data_name'], seq+1)
                                copy_book_list_2.append(copy_item.copy())
                                copy_book_list_2[-1]['data_name'] = new_data_name
                                copy_book_list_2[-1]['occurs_now'] = seq+1
                                if 'redefines' in copy_item:
                                    if seq > 0:
                                        del copy_book_list_2[-1]['redefines']
                else:
                    copy_book_list_2.append(copy_item.copy())
            else:
                break
        return copy_book_list_2


    def flatten_occurs(self, copy_book_list):
        #
        # De-normalize Occurs
        #
        occurs_item = []
        occurs_nested = []

        copy_book_list_2 = []
        occurs_level_depth = 0
        idx_skip = 0
        idx_skip_no = 0

        for idx, copy_item in enumerate(copy_book_list):

            if idx_skip > idx:
                continue

            if 'occurs' in copy_item:
                if copy_item['occurs'] == True:
                    if 'group' in copy_item:
                        if copy_item['group'] == True:
                            occurs_level_depth = copy_item['level_depth_9']
                            copy_book_list_2.append(copy_item.copy())
                            child_items = self.getchild_items(copy_book_list[idx+1:], occurs_level_depth)
                            idx_skip = idx+1 + len(child_items)
                            repeat_items = self.repeat_items(child_items, occurs_level_depth)

                            for seq in range(copy_item['occurs_max']):
                                for idx, child_copy_item in enumerate(repeat_items):
                                    idx_skip_no = idx_skip_no + 1
                                    new_data_name = '{}-{}'.format(child_copy_item['data_name'], seq+1)
                                    copy_book_list_2.append(child_copy_item.copy())
                                    copy_book_list_2[-1]['data_name'] = new_data_name
                                    copy_book_list_2[-1]['occurs_now'] = seq+1

                    elif 'picture' in copy_item:
                        for seq in range(copy_item['occurs_max']):
                            idx_skip_no = idx_skip_no + 1
                            new_data_name = '{}-{}'.format(copy_item['data_name'], seq+1)
                            copy_book_list_2.append(copy_item.copy())
                            copy_book_list_2[-1]['data_name'] = new_data_name
                            copy_book_list_2[-1]['occurs_now'] = seq+1
                            if 'redefines' in copy_item:
                                if seq > 0:
                                    del copy_book_list_2[-1]['redefines']
            else:
                copy_book_list_2.append(copy_item.copy())

        return copy_book_list_2



    def group_usage(self, copy_book_list):
        #
        # De-normalize Occurs
        #
        occurs_item = []
        occurs_nested = []

        copy_book_list_2 = []
        occurs_level_depth = 0
        idx_skip = 0
        idx_skip_no = 0

        for idx, copy_item in enumerate(copy_book_list):

            if idx_skip > idx:
                continue

            if 'group' in copy_item:
                if copy_item['group'] == True:
                    if 'usage' in copy_item:
                        occurs_level_depth = copy_item['level_depth_9']
                        copy_book_list_2.append(copy_item.copy())
                        child_items = self.getchild_items(copy_book_list[idx+1:], occurs_level_depth)
                        idx_skip = idx + 1 + len(child_items)

                        for idx, child_copy_item in enumerate(child_items):
                            idx_skip_no = idx_skip_no + 1
                            copy_book_list_2.append(child_copy_item.copy())
                            copy_book_list_2[-1]['usage'] = copy_item['usage']
                            if copy_book_list_2[-1]['usage'] == 'COMP':
                                copy_book_list_2[-1]['storage_length'] = self.comp_size(copy_book_list_2[-1]['disp_length'])
                            elif copy_book_list_2[-1]['usage'] == 'COMP-3':
                                copy_book_list_2[-1]['storage_length'] = self.comp3_size(copy_book_list_2[-1]['disp_length'])
                            else:
                                copy_book_list_2[-1]['storage_length'] = copy_book_list_2[-1]['disp_length']
                    else:
                        copy_book_list_2.append(copy_item.copy())
            else:
                copy_book_list_2.append(copy_item.copy())

        return copy_book_list_2


    def filter_dict(self, list_dict, key, value):
        result = None
        for dict in list_dict:
            if dict[key] == value:
                result = dict
                break
        return result

    def cpy2json(self, copy_book):

        occurs_depending_on = ['OCCURS', 'DEPENDING']

        for copy_line in copy_book:
            if all(copy_key in occurs_depending_on for copy_key in copy_line):
                variable_length = True
                print ('OCCURS DEPENDING ON found in this copybook')

        copy_book_list = []

        for copy_line in copy_book:
            copy_line_str = ' '.join(copy_line)

            copy_line_dict = {}

            for match in self.clause_pattern.finditer(copy_line_str):
                copy_line_dict.update(dict((key, val) for key, val in match.groupdict().items() if val))

            copy_line_dict['level'] = copy_line[0]
            copy_line_dict['data_name'] = copy_line[1]

            n_picture = {}

            if 'picture' in copy_line_dict:
                n_picture = self.normalize_picture(copy_line_dict['picture'])
                copy_line_dict.update(n_picture)

            if 'usage' in copy_line_dict:
                if copy_line_dict['usage'] in ('BINARY', 'COMP', 'COMPUTATIONAL'):
                    copy_line_dict['usage'] = 'COMP'
                elif copy_line_dict['usage'] in ('PACKED-DECIMAL', 'COMP-3', 'COMPUTATIONAL-3'):
                    copy_line_dict['usage'] = 'COMP-3'
                elif copy_line_dict['usage'] in ('COMP-4', 'COMPUTATIONAL-4'):
                    copy_line_dict['usage'] = 'COMP-4'
                elif copy_line_dict['usage'] in ('COMP-1', 'COMPUTATIONAL-1'):
                    copy_line_dict['usage'] = 'COMP-1'
                elif copy_line_dict['usage'] in ('COMP-2', 'COMPUTATIONAL-2'):
                    copy_line_dict['usage'] = 'COMP-2'
                elif copy_line_dict['usage'] in ('DISPLAY'):
                    copy_line_dict['usage'] = 'DISPLAY'
                else:
                    print ('Unknown USAGE clause value : {}', copy_line_dict['usage'])
            
            if 'digit' in copy_line_dict:
                
                if isinstance(copy_line_dict['digit'], list):
                    if copy_line_dict['digit'][0].isnumeric():
                        if copy_line_dict['digit'][1]:
                            copy_line_dict['scale'] = len(copy_line_dict['digit'][1])
                        else:
                            copy_line_dict['scale'] = 0

                copy_line_dict['digit'] = ''.join(copy_line_dict['digit'])
                copy_line_dict['disp_length'] = len(copy_line_dict['digit'])

                if copy_line_dict['digit'].isnumeric():
                    copy_line_dict['numeric'] = True
                    if not 'scale' in copy_line_dict:
                        copy_line_dict['scale'] = 0
                else:
                    copy_line_dict['numeric'] = False

                copy_line_dict['storage_length'] = copy_line_dict['disp_length']

                if copy_line_dict['numeric'] == True:
                    if 'sign' in copy_line_dict:
                        copy_line_dict['signed'] = True

                    if 'usage' in copy_line_dict:
                        if copy_line_dict['usage'] == 'COMP':
                            copy_line_dict['storage_length'] = self.comp_size(copy_line_dict['disp_length'])
                        elif copy_line_dict['usage'] == 'COMP-3':
                            copy_line_dict['storage_length'] = self.comp3_size(copy_line_dict['disp_length'])
                        else:
                            copy_line_dict['storage_length'] = copy_line_dict['disp_length']

            if 'occurs_fixed' in copy_line_dict:
                copy_line_dict['occurs'] = True
                copy_line_dict['occurs_min'] = int(copy_line_dict['occurs_fixed'])
                copy_line_dict['occurs_max'] = int(copy_line_dict['occurs_fixed'])
                del copy_line_dict['occurs_fixed']

            if 'depending_on' in copy_line_dict:
                copy_line_dict['occurs'] = True
                copy_line_dict['occurs_min'] = int(copy_line_dict['occurs_min'])
                copy_line_dict['occurs_max'] = int(copy_line_dict['occurs_min'])

            if len(copy_line) <= 2:
                copy_line_dict['group'] = True

            
            # if not variable_length:
            if 'repeat' in copy_line_dict:
                del copy_line_dict['repeat']
            if 'digit' in copy_line_dict:
                del copy_line_dict['digit']
            copy_book_list.append(copy_line_dict)

        #
        # Add Group and Level depth
        #
        curr_level = ''
        prev_level = ''
        level_depth = []

        for idx, copy_item in enumerate(copy_book_list):
            if 'level' in copy_item:
                curr_level = copy_item['level']
                if copy_item['level'] == '01':
                    prev_level = ''
                    copy_book_list[idx]['group'] = True
                    level_depth = [curr_level]

                if copy_item['level'] in ('88','66', '77'):
                    print('Unsupported level number : {} {}. Skipping this...'.format(copy_item['level'], copy_item['data_name']))
                    continue

                if prev_level:
                    if int(curr_level) > int(prev_level):
                        level_depth.append(curr_level)
                        copy_book_list[idx-1]['group'] = True
                    elif int(curr_level) < int(prev_level):
                        if curr_level in level_depth:
                            level_depth_idx = level_depth.index(curr_level)
                            level_depth = level_depth[:level_depth_idx+1]
                        else:
                            level_depth.pop(-1)

                copy_book_list[idx]['level_depth_x'] = '-'.join(level_depth)
                copy_book_list[idx]['level_depth_9'] = len(level_depth)

            prev_level = curr_level

        # #
        # # De-normalize Occurs - OCCURS with PIC
        # #

        copy_book_list = self.group_usage(copy_book_list)
        copy_book_list = self.flatten_occurs(copy_book_list)
        copy_book_list = self.calc_offset(copy_book_list)

        #
        # Add seq number
        #
        copy_book_dict_out = {}
        for idx, copy_item in enumerate(copy_book_list):
            copy_book_dict_out[idx] = copy_item

        return copy_book_dict_out


    def cpyparser(self, tokenizer):
        
        seqnum = self.seq_num(100)

        copy_line = []
        copy_book = []

        token = tokenizer.getToken()
        while token:

            # Validate level number
            if not token.isnumeric():
                raise ValueError('Parsing error: The Level Number must be numeric. But found ', token)
            copy_line.append(token)
            token = tokenizer.getToken()

            # Validate data name
            if token in self.keywords:
                if token == "FILLER":
                    copy_line.append("FILLER-X-{}".format(next(seqnum)))
                    print ('Replacing data name {} with {} ...'.format(token, copy_line[-1]))
                    token = tokenizer.getToken()
                else:
                    copy_line.append("NODATANAME-X-{}".format(next(seqnum)))
                    print ('No Data Name provided. Adding data name {} ...'.format(copy_line[-1]))
            else:
                copy_line.append(token)
                token = tokenizer.getToken()

            while token:
                if token == '.':
                    copy_book.append(copy_line)
                    copy_line = []
                    token = tokenizer.getToken()
                    break
                copy_line.append(token)
                token = tokenizer.getToken()

        copy_json = self.cpy2json(copy_book)

        return copy_json