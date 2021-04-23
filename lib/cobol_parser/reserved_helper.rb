# frozen_string_literal: true

require "ostruct"

module CobolParser::ReservedHelper
  system_table = [
    ["SYSIN", :DEVICE_NAME, :DEVICE_SYSIN],
    ["SYSIPT", :DEVICE_NAME, :DEVICE_SYSIN],
    ["SYSOUT", :DEVICE_NAME, :DEVICE_SYSOUT],
    ["SYSLIST", :DEVICE_NAME, :DEVICE_SYSOUT],
    ["SYSLST", :DEVICE_NAME, :DEVICE_SYSOUT],
    ["PRINTER", :DEVICE_NAME, :DEVICE_SYSOUT],
    ["SYSERR", :DEVICE_NAME, :DEVICE_SYSERR],
    ["CONSOLE", :DEVICE_NAME, :DEVICE_CONSOLE],
    ["C01", :FEATURE_NAME, :FEATURE_C01],
    ["C02", :FEATURE_NAME, :FEATURE_C02],
    ["C03", :FEATURE_NAME, :FEATURE_C03],
    ["C04", :FEATURE_NAME, :FEATURE_C04],
    ["C05", :FEATURE_NAME, :FEATURE_C05],
    ["C06", :FEATURE_NAME, :FEATURE_C06],
    ["C07", :FEATURE_NAME, :FEATURE_C07],
    ["C08", :FEATURE_NAME, :FEATURE_C08],
    ["C09", :FEATURE_NAME, :FEATURE_C09],
    ["C10", :FEATURE_NAME, :FEATURE_C10],
    ["C11", :FEATURE_NAME, :FEATURE_C11],
    ["C12", :FEATURE_NAME, :FEATURE_C12],
    ["FORMFEED", :FEATURE_NAME, :FEATURE_FORMFEED],
    ["SWITCH-1", :SWITCH_NAME, :SWITCH_1],
    ["SWITCH-2", :SWITCH_NAME, :SWITCH_2],
    ["SWITCH-3", :SWITCH_NAME, :SWITCH_3],
    ["SWITCH-4", :SWITCH_NAME, :SWITCH_4],
    ["SWITCH-5", :SWITCH_NAME, :SWITCH_5],
    ["SWITCH-6", :SWITCH_NAME, :SWITCH_6],
    ["SWITCH-7", :SWITCH_NAME, :SWITCH_7],
    ["SWITCH-8", :SWITCH_NAME, :SWITCH_8],
  ].freeze
  SYSTEM_TABLE = system_table.to_h { |x|
    [
      x[0].upcase,
      OpenStruct.new(
        name: x[0],
        system_category: x[1],
        token: x[2],
        node: nil
      ),
    ]
  }

  function_list = [
    ["ABS", 1, 1, :ABS,
     "cob_intr_abs",
     :NUMERIC, 0,],
    ["ACOS", 1, 1, :ACOS,
     "cob_intr_acos",
     :NUMERIC, 0,],
    ["ANNUITY", 2, 1, :ANNUITY,
     "cob_intr_annuity",
     :NUMERIC, 0,],
    ["ASIN", 1, 1, :ASIN,
     "cob_intr_asin",
     :NUMERIC, 0,],
    ["ATAN", 1, 1, :ATAN,
     "cob_intr_atan",
     :NUMERIC, 0,],
    ["BOOLEAN-OF-INTEGER", 2, 0, :BOOLEAN_OF_INTEGER,
     nil,
     :NUMERIC, 0,],
    ["BYTE-LENGTH", 1, 1, :BYTE_LENGTH,
     "cob_intr_length",
     :NUMERIC, 0,],
    ["CHAR", 1, 1, :CHAR,
     "cob_intr_char",
     :ALPHANUMERIC, 0,],
    ["CHAR-NATIONAL", 1, 0, :CHAR_NATIONAL,
     nil,
     :ALPHANUMERIC, 0,],
    ["COMBINED-DATETIME", 2, 1, :COMBINED_DATETIME,
     "cob_intr_combined_datetime",
     :NUMERIC, 0,],
    ["CONCATENATE", -1, 1, :CONCATENATE,
     "cob_intr_concatenate",
     :ALPHANUMERIC, 1,],
    ["COS", 1, 1, :COS,
     "cob_intr_cos",
     :NUMERIC, 0,],
    ["CURRENT-DATE", 0, 1, :CURRENT_DATE,
     "cob_intr_current_date",
     :ALPHANUMERIC, 1,],
    ["DATE-OF-INTEGER", 1, 1, :DATE_OF_INTEGER,
     "cob_intr_date_of_integer",
     :NUMERIC, 0,],
    ["DATE-TO-YYYYMMDD", -1, 1, :DATE_TO_YYYYMMDD,
     "cob_intr_date_to_yyyymmdd",
     :NUMERIC, 0,],
    ["DAY-OF-INTEGER", 1, 1, :DAY_OF_INTEGER,
     "cob_intr_day_of_integer",
     :NUMERIC, 0,],
    ["DAY-TO-YYYYDDD", -1, 1, :DAY_TO_YYYYDDD,
     "cob_intr_day_to_yyyyddd",
     :NUMERIC, 0,],
    ["DISPLAY-OF", -1, 0, :DISPLAY_OF,
     nil,
     :ALPHANUMERIC, 0,],
    ["E", 0, 1, :E,
     nil,
     :NUMERIC, 0,],
    ["EXCEPTION-FILE", 0, 1, :EXCEPTION_FILE,
     "cob_intr_exception_file",
     :ALPHANUMERIC, 0,],
    ["EXCEPTION-FILE-N", 0, 0, :EXCEPTION_FILE_N,
     nil,
     :ALPHANUMERIC, 0,],
    ["EXCEPTION-LOCATION", 0, 1, :EXCEPTION_LOCATION,
     "cob_intr_exception_location",
     :ALPHANUMERIC, 0,],
    ["EXCEPTION-LOCATION-N", 0, 0, :EXCEPTION_LOCATION_N,
     nil,
     :ALPHANUMERIC, 0,],
    ["EXCEPTION-STATEMENT", 0, 1, :EXCEPTION_STATEMENT,
     "cob_intr_exception_statement",
     :ALPHANUMERIC, 0,],
    ["EXCEPTION-STATUS", 0, 1, :EXCEPTION_STATUS,
     "cob_intr_exception_status",
     :ALPHANUMERIC, 0,],
    ["EXP", 1, 1, :EXP,
     "cob_intr_exp",
     :NUMERIC, 0,],
    ["EXP10", 1, 1, :EXP10,
     "cob_intr_exp10",
     :NUMERIC, 0,],
    ["FACTORIAL", 1, 1, :FACTORIAL,
     "cob_intr_factorial",
     :NUMERIC, 0,],
    ["FRACTION-PART", 1, 1, :FRACTION_PART,
     "cob_intr_fraction_part",
     :NUMERIC, 0,],
    ["HIGHEST-ALGEBRAIC", 1, 0, :HIGHEST_ALGEBRAIC,
     nil,
     :NUMERIC, 0,],
    ["INTEGER", 1, 1, :INTEGER,
     "cob_intr_integer",
     :NUMERIC, 0,],
    ["INTEGER-OF-BOOLEAN", 1, 0, :INTEGER_OF_BOOLEAN,
     nil,
     :NUMERIC, 0,],
    ["INTEGER-OF-DATE", 1, 1, :INTEGER_OF_DATE,
     "cob_intr_integer_of_date",
     :NUMERIC, 0,],
    ["INTEGER-OF-DAY", 1, 1, :INTEGER_OF_DAY,
     "cob_intr_integer_of_day",
     :NUMERIC, 0,],
    ["INTEGER-PART", 1, 1, :INTEGER_PART,
     "cob_intr_integer_part",
     :NUMERIC, 0,],
    ["LENGTH", 1, 1, :LENGTH,
     "cob_intr_length",
     :NUMERIC, 0,],
    ["LOCALE-COMPARE", -1, 0, :LOCALE_COMPARE,
     nil,
     :ALPHANUMERIC, 0,],
    ["LOCALE-DATE", 2, 1, :LOCALE_DATE,
     "cob_intr_locale_date",
     :ALPHANUMERIC, 1,],
    ["LOCALE-TIME", 2, 1, :LOCALE_TIME,
     "cob_intr_locale_time",
     :ALPHANUMERIC, 1,],
    ["LOCALE-TIME-FROM-SECONDS", 2, 1, :LOCALE_TIME_FROM_SECS,
     "cob_intr_lcl_time_from_secs",
     :ALPHANUMERIC, 1,],
    ["LOG", 1, 1, :LOG,
     "cob_intr_log",
     :NUMERIC, 0,],
    ["LOG10", 1, 1, :LOG10,
     "cob_intr_log10",
     :NUMERIC, 0,],
    ["LOWER-CASE", 1, 1, :LOWER_CASE,
     "cob_intr_lower_case",
     :ALPHANUMERIC, 1,],
    ["LOWEST-ALGEBRAIC", 1, 0, :LOWEST_ALGEBRAIC,
     nil,
     :NUMERIC, 0,],
    ["MAX", -1, 1, :MAX,
     "cob_intr_max",
     :NUMERIC, 0,],
    ["MEAN", -1, 1, :MEAN,
     "cob_intr_mean",
     :NUMERIC, 0,],
    ["MEDIAN", -1, 1, :MEDIAN,
     "cob_intr_median",
     :NUMERIC, 0,],
    ["MIDRANGE", -1, 1, :MIDRANGE,
     "cob_intr_midrange",
     :NUMERIC, 0,],
    ["MIN", -1, 1, :MIN,
     "cob_intr_min",
     :NUMERIC, 0,],
    ["MOD", 2, 1, :MOD,
     "cob_intr_mod",
     :NUMERIC, 0,],
    ["NATIONAL-OF", -1, 0, :NATIONAL_OF,
     nil,
     :ALPHANUMERIC, 0,],
    ["NUMVAL", 1, 1, :NUMVAL,
     "cob_intr_numval",
     :NUMERIC, 0,],
    ["NUMVAL-C", 2, 1, :NUMVAL_C,
     "cob_intr_numval_c",
     :NUMERIC, 0,],
    ["NUMVAL-F", 1, 0, :NUMVAL_F,
     nil,
     :NUMERIC, 0,],
    ["ORD", 1, 1, :ORD,
     "cob_intr_ord",
     :NUMERIC, 0,],
    ["ORD-MAX", -1, 1, :ORD_MAX,
     "cob_intr_ord_max",
     :NUMERIC, 0,],
    ["ORD-MIN", -1, 1, :ORD_MIN,
     "cob_intr_ord_min",
     :NUMERIC, 0,],
    ["PI", 0, 1, :PI,
     nil,
     :NUMERIC, 0,],
    ["PRESENT-VALUE", -1, 1, :PRESENT_VALUE,
     "cob_intr_present_value",
     :NUMERIC, 0,],
    ["RANDOM", -1, 1, :RANDOM,
     "cob_intr_random",
     :NUMERIC, 0,],
    ["RANGE", -1, 1, :RANGE,
     "cob_intr_range",
     :NUMERIC, 0,],
    ["REM", 2, 1, :REM,
     "cob_intr_rem",
     :NUMERIC, 0,],
    ["REVERSE", 1, 1, :REVERSE,
     "cob_intr_reverse",
     :ALPHANUMERIC, 1,],
    ["SECONDS-FROM-FORMATTED-TIME", 2, 1, :SECONDS_PAST_MIDNIGHT,
     "cob_intr_seconds_from_formatted_time",
     :NUMERIC, 0,],
    ["SECONDS-PAST-MIDNIGHT", 0, 1, :SECONDS_PAST_MIDNIGHT,
     "cob_intr_seconds_past_midnight",
     :NUMERIC, 0,],
    ["SIGN", 1, 1, :SIGN,
     "cob_intr_sign",
     :NUMERIC, 0,],
    ["SIN", 1, 1, :SIN,
     "cob_intr_sin",
     :NUMERIC, 0,],
    ["SQRT", 1, 1, :SQRT,
     "cob_intr_sqrt",
     :NUMERIC, 0,],
    ["STANDARD-COMPARE", -1, 0, :STANDARD_COMPARE,
     nil,
     :ALPHANUMERIC, 0,],
    ["STANDARD-DEVIATION", -1, 1, :STANDARD_DEVIATION,
     "cob_intr_standard_deviation",
     :NUMERIC, 0,],
    ["STORED-CHAR-LENGTH", 1, 1, :STORED_CHAR_LENGTH,
     "cob_intr_stored_char_length",
     :NUMERIC, 0,],
    ["SUBSTITUTE", -1, 1, :SUBSTITUTE,
     "cob_intr_substitute",
     :ALPHANUMERIC, 1,],
    ["SUBSTITUTE-CASE", -1, 1, :SUBSTITUTE_CASE,
     "cob_intr_substitute_case",
     :ALPHANUMERIC, 1,],
    ["SUM", -1, 1, :SUM,
     "cob_intr_sum",
     :NUMERIC, 0,],
    ["TAN", 1, 1, :TAN,
     "cob_intr_tan",
     :NUMERIC, 0,],
    ["TEST-DATE-YYYYMMDD", 1, 1, :TEST_DATE_YYYYMMDD,
     "cob_intr_test_date_yyyymmdd",
     :NUMERIC, 0,],
    ["TEST-DAY-YYYYDDD", 1, 1, :TEST_DAY_YYYYDDD,
     "cob_intr_test_day_yyyyddd",
     :NUMERIC, 0,],
    ["TEST-NUMVAL", 1, 0, :TEST_NUMVAL,
     nil,
     :NUMERIC, 0,],
    ["TEST-NUMVAL-C", -1, 0, :TEST_NUMVAL_C,
     nil,
     :NUMERIC, 0,],
    ["TEST-NUMVAL-F", 1, 0, :TEST_NUMVAL_F,
     nil,
     :NUMERIC, 0,],
    ["TRIM", 2, 1, :TRIM,
     "cob_intr_trim",
     :ALPHANUMERIC, 1,],
    ["UPPER-CASE", 1, 1, :UPPER_CASE,
     "cob_intr_upper_case",
     :ALPHANUMERIC, 1,],
    ["VARIANCE", -1, 1, :VARIANCE,
     "cob_intr_variance",
     :NUMERIC, 0,],
    ["WHEN-COMPILED", 0, 1, :WHEN_COMPILED,
     "cob_intr_when_compiled",
     :ALPHANUMERIC, 1,],
    ["YEAR-TO-YYYY", -1, 1, :YEAR_TO_YYYY,
     "cob_intr_year_to_yyyy",
     :NUMERIC, 0,],
  ]
  FUNCTION_LIST = function_list.to_h { |x|
    [
      x[0].upcase,
      OpenStruct.new(
        name: x[0],
        args: x[1],
        implemented: x[2] == 1,
        intr_enum: x[3],
        intr_routine: x[4],
        category: x[5],
        refmod: x[6] == 1
      ),
    ]
  }

  reserved_words = [
    ["ACCEPT", :ACCEPT], # 2002
    ["ACCESS", :ACCESS], # 2002
    ["ACTIVE-CLASS", -1], # 2002
    ["ADD", :ADD], # 2002
    ["ADDRESS", :ADDRESS], # 2002
    ["ADVANCING", :ADVANCING], # 2002
    ["AFTER", :AFTER], # 2002
    ["ALIGNED", -1], # 2002
    ["ALL", :ALL], # 2002
    ["ALLOCATE", :ALLOCATE], # 2002
    ["ALPHABET", :ALPHABET], # 2002
    ["ALPHABETIC", :ALPHABETIC], # 2002
    ["ALPHABETIC-LOWER", :ALPHABETIC_LOWER], # 2002
    ["ALPHABETIC-UPPER", :ALPHABETIC_UPPER], # 2002
    ["ALPHANUMERIC", :ALPHANUMERIC], # 2002
    ["ALPHANUMERIC-EDITED", :ALPHANUMERIC_EDITED], # 2002
    ["ALSO", :ALSO], # 2002
    ["ALTER", :ALTER], # 85
    ["ALTERNATE", :ALTERNATE], # 2002
    ["AND", :AND], # 2002
    ["ANY", :ANY], # 2002
    ["ANYCASE", -1], # 2002
    ["ARE", :ARE], # 2002
    ["AREA", :AREA], # 2002
    ["AREAS", :AREA], # 2002
    ["ARGUMENT-NUMBER", :ARGUMENT_NUMBER], # extension
    ["ARGUMENT-VALUE", :ARGUMENT_VALUE], # extension
    ["ARITHMETIC", -1], # 2002 (C/S)
    ["AS", :AS], # 2002
    ["ASCENDING", :ASCENDING], # 2002
    ["ASSIGN", :ASSIGN], # 2002
    ["AT", :AT], # 2002
    ["ATTRIBUTE", -1], # 2002 (C/S)
    ["AUTO", :AUTO], # 2002 (C/S)
    ["AUTO-SKIP", :AUTO], # extension
    ["AUTOMATIC", :AUTOMATIC], # extension
    ["AUTOTERMINATE", :AUTO], # extension
    ["B-AND", -1], # 2002
    ["B-NOT", -1], # 2002
    ["B-OR", -1], # 2002
    ["B-XOR", -1], # 2002
    ["BACKGROUND-COLOR", :BACKGROUND_COLOR], # 2002 (C/S)
    ["BASED", :BASED], # 2002
    ["BEEP", :BELL], # extension
    ["BEFORE", :BEFORE], # 2002
    ["BELL", :BELL], # 2002 (C/S)
    ["BINARY", :BINARY], # 2002
    ["BINARY-C-LONG", :BINARY_C_LONG], # Extension
    ["BINARY-CHAR", :BINARY_CHAR], # 2002
    ["BINARY-DOUBLE", :BINARY_DOUBLE], # 2002
    ["BINARY-LONG", :BINARY_LONG], # 2002
    ["BINARY-SHORT", :BINARY_SHORT], # 2002
    ["BIT", -1], # 2002
    ["BLANK", :BLANK], # 2002
    ["BLINK", :BLINK], # 2002 (C/S)
    ["BLOCK", :BLOCK], # 2002
    ["BOOLEAN", -1], # 2002
    ["BOTTOM", :BOTTOM], # 2002
    ["BY", :BY], # 2002
    ["BYTE-LENGTH", :BYTE_LENGTH], # 2002 (C/S)
    ["CALL", :CALL], # 2002
    ["CANCEL", :CANCEL], # 2002
    ["CD", -1], # 2002
    ["CENTER", -1], # 2002 (C/S)
    ["CF", :CONTROL_FOOTING], # 2002
    ["CH", :CONTROL_HEADING], # 2002
    ["CHAIN", -1], # extension
    ["CHAINING", :CHAINING], # extension
    ["CHARACTER", :CHARACTER], # 2002
    ["CHARACTERS", :CHARACTERS], # 2002
    ["CLASS", :CLASS], # 2002
    ["CLASS-ID", -1], # 2002
    ["CLASSIFICATION", -1], # 2002 (C/S)
    ["CLOSE", :CLOSE], # 2002
    ["CODE", :CODE], # 2002
    ["CODE-SET", :CODE_SET], # 2002
    ["COL", :COLUMN], # 2002
    ["COLLATING", :COLLATING], # 2002
    ["COLS", :COLUMNS], # 2002
    ["COLUMN", :COLUMN], # 2002
    ["COLUMNS", :COLUMNS], # 2002
    ["COMMA", :COMMA], # 2002
    ["COMMAND-LINE", :COMMAND_LINE], # extension
    ["COMMIT", :COMMIT], # extension
    ["COMMON", :COMMON], # 2002
    ["COMMUNICATION", -1], # 2002
    ["COMP", :COMP], # 2002
    # #ifdef __MVS__ # EBCDIC!
    # ["COMP-X", :COMP_X], # extension
    # #endif
    ["COMP-1", :COMP_1], # extension
    ["COMP-2", :COMP_2], # extension
    ["COMP-3", :COMP_3], # extension
    ["COMP-4", :COMP_4], # extension
    ["COMP-5", :COMP_5], # extension
    # #ifndef __MVS__
    #   ["COMP-X", :COMP_X], # extension
    # #endif
    ["COMPUTATIONAL", :COMP], # 2002
    # #ifdef __MVS__ # EBCDIC!
    #   ["COMPUTATIONAL-X", :COMP_X], # extension
    # #endif
    ["COMPUTATIONAL-1", :COMP_1], # extension
    ["COMPUTATIONAL-2", :COMP_2], # extension
    ["COMPUTATIONAL-3", :COMP_3], # extension
    ["COMPUTATIONAL-4", :COMP_4], # extension
    ["COMPUTATIONAL-5", :COMP_5], # extension
    # #ifndef __MVS__
    #   ["COMPUTATIONAL-X", :COMP_X], # extension
    # #endif
    ["COMPUTE", :COMPUTE], # 2002
    ["CONDITION", -1], # 2002
    ["CONFIGURATION", :CONFIGURATION], # 2002
    ["CONSTANT", :CONSTANT], # 2002
    ["CONTAINS", :CONTAINS], # 2002
    ["CONTENT", :CONTENT], # 2002
    ["CONTINUE", :CONTINUE], # 2002
    ["CONTROL", :CONTROL], # 2002
    ["CONTROLS", :CONTROL], # 2002
    ["CONVERTING", :CONVERTING], # 2002
    ["COPY", nil], # 2002
    ["CORR", :CORRESPONDING], # 2002
    ["CORRESPONDING", :CORRESPONDING], # 2002
    ["COUNT", :COUNT], # 2002
    ["CRT", :CRT], # 2002
    ["CURRENCY", :CURRENCY], # 2002
    ["CURSOR", :CURSOR], # 2002
    ["CYCLE", :CYCLE], # 2002 (C/S)
    ["DATA", :DATA], # 2002
    ["DATA-POINTER", -1], # 2002
    ["DATE", :DATE], # 2002
    ["DAY", :DAY], # 2002
    ["DAY-OF-WEEK", :DAY_OF_WEEK], # 2002
    ["DE", :DETAIL], # 2002
    ["DEBUGGING", :DEBUGGING], # 2002
    ["DECIMAL-POINT", :DECIMAL_POINT], # 2002
    ["DECLARATIVES", :DECLARATIVES], # 2002
    ["DEFAULT", :DEFAULT], # 2002
    ["DELETE", :DELETE], # 2002
    ["DELIMITED", :DELIMITED], # 2002
    ["DELIMITER", :DELIMITER], # 2002
    ["DEPENDING", :DEPENDING], # 2002
    ["DESCENDING", :DESCENDING], # 2002
    ["DESTINATION", -1], # 2002
    ["DETAIL", :DETAIL], # 2002
    ["DISABLE", -1], # 2002
    ["DISK", :DISK], # extension
    ["DISPLAY", :DISPLAY], # 2002
    ["DIVIDE", :DIVIDE], # 2002
    ["DIVISION", :DIVISION], # 2002
    ["DOWN", :DOWN], # 2002
    ["DUPLICATES", :DUPLICATES], # 2002
    ["DYNAMIC", :DYNAMIC], # 2002
    ["EBCDIC", :EBCDIC], # extension
    ["EC", -1], # 2002
    ["EGI", -1], # 2002
    ["ELSE", :ELSE], # 2002
    ["EMI", -1], # 2002
    ["ENABLE", -1], # 2002
    ["END", :END], # 2002
    ["END-ACCEPT", :END_ACCEPT], # 2002
    ["END-ADD", :END_ADD], # 2002
    ["END-CALL", :END_CALL], # 2002
    ["END-COMPUTE", :END_COMPUTE], # 2002
    ["END-DELETE", :END_DELETE], # 2002
    ["END-DISPLAY", :END_DISPLAY], # 2002
    ["END-DIVIDE", :END_DIVIDE], # 2002
    ["END-EVALUATE", :END_EVALUATE], # 2002
    ["END-IF", :END_IF], # 2002
    ["END-MULTIPLY", :END_MULTIPLY], # 2002
    ["END-OF-PAGE", :EOP], # 2002
    ["END-PERFORM", :END_PERFORM], # 2002
    ["END-READ", :END_READ], # 2002
    ["END-RECEIVE", -1], # 2002
    ["END-RETURN", :END_RETURN], # 2002
    ["END-REWRITE", :END_REWRITE], # 2002
    ["END-SEARCH", :END_SEARCH], # 2002
    ["END-START", :END_START], # 2002
    ["END-STRING", :END_STRING], # 2002
    ["END-SUBTRACT", :END_SUBTRACT], # 2002
    ["END-UNSTRING", :END_UNSTRING], # 2002
    ["END-WRITE", :END_WRITE], # 2002
    ["ENTRY", :ENTRY], # extension
    ["ENTRY-CONVENTION", -1], # 2002 (C/S)
    ["ENVIRONMENT", :ENVIRONMENT], # 2002
    ["ENVIRONMENT-NAME", :ENVIRONMENT_NAME], # extension
    ["ENVIRONMENT-VALUE", :ENVIRONMENT_VALUE], # extension
    ["EO", -1], # 2002
    ["EOL", :EOL], # 2002 (C/S)
    ["EOP", :EOP], # 2002
    ["EOS", :EOS], # 2002 (C/S)
    ["EQUAL", :EQUAL], # 2002
    ["EQUALS", :EQUALS], # extension
    ["ERASE", :ERASE], # 2002 (C/S)
    ["ERROR", :ERROR], # 2002
    ["ESCAPE", :ESCAPE], # extension
    ["ESI", -1], # 2002
    ["EVALUATE", :EVALUATE], # 2002
    ["EXCEPTION", :EXCEPTION], # 2002
    ["EXCEPTION-OBJECT", -1], # 2002
    ["EXCLUSIVE", :EXCLUSIVE], # extension
    ["EXIT", :EXIT], # 2002
    ["EXPANDS", -1], # 2002 (C/S)
    ["EXTEND", :EXTEND], # 2002
    ["EXTERNAL", :EXTERNAL], # 2002
    ["FACTORY", -1], # 2002
    ["FALSE", :TOK_FALSE], # 2002
    ["FD", :FD], # 2002
    ["FILE", :TOK_FILE], # 2002
    ["FILE-CONTROL", :FILE_CONTROL], # 2002
    ["FILE-ID", :FILE_ID], # extension
    ["FILLER", :FILLER], # 2002
    ["FINAL", :FINAL], # 2002
    ["FIRST", :FIRST], # 2002
    ["FLOAT-BINARY-16", -1], # 2008
    ["FLOAT-BINARY-34", -1], # 2008
    ["FLOAT-BINARY-7", -1], # 2008
    ["FLOAT-DECIMAL-16", -1], # 2008
    ["FLOAT-DECIMAL-34", -1], # 2008
    ["FLOAT-EXTENDED", -1], # 2002
    ["FLOAT-LONG", :COMP_2], # 2002
    ["FLOAT-SHORT", :COMP_1], # 2002
    ["FOOTING", :FOOTING], # 2002
    ["FOR", :FOR], # 2002
    ["FOREGROUND-COLOR", :FOREGROUND_COLOR], # 2002 (C/S)
    ["FOREVER", :FOREVER], # 2002 (C/S)
    ["FORMAT", -1], # 2002
    ["FREE", :FREE], # 2002
    ["FROM", :FROM], # 2002
    ["FULL", :FULL], # 2002 (C/S)
    ["FUNCTION", :FUNCTION], # 2002
    ["FUNCTION-ID", :FUNCTION_ID], # 2002
    ["FUNCTION-POINTER", -1], # 2008
    ["GENERATE", :GENERATE], # 2002
    ["GET", -1], # 2002
    ["GIVING", :GIVING], # 2002
    ["GLOBAL", :GLOBAL], # 2002
    ["GO", :GO], # 2002
    ["GOBACK", :GOBACK], # 2002
    ["GREATER", :GREATER], # 2002
    ["GROUP", :GROUP], # 2002
    ["GROUP-USAGE", -1], # 2002
    ["HEADING", :HEADING], # 2002
    ["HIGH-VALUE", :HIGH_VALUE], # 2002
    ["HIGH-VALUES", :HIGH_VALUE], # 2002
    ["HIGHLIGHT", :HIGHLIGHT], # 2002 (C/S)
    ["I-O", :I_O], # 2002
    ["I-O-CONTROL", :I_O_CONTROL], # 2002
    ["ID", :IDENTIFICATION], # extension
    ["IDENTIFICATION", :IDENTIFICATION], # 2002
    ["IF", :IF], # 2002
    ["IGNORE", :IGNORE], # extension
    ["IGNORING", :IGNORING], # 2002 (C/S)
    ["IMPLEMENTS", -1], # 2002 (C/S)
    ["IN", :IN], # 2002
    ["INDEX", :INDEX], # 2002
    ["INDEXED", :INDEXED], # 2002
    ["INDICATE", :INDICATE], # 2002
    ["INFINITY", -1], # 2002
    ["INHERITS", -1], # 2002
    ["INITIAL", :TOK_INITIAL], # 2002
    ["INITIALIZE", :INITIALIZE], # 2002
    ["INITIALIZED", :INITIALIZED], # 2002
    ["INITIATE", :INITIATE], # 2002
    ["INPUT", :INPUT], # 2002
    ["INPUT-OUTPUT", :INPUT_OUTPUT], # 2002
    ["INSPECT", :INSPECT], # 2002
    ["INTERFACE", -1], # 2002
    ["INTERFACE-ID", -1], # 2002
    ["INTO", :INTO], # 2002
    ["INTRINSIC", :INTRINSIC], # 2002 (C/S)
    ["INVALID", :INVALID], # 2002
    ["INVOKE", -1], # 2002
    ["IS", :IS], # 2002
    ["JUST", :JUSTIFIED], # 2002
    ["JUSTIFIED", :JUSTIFIED], # 2002
    ["KEY", :KEY], # 2002
    ["LABEL", :LABEL], # 85
    ["LAST", :LAST], # 2002
    ["LC_ALL", -1], # 2002 (C/S)
    ["LC_COLLATE", -1], # 2002 (C/S)
    ["LC_CTYPE", -1], # 2002 (C/S)
    ["LC_MESSAGES", -1], # 2002 (C/S)
    ["LC_MONETARY", -1], # 2002 (C/S)
    ["LC_NUMERIC", -1], # 2002 (C/S)
    ["LC_TIME", -1], # 2002 (C/S)
    ["LEADING", :LEADING], # 2002
    ["LEFT", :LEFT], # 2002
    ["LENGTH", :LENGTH], # 2002
    ["LESS", :LESS], # 2002
    ["LIMIT", :LIMIT], # 2002
    ["LIMITS", :LIMITS], # 2002
    ["LINAGE", :LINAGE], # 2002
    ["LINAGE-COUNTER", :LINAGE_COUNTER], # 2002
    ["LINE", :LINE], # 2002
    ["LINE-COUNTER", -1], # 2002
    ["LINES", :LINES], # 2002
    ["LINKAGE", :LINKAGE], # 2002
    ["LOCAL-STORAGE", :LOCAL_STORAGE], # 2002
    ["LOCALE", :LOCALE], # 2002
    ["LOCK", :LOCK], # 2002
    ["LOW-VALUE", :LOW_VALUE], # 2002
    ["LOW-VALUES", :LOW_VALUE], # 2002
    ["LOWLIGHT", :LOWLIGHT], # 2002 (C/S)
    ["MANUAL", :MANUAL], # 2002 (C/S)
    ["MEMORY", :MEMORY], # 85
    ["MERGE", :MERGE], # 2002
    ["MESSAGE", -1], # 2002
    ["METHOD", -1], # 2002
    ["METHOD-ID", -1], # 2002
    ["MINUS", :MINUS], # 2002
    ["MODE", :MODE], # 2002
    ["MOVE", :MOVE], # 2002
    ["MULTIPLE", :MULTIPLE], # 2002 (C/S)
    ["MULTIPLY", :MULTIPLY], # 2002
    ["NATIONAL", :NATIONAL], # 2002
    ["NATIONAL-EDITED", :NATIONAL_EDITED], # 2002
    ["NATIVE", :NATIVE], # 2002
    ["NEGATIVE", :NEGATIVE], # 2002
    ["NESTED", -1], # 2002
    ["NEXT", :NEXT], # 2002
    ["NO", :NO], # 2002
    ["NONE", -1], # 2002 (C/S)
    ["NORMAL", -1], # 2002 (C/S)
    ["NOT", :NOT], # 2002
    ["NULL", :TOK_NULL], # 2002
    ["NULLS", :TOK_NULL], # extension
    ["NUMBER", :NUMBER], # 2002
    ["NUMBERS", :NUMBER], # 2002 (C/S)
    ["NUMERIC", :NUMERIC], # 2002
    ["NUMERIC-EDITED", :NUMERIC_EDITED], # 2002
    ["OBJECT", -1], # 2002
    ["OBJECT-COMPUTER", :OBJECT_COMPUTER], # 2002
    ["OBJECT-REFERENCE", -1], # 2002
    ["OCCURS", :OCCURS], # 2002
    ["OF", :OF], # 2002
    ["OFF", :OFF], # 2002
    ["OMITTED", :OMITTED], # 2002
    ["ON", :ON], # 2002
    ["ONLY", :ONLY], # 2002 (C/S)
    ["OPEN", :OPEN], # 2002
    ["OPTIONAL", :OPTIONAL], # 2002
    ["OPTIONS", -1], # 2002
    ["OR", :OR], # 2002
    ["ORDER", :ORDER], # 2002
    ["ORGANIZATION", :ORGANIZATION], # 2002
    ["OTHER", :OTHER], # 2002
    ["OUTPUT", :OUTPUT], # 2002
    ["OVERFLOW", :OVERFLOW], # 2002
    ["OVERLINE", :OVERLINE], # extension
    ["OVERRIDE", -1], # 2002
    ["PACKED-DECIMAL", :PACKED_DECIMAL], # 2002
    ["PADDING", :PADDING], # 2002
    ["PAGE", :PAGE], # 2002
    ["PAGE-COUNTER", -1], # 2002
    ["PARAGRAPH", :PARAGRAPH], # 2002 (C/S)
    ["PERFORM", :PERFORM], # 2002
    ["PF", :PAGE_FOOTING], # 2002
    ["PH", :PAGE_HEADING], # 2002
    ["PIC", nil], # 2002
    ["PICTURE", nil], # 2002
    ["PLUS", :PLUS], # 2002
    ["POINTER", :POINTER], # 2002
    ["POSITION", :POSITION], # 85
    ["POSITIVE", :POSITIVE], # 2002
    ["PRESENT", :PRESENT], # 2002
    ["PREVIOUS", :PREVIOUS], # 2002 (C/S)
    ["PRINTER", :PRINTER], # extension
    ["PRINTING", :PRINTING], # 2002
    ["PROCEDURE", :PROCEDURE], # 2002
    ["PROCEDURE-POINTER", :PROGRAM_POINTER], # extension
    ["PROCEDURES", :PROCEDURES], # extension
    ["PROCEED", :PROCEED], # 85
    ["PROGRAM", :PROGRAM], # 2002
    ["PROGRAM-ID", :PROGRAM_ID], # 2002
    ["PROGRAM-POINTER", :PROGRAM_POINTER], # 2002
    ["PROMPT", :PROMPT], # extension
    ["PROPERTY", -1], # 2002
    ["PROTOTYPE", -1], # 2002
    ["PURGE", -1], # 2002
    ["QUEUE", -1], # 2002
    ["QUOTE", :QUOTE], # 2002
    ["QUOTES", :QUOTE], # 2002
    ["RAISE", -1], # 2002
    ["RAISING", -1], # 2002
    ["RANDOM", :RANDOM], # 2002
    ["RD", :RD], # 2002
    ["READ", :READ], # 2002
    ["RECEIVE", -1], # 2002
    ["RECORD", :RECORD], # 2002
    ["RECORDING", :RECORDING], # extension
    ["RECORDS", :RECORDS], # 2002
    ["RECURSIVE", :RECURSIVE], # 2002 (C/S)
    ["REDEFINES", :REDEFINES], # 2002
    ["REEL", :REEL], # 2002
    ["REFERENCE", :REFERENCE], # 2002
    ["RELATION", -1], # 2002 (C/S)
    ["RELATIVE", :RELATIVE], # 2002
    ["RELEASE", :RELEASE], # 2002
    ["REMAINDER", :REMAINDER], # 2002
    ["REMOVAL", :REMOVAL], # 2002
    ["RENAMES", :RENAMES], # 2002
    ["REPLACE", -1], # 2002
    ["REPLACING", :REPLACING], # 2002
    ["REPORT", :REPORT], # 2002
    ["REPORTING", :REPORTING], # 2002
    ["REPORTS", :REPORTS], # 2002
    ["REPOSITORY", :REPOSITORY], # 2002
    ["REPRESENTS-NOT-A-NUMBER", -1], # 2008
    ["REQUIRED", :REQUIRED], # 2002 (C/S)
    ["RESERVE", :RESERVE], # 2002
    ["RESET", -1], # 2002
    ["RESUME", -1], # 2002
    ["RETRY", -1], # 2002
    ["RETURN", :RETURN], # 2002
    ["RETURNING", :RETURNING], # 2002
    ["REVERSE-VIDEO", :REVERSE_VIDEO], # 2002 (C/S)
    ["REWIND", :REWIND], # 2002
    ["REWRITE", :REWRITE], # 2002
    ["RF", :REPORT_FOOTING], # 2002
    ["RH", :REPORT_HEADING], # 2002
    ["RIGHT", :RIGHT], # 2002
    ["ROLLBACK", :ROLLBACK], # extension
    ["ROUNDED", :ROUNDED], # 2002
    ["RUN", :RUN], # 2002
    ["SAME", :SAME], # 2002
    ["SCREEN", :SCREEN], # 2002
    ["SCROLL", :SCROLL], # extension
    ["SD", :SD], # 2002
    ["SEARCH", :SEARCH], # 2002
    ["SECONDS", -1], # 2002 (C/S)
    ["SECTION", :SECTION], # 2002
    ["SECURE", :SECURE], # 2002 (C/S)
    ["SEGMENT", -1], # 2002
    ["SEGMENT-LIMIT", :SEGMENT_LIMIT], # 85
    ["SELECT", :SELECT], # 2002
    ["SELF", -1], # 2002
    ["SEND", -1], # 2002
    ["SENTENCE", :SENTENCE], # 2002
    ["SEPARATE", :SEPARATE], # 2002
    ["SEQUENCE", :SEQUENCE], # 2002
    ["SEQUENTIAL", :SEQUENTIAL], # 2002
    ["SET", :SET], # 2002
    ["SHARING", :SHARING], # 2002
    ["SIGN", :SIGN], # 2002
    ["SIGNED", :SIGNED], # 2002 (C/S)
    ["SIGNED-INT", :SIGNED_INT], # extension
    ["SIGNED-LONG", :SIGNED_LONG], # extension
    ["SIGNED-SHORT", :SIGNED_SHORT], # extension
    ["SIZE", :SIZE], # 2002
    ["SORT", :SORT], # 2002
    ["SORT-MERGE", :SORT_MERGE], # 2002
    ["SOURCE", :SOURCE], # 2002
    ["SOURCE-COMPUTER", :SOURCE_COMPUTER], # 2002
    ["SOURCES", -1], # 2002
    ["SPACE", :SPACE], # 2002
    ["SPACES", :SPACE], # 2002
    ["SPECIAL-NAMES", :SPECIAL_NAMES], # 2002
    ["STANDARD", :STANDARD], # 2002
    ["STANDARD-1", :STANDARD_1], # 2002
    ["STANDARD-2", :STANDARD_2], # 2002
    ["START", :START], # 2002
    ["STATEMENT", -1], # 2002 (C/S)
    ["STATUS", :STATUS], # 2002
    ["STEP", -1], # 2002 (C/S)
    ["STOP", :STOP], # 2002
    ["STRING", :STRING], # 2002
    ["STRONG", -1], # 2002 (C/S)
    ["SUB-QUEUE-1", -1], # 2002
    ["SUB-QUEUE-2", -1], # 2002
    ["SUB-QUEUE-3", -1], # 2002
    ["SUBTRACT", :SUBTRACT], # 2002
    ["SUM", :SUM], # 2002
    ["SUPER", -1], # 2002
    ["SUPPRESS", :SUPPRESS], # 2002
    ["SYMBOL", -1], # 2002 (C/S)
    ["SYMBOLIC", :SYMBOLIC], # 2002
    ["SYNC", :SYNCHRONIZED], # 2002
    ["SYNCHRONIZED", :SYNCHRONIZED], # 2002
    ["SYSTEM-DEFAULT", -1], # 2002
    ["TABLE", -1], # 2002
    ["TALLYING", :TALLYING], # 2002
    ["TAPE", :TAPE], # 85
    ["TERMINAL", -1], # 2002
    ["TERMINATE", :TERMINATE], # 2002
    ["TEST", :TEST], # 2002
    ["TEXT", -1], # 2002
    ["THAN", :THAN], # 2002
    ["THEN", :THEN], # 2002
    ["THROUGH", :THRU], # 2002
    ["THRU", :THRU], # 2002
    ["TIME", :TIME], # 2002
    ["TIMES", :TIMES], # 2002
    ["TO", :TO], # 2002
    ["TOP", :TOP], # 2002
    ["TRAILING", :TRAILING], # 2002
    ["TRANSFORM", :TRANSFORM], # OSVS
    ["TRUE", :TOK_TRUE], # 2002
    ["TYPE", :TYPE], # 2002
    ["TYPEDEF", -1], # 2002
    ["UCS-4", -1], # 2002 (C/S)
    ["UNDERLINE", :UNDERLINE], # 2002 (C/S)
    ["UNIT", :UNIT], # 2002
    ["UNIVERSAL", -1], # 2002
    ["UNLOCK", :UNLOCK], # 2002
    ["UNSIGNED", :UNSIGNED], # 2002 (C/S)
    ["UNSIGNED-INT", :UNSIGNED_INT], # extension
    ["UNSIGNED-LONG", :UNSIGNED_LONG], # extension
    ["UNSIGNED-SHORT", :UNSIGNED_SHORT], # extension
    ["UNSTRING", :UNSTRING], # 2002
    ["UNTIL", :UNTIL], # 2002
    ["UP", :UP], # 2002
    ["UPDATE", :UPDATE], # extension
    ["UPON", :UPON], # 2002
    ["USAGE", :USAGE], # 2002
    ["USE", :USE], # 2002
    ["USER-DEFAULT", -1], # 2002
    ["USING", :USING], # 2002
    ["UTF-16", -1], # 2002 (C/S)
    ["UTF-8", -1], # 2002 (C/S)
    ["VAL-STATUS", -1], # 2002
    ["VALID", -1], # 2002
    ["VALIDATE", -1], # 2002
    ["VALIDATE-STATUS", -1], # 2002
    ["VALUE", :VALUE], # 2002
    ["VALUES", :VALUE], # 2002
    ["VARYING", :VARYING], # 2002
    ["WAIT", :WAIT], # extension
    ["WHEN", :WHEN], # 2002
    ["WITH", :WITH], # 2002
    ["WORDS", :WORDS], # 85
    ["WORKING-STORAGE", :WORKING_STORAGE], # 2002
    ["WRITE", :WRITE], # 2002
    ["YYYYDDD", :YYYYDDD], # 2002 (C/S)
    ["YYYYMMDD", :YYYYMMDD], # 2002 (C/S)
    ["ZERO", :ZERO], # 2002
    ["ZEROES", :ZERO], # 2002
    ["ZEROS", :ZERO], # 2002
  ]
  RESERVED_WORDS = reserved_words.to_h { |x|
    [
      x[0].upcase,
      OpenStruct.new(
        name: x[0],
        token: x[1]
      ),
    ]
  }

  def lookup_system_name(name)
    if SYSTEM_TABLE.key?(name.upcase)
      SYSTEM_TABLE[name].node
    else
      @cb.error_node
    end
  end

  def lookup_reserved_word(name)
    p = RESERVED_WORDS[name.upcase]
    return nil if !p

    return nil if norestab.any? { |x| name.casecmp?(x.noresword) }

    return p.token if p.token != -1

    error("'%s' reserved word, but not supported yet", name)

    nil
  end

  def lookup_intrinsic(name, checkres)
    return nil if checkres && norestab.any? { |x| name.casecmp?(x.noresword) }

    cbp = FUNCTION_LIST[name.upcase]
    return cbp if cbp&.implemented

    nil
  end

  def list_reserved
    printf("Reserved Words (Parsed Y/N)\n\n")

    RESERVED_WORDS.each_value do |x|
      printf("%s%s(%s)\n", x.name, "\t" * (4 - x.name.length / 8), x.token == -1 ? "N" : "Y")
    end
  end

  def list_intrinsics
    printf("Intrinsic Function (Implemented Y/N)\n\n")

    FUNCTION_LIST.each_value do |x|
      printf("%s%s(%s)\n", x.name, "\t" * (4 - x.name.length / 8), x.implemented ? "Y" : "N")
    end
  end

  def list_mnemonics
    printf("Mnemonic names\n\n")

    SYSTEM_TABLE.each_value do |x|
      printf("%s\n", x.name)
    end
  end

  def init_reserved
    # build system-name table
    SYSTEM_TABLE.each_value do |x|
      x.node = @cb.build_system_name(x.system_category, x.token)
    end
  end
end
