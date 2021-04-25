# frozen_string_literal: true

require "test_helper"
require "tempfile"

module CobolParser
  class PPLexerTest < Test::Unit::TestCase
    def create_pp_lexer
      context = create_context
      options = create_common_options
      PPLexer.new(context, options)
    end

    sub_test_case "#each_token" do
      test "COPY" do
        tempfile = Tempfile.new
        tempfile.write <<-EOS
         COPY    "FOO.INC"       REPLACING   //FOO//
                                 BY          //BAR//.
        EOS
        tempfile.close(false)

         pp_lexer = create_pp_lexer
         pp_lexer.ppopen(tempfile.path)

         tokens = pp_lexer.each_token
         assert_token_equal(:COPY, "COPY", tokens.next)
         assert_token_equal(:TOKEN, '"FOO.INC"', tokens.next)
         assert_token_equal(:REPLACING, "REPLACING", tokens.next)
         assert_token_equal(:EQEQ, "//", tokens.next)
         assert_token_equal(:TOKEN, "FOO", tokens.next)
         assert_token_equal(:EQEQ, "//", tokens.next)
         assert_token_equal(:BY, "BY", tokens.next)
         assert_token_equal(:EQEQ, "//", tokens.next)
         assert_token_equal(:TOKEN, "BAR", tokens.next)
         assert_token_equal(:EQEQ, "//", tokens.next)
         assert_token_equal(".", ".", tokens.next)
         assert_raise(StopIteration) do
           tokens.next
         end

         lines = pp_lexer.output_file.string.lines
         assert_match(/^# 1 "[^"]+"\n$/, lines[0])
         assert_equal(" \n", lines[1])
      end

      test "multibyte" do
        tempfile = Tempfile.new
        tempfile.write <<-EOS
           MOVE    "あいうえおかきくけこさしすせそ"
        EOS
        tempfile.close(false)

        pp_lexer = create_pp_lexer
        pp_lexer.ppopen(tempfile.path)

        tokens = pp_lexer.each_token
        assert_raise(StopIteration) do
          tokens.next
        end

        lines = pp_lexer.output_file.string.lines
        assert_match(/^# 1 "[^"]+"\n$/, lines[0])
        assert_equal(" MOVE \"あいうえおかきくけこさしすせそ\" \n", lines[1])
      end
    end
  end
end
