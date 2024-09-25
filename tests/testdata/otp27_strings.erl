-module(otp27_strings).


-doc """
    First line
    Second line with "\*not emphasized\* Markdown"
    Third line
    """.
triple_quoted_strings() ->
    X = <<"""
        Line 1
        Line 2
        """/utf8>>,
    X = """"
        ++ foo() ++
        """",
    ok.


sigil_strings() ->
    ~"abc\d",
    ~'abc"d',
    ~b[abc"d],
    ~R/^from: /i,
    ~""""
     ++ foo() ++
     """",
    ok.
