import unittest
from logs import process_message

first_line_message = """{
    "timestamp": "2019-04-14T01:26:20.415686Z",
    "textPayload": "[514063]: [157411802-1] db=postgres,user=postgres DETAIL: parameters: $1 = '2'"
}"""

# Note: we know this isn't a first-line message because it doesn't start with
# [%p]: [%l-1] db=%d,user=%u
subsequent_line_message = """{
    "timestamp": "2019-04-14T01:26:20.415686Z",
    "textPayload": "(canvas_id, trace_id, module, path)"
}"""


class TimestampMungingOfStackdriverLogsTestCase(unittest.TestCase):

    def test_timestamp_is_prepended_to_first_line_in_group(self):
        result = process_message(first_line_message)

        # Timestamp here has been modified - the date and time are separated by
        # ' ' instead of 'T', and the time zone is ' UTC' instead of 'Z'
        self.assertTrue(
            result.startswith("[2019-04-14 01:26:20.415686 UTC]: ")
        )

    def test_timestamp_is_not_prepended_to_subsequent_lines(self):
        result = process_message(subsequent_line_message)
        # Result is the textPayload, unmodified
        self.assertEqual(
            result,
            "(canvas_id, trace_id, module, path)"
        )


if __name__ == "__main__":
    unittest.main()
