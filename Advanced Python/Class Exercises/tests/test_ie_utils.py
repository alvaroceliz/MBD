import pytest

from ie_utils import tokenize

@pytest.mark.parametrize("sentence, expected_tokens", [
    ("This is a sentence", ["This", "is", "a", "sentence"]),
    ("Another sentence", ["Another", "sentence"]),
])
def test_tokenize_returns_expected_tokens(sentence, expected_tokens):
    tokens = tokenize(sentence)

    assert tokens == expected_tokens


def test_tokenize_lower_returns_lowercase_tokens():
    sentence = "Hello World!"
    expected_tokens = ["hello", "world!"]

    tokens = tokenize(sentence, lower=True)

    assert tokens == expected_tokens
