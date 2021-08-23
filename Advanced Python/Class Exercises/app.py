from flask import Flask, request
from ie_utils import tokenize

app = Flask(__name__)

@app.route("/")
def home():
    return {
        "message": "Hello world!"
    }

@app.route("/tokenize")
def do_tokenize():
    print(request.args)
    sentence = request.args.get("sentence")
    # You need to correct the value to the correct type!
    lower = bool(request.args.get("lower", False))
    try:
        return str(tokenize(sentence, lower=lower))
    except ValueError:
        return "[]"

if __name__ == "__main__":
    import os
    port = int(os.environ["PORT"])
    app.run(host="0.0.0.0", port=port)