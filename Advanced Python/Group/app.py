# Installing the environment
from flask import Flask, request
import sklearn
import ie_bike_model
import argparse
import sys
from ie_bike_model.model import train_and_persist


# 1. Create `app.py` with simple Flask `/` endpoint returning dictionary with the versions of scikit-learn, ie-bike-model & Python
app = Flask(__name__)


@app.route("/")
def versions():
    versions_sklearn = sklearn.__version__
    versions_ie_bike_model = ie_bike_model.__version__
    versions_python = sys.version[0:5]
    return {
        "Sklearn": versions_sklearn,
        " IE bike model": versions_ie_bike_model,
        "Pyhton": versions_python,
    }


# 3. Add Flask `/train_and_persist` endpoint to call `train_and_persist` function from `ie-bike-model` & returns `{"status": "ok"}`
@app.route("/train_and_persist")
def do_train_and_persist():
    try:
        from ie_bike_model.model import train_and_persist

        result_metrics = train_and_persist()
        return {"MSE": result_metrics, "status": "ok"}
    except ValueError:
        return {"status": "Error"}


# 4. Add a Flask `/predict` endpoint to `app.py` that uses the URL parameters to call the `predict` function from `ie-bike-model` and returns `{"result": NN: "elapsed_fime": FF.FFF}` where `NN` is the expected number of cyclists and `FF.FFF` is the processing time, in seconds


# values of `weather_situation` must be `clear`, `cloudy`, `light_rain`, and `heavy_rain`.

weather_situation_dict = {
    "clear": "Clear, Few clouds, Partly cloudy, Partly cloudy",
    "light_rain": "Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds",
    "cloudy": "Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist",
    "heavy_rain": "Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog",
}


@app.route("/predict")
def do_predict():
    try:
        from ie_bike_model.model import predict
        import timeit

        date = request.args.get("date", "")
        hour = request.args.get("hour", "")
        hour = int(hour)
        weather_sit = request.args.get("weather_situation", "")
        temperature = request.args.get("temperature", "")
        temperature = float(temperature)
        feeling_temperature = request.args.get("feeling_temperature", "")
        humidity = request.args.get("humidity", "")
        humidity = float(humidity)

        windspeed = request.args.get("windspeed", "")
        windspeed = float(windspeed)

        weather_sit_dict = {
            "cloudy": "Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist",
            "clear": "Clear, Few clouds, Partly cloudy, Partly cloudy",
            "light_rain": "Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds",
            "heavy_rain": "Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog",
        }

        weather_situation = weather_sit_dict[weather_sit]

        timeBefore = timeit.default_timer()
        NN = predict(
            dteday=date,
            hr=hour,
            weathersit=weather_situation,
            temp=temperature,
            atemp=feeling_temperature,
            hum=humidity,
            windspeed=windspeed,
        )
        timeAfter = timeit.default_timer()
        time = timeAfter - timeBefore
        return {"result": NN, "elapsed_time": time}
    except ValueError:
        return {"status": "Error"}

    # return {"result": prediction, "elapsed_time": "{:.3f}".format(round(time, 3))}


# 2. Include the necessary code so that `python app.py NNNN` launches a basic Flask application on port NNNN and host 0.0.0.0
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Running the model")
    parser.add_argument("-p", "--port", help="Port_number")
    args = vars(parser.parse_args())
    print(args)
    app.run(host="0.0.0.0", debug=True, port=args["port"])


# to run on terminal: python app.py -p 5005
# flask run --port 63684
