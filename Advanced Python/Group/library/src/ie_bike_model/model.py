import os

import numpy as np
import pandas as pd
from sklearn.compose import (
    ColumnTransformer,
    make_column_transformer,
    make_column_selector,
)
from sklearn.impute import SimpleImputer, KNNImputer
from sklearn.pipeline import Pipeline, make_pipeline, FeatureUnion, make_union
from sklearn.preprocessing import OrdinalEncoder, FunctionTransformer
from sklearn.linear_model import LinearRegression, Lasso
from sklearn.ensemble import RandomForestRegressor
import joblib
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error

DATA_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_PATH = os.path.join(DATA_DIR, "hour.csv")
MODEL_DIR = os.path.join(os.path.expanduser("~"), ".ie_bike_model")
MODEL_PATH = os.path.join(MODEL_DIR, "model.joblib")


def ffill_missing(ser):
    return ser.fillna(method="ffill")


def is_weekend(df):
    return df["dteday"].dt.day_name().isin(["Saturday", "Sunday"]).to_frame()


from sklearn.model_selection import train_test_split


def train_and_persist():
    df = pd.read_csv(DATA_PATH, parse_dates=["dteday"])
    df_train = df.loc[df["dteday"] < "2012-10"]

    ffiller = FunctionTransformer(ffill_missing)
    weather_enc = make_pipeline(ffiller, OrdinalEncoder())
    ct = make_column_transformer(
        (ffiller, make_column_selector(dtype_include=np.number)),
        (weather_enc, ["weathersit"]),
    )
    preprocessing = FeatureUnion(
        [("is_weekend", FunctionTransformer(is_weekend)), ("column_transform", ct)]
    )

    reg = Pipeline(
        [("preprocessing", preprocessing), ("model", RandomForestRegressor())]
    )

    X_all_rows = df_train.drop(columns=["instant", "cnt", "casual", "registered"])
    y_all_rows = df_train["cnt"]

    X_train, X_test, y_train, y_test = train_test_split(
        X_all_rows, y_all_rows, test_size=0.33, random_state=42
    )

    reg.fit(X_train, y_train)

    y_pred = reg.predict(X_test)

    MSE = mean_squared_error(y_test, y_pred)

    if not os.path.isdir(MODEL_DIR):
        os.mkdir(MODEL_DIR)
    joblib.dump(reg, MODEL_PATH)

    return MSE


def predict(dteday, hr, weathersit, temp, atemp, hum, windspeed):
    reg = joblib.load(MODEL_PATH)

    X_input = pd.DataFrame(
        [
            {
                "dteday": pd.to_datetime(dteday),
                "hr": hr,
                "weathersit": weathersit,
                "temp": temp,
                "atemp": atemp,
                "hum": hum,
                "windspeed": windspeed,
            }
        ]
    )

    y_pred = reg.predict(X_input)
    assert len(y_pred) == 1

    return y_pred[0]


if __name__ == "__main__":
    print(
        predict(
            dteday="2012-11-10",
            hr4=10,
            weathersit="Clear, Few clouds, Partly cloudy, Partly cloudy",
            temp=0.3,
            atemp=0.31,
            hum=0.8,
            windspeed=0.0,
        )
    )
