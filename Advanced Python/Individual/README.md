# Bike sharing prediction model

## Usage

To install the library:

```
$ # pip install ie_bike_model  # If I ever upload this to PyPI, which I won't
$ pip install .
```

Basic usage:

```python
>>> from ie_bike_model.model import train_and_persist, predict
>>> train_and_persist()  # Trains the model and saves it to `model.joblib`
>>> predict(
...     dteday="2012-11-01",
...     hr=10,
...     weathersit="Clear, Few clouds, Partly cloudy, Partly cloudy"
...     temp=0.3,
...     atemp=0.31,
...     hum=0.8,
...     windspeed=0.0,
... )
105
```

## Development

To install a development version of the library:

```
$ flit install --symlink
```

To run the tests:

```
$ pytest
```
