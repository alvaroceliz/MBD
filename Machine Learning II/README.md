# Machine Learning II

## Introduction

The goal of this assignment is to build a Machine Learning model using different models learned in class.

## Description

This assignment will deal with regression models, because we want you to build a model that predicts the day-ahead price of power in Spain given some forecast available before the daily auction.
The power market is like many markets in which the price is settled where demand meets supply.
- Supply: power generation plants (nuclear, solar, wind, etc). Each type of production is different and should be analyzed independently before making assumptions. Analyzing seasonality, distributions and correlations is advised.
- Demand: all the power consumption in the country, from huge factories to small house-hold consumers. Very seasonal and heavily affected by temperature.
The data provided is a timeseries form.

## Data Dictionary

The provided datasets contain the following timeseries with hourly granularity:
- date: date of the observation "%Y-%m-%d"
- hour: hour of the observation, [0 - 23]
- fc demand: forecast of demand in MWh
- fc nuclear: forecast of nuclear power production in MWh
- import FR: forecast of the importing capacity from France to Spain in MWh
- export FR: forecast of the exporting capacity from Spain to France in MWh
- fc wind: forecast of wind power production in MWh
- fc solar pv: forecast of PV solar (solar panels) power production in MWh
- fc solar th: forecast of thermal solar power production in MWh
- price: power price for each hour in €/MWh. This is the target to predict.
