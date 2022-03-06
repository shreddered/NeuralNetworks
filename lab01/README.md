# Лабораторная работа 1

За тренировку нейрона отвечает функция `train`, имеющая следующую сигнатуру:

```haskell
train :: [Double]           -- reference
      -> Double             -- learning rate
      -> ActivationFunction -- activation function
      -> ([Double], [Int])  -- (final weights, list of errors)
```

`ActivationFunction` - вспомогательный тип, в котором передаётся информация о функции активации (сама функция и её производная):
```haskell
data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    }
```

Все функции для выполнения расчётов определены в модуле [Data.Neuron](src/Data/Neuron.hs).

[Точка входа](app/Main.hs) содержит лишь обработку полученных результатов.
