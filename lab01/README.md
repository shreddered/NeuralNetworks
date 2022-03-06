# Лабораторная работа 1

За обучение нейрона отвечает функция `train`, имеющая следующую сигнатуру:

```haskell
train :: [Double]           -- reference
      -> Double             -- learning rate
      -> ActivationFunction -- activation function
      -> ([Double], [Int])  -- (final weights, list of errors)
```

`ActivationFunction` - вспомогательный тип, в котором передаётся информация о функции активации (сама функция, её производная и функция для получения реального выхода):
```haskell
data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    , out        :: Double -> Double
    }
```

Все функции для выполнения расчётов определены в модуле [Data.Neuron](src/Data/Neuron.hs).

[Точка входа](app/Main.hs) содержит лишь обработку полученных результатов.

В результате выполнения программы были получены следующие веса:

* Для пороговой функции активации: `[2.1,-2.1,-1.8,1.5,1.2]`

* Для логистической функции активации:
`[0.8453172751625206,-0.7987497124070773,-0.7993426281489464,0.5252500105051507,0.32780057680973007]`

Зависимость количества ошибок от номера эпохи:

* Для пороговой функции активации:

![threshold plot](images/plot1.png)

* Для логистической функции активации:

![logistic plot](images/plot2.png)
