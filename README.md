## Линейная регрессия с использованием библиотеки [Breeze](https://github.com/scalanlp/breeze) и Scala

Структура проекта:
* data/ -- директория с данными
* linearRegression_Scala2.11_final.ipynb -- основной файл, отвечающий на вопросы задания
* H2O_help_for_lineregression.ipynb -- вспомогательный блокнот для отыскания квазиоптимальной
прогностической модели на базе библиотеки H2O
* StackedEnsemble_AllModels_AutoML_20201207_001304 -- квазиоптимальная модель, найденная H2OAutoML
* Python_help_for_lineregression.ipynb -- вспомогательный блокнот, использующий стандартный DS-стек инструментов анализа
* Facebook_Comments_Line_Regression_wo_dupl.html -- отчет по анализу данных, подготовленный pandas_profiling
* config_minimal.yaml -- конфигурационный файл, управляющий настройками pandas_profiling
* make_full_report.sh -- bash-сценарий, автоматизирующий запуск генератора отчета в полном режиме
* make_minimal_report.sh -- bash-сценарий, автоматизирующий запуск генератора отчета в минимальном режиме
* reader_wo_dupl.csv -- csv-файл, подготовленный для pandas_profiling
