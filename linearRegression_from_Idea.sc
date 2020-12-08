import breeze.linalg._                                                           //import breeze.linalg._
import breeze.numerics._                                                         //import breeze.numerics._
import breeze.stats.regression.{lasso, leastSquares}                             //import breeze.stats.regression.{lasso, leastSquares}
import com.github.tototoshi.csv._                                                //import com.github.tototoshi.csv._
import java.io.File                                                              //import java.io.File
import scala.collection.mutable.ListBuffer                                       //import scala.collection.mutable.ListBuffer


// Загрузка данных
val dataFilePath = "C:\\Users\\ADM\\IdeaProjects\\scala_fs\\data\\Dataset.csv"   //val dataFilePath: String = C:\Users\ADM\IdeaProjects\scala_fs\data\Dataset.csv
val reader = CSVReader.open(new File(dataFilePath))                              //val reader: com.github.tototoshi.csv.CSVReader = com.github.tototoshi.csv.CSVReader@52592348
val readerAll = reader.all()  // прочитать csv-файл                              //val readerAll: List[List[String]] = List(List(likes, Checkins, Returns, Category, commBase, comm24, comm48, comm24_1, diff2448, baseTime, length, shares, hrs, sun_pub, mon_pub, tue_pub, wed_pub, thu_pub, fri_pub, sat_pub, sun_base, mon_base, tue_base, wed_base, thu_base, fri_base, sat_base, output), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 65, 166, 2, 24, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 10, 132, 1, 24, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 14, 133, 2, 24, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0), List(634995, 0, 463, 1, 7, 0, 3, 7, -3, 62, 131, 1, 24, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(634995, 0, 463, 1, 1, 0, 0, 1, 0, 58, 142, 5, 24, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,...
val readerWoDubl = readerAll.distinct  // удалить строки-дубли                   //val readerWoDubl: List[List[String]] = List(List(likes, Checkins, Returns, Category, commBase, comm24, comm48, comm24_1, diff2448, baseTime, length, shares, hrs, sun_pub, mon_pub, tue_pub, wed_pub, thu_pub, fri_pub, sat_pub, sun_base, mon_base, tue_base, wed_base, thu_base, fri_base, sat_base, output), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 65, 166, 2, 24, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 10, 132, 1, 24, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), List(634995, 0, 463, 1, 0, 0, 0, 0, 0, 14, 133, 2, 24, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0), List(634995, 0, 463, 1, 7, 0, 3, 7, -3, 62, 131, 1, 24, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0), List(634995, 0, 463, 1, 1, 0, 0, 1, 0, 58, 142, 5, 24, 0, "", 0, 0, 0, 0, 0, 0, 0, 0, 1,...


def matrixShape(m: DenseMatrix[Double]): (Int, Int) = {                          //def matrixShape(m: breeze.linalg.DenseMatrix[Double]): (Int, Int)
  // Возвращает форму массива
  (m.rows, m.cols)
}

def row2denseVector(row: List[String]): DenseVector[Double] = {                  //def row2denseVector(row: List[String]): breeze.linalg.DenseVector[Double]
  // Забивает пустые строки Double.NaN и
  // преобразует строку из csv-файла в полносвязный вектор
  val arr = row.map(
    elem => if (elem == "") Double.NaN.toString // обязательно Double.NaN.toString!!!
    else elem
  ).map(_.toDouble).toArray
  DenseVector(arr)
}

def denseMatrixWoNanRows(input: DenseMatrix[Double]): DenseMatrix[Double] = {    //def denseMatrixWoNanRows(input: breeze.linalg.DenseMatrix[Double]): breeze.linalg.DenseMatrix[Double]
  // Удаляет из полносвязной матрицы строки, в которых есть хотя бы один NaN
  val lstDenseVectorsWoNaN = ListBuffer[DenseVector[Double]]()
  for (idx <- 0 until input.rows) {
    val densVecRow = input(idx, ::).t
    val sumRow = densVecRow.map(_.isNaN).toArray.collect{ case true => 1; case false => 0}.sum
    if (sumRow == 0) {
      lstDenseVectorsWoNaN += densVecRow
    }
  }
  DenseMatrix(lstDenseVectorsWoNaN.toList: _*)
}

def nanColCounts(m: DenseMatrix[Double], colNames: DenseVector[String]): DenseMatrix[String] = { //def nanColCounts(m: breeze.linalg.DenseMatrix[Double], colNames: breeze.linalg.DenseVector[String]): breeze.linalg.DenseMatrix[String]
  // Вычисляет число NaN в каждом столбце
  val lstNanColsCounts = ListBuffer[Int]()
  val indexes = DenseVector(0 until m.cols: _*).map(_.toString)
  for (idx <- 0 until m.cols) {
    val densVecCol = m(::, idx)
    val sumCol = densVecCol.map(_.isNaN).toArray.collect {
      case true => 1; case false => 0
    }.sum
    lstNanColsCounts += sumCol
  }
  DenseMatrix(
    indexes,
    colNames,
    DenseVector(lstNanColsCounts.toArray.map(_.toString))
  ).t
}

def readCSV2DenseMatrix(input: List[List[String]], skip: Int = 0): DenseMatrix[Double] = { //def readCSV2DenseMatrix(input: List[List[String]], skip: Int): breeze.linalg.DenseMatrix[Double]
  // читает csv-файл в полносвязную вещественную матрицу
  val lstDenseVectors = ListBuffer[DenseVector[Double]]()
  for (idx <- skip until input.size) {
    lstDenseVectors += row2denseVector(input(idx))
  }
  DenseMatrix( // DenseMatrix строится на базе объектов НЕИЗМЕНЯЕМОГО типа данных!
    lstDenseVectors.toList: _* // распаковать список полносвязных векторов
  )
}

def removeColumns(input: DenseMatrix[Double], lstIdxRemoveCols: List[Int]): DenseMatrix[Double] = { //def removeColumns(input: breeze.linalg.DenseMatrix[Double], lstIdxRemoveCols: List[Int]): breeze.linalg.DenseMatrix[Double]
  val restCols = Set(0 until input.cols: _*).diff(lstIdxRemoveCols.toSet).toList
  input(::, restCols).toDenseMatrix
}

def trainTestSplit(input: DenseMatrix[Double],                                   //def trainTestSplit(input: breeze.linalg.DenseMatrix[Double], testSize: Double): (breeze.linalg.DenseMatrix[Double], breeze.linalg.DenseMatrix[Double], breeze.linalg.DenseVector[Double], breeze.linalg.DenseVector[Double])
                   testSize: Double = 0.20): (DenseMatrix[Double], DenseMatrix[Double],
                                              DenseVector[Double], DenseVector[Double]) = {
  val shuffleIdx = scala.util.Random.shuffle(List(0 until input.rows: _*))
  val inputShaked = input(shuffleIdx, ::).toDenseMatrix  // перемешанный набор данных
  val nRowsTrain = ((1 - testSize)*input.rows).toInt
  val xtrain = inputShaked(0 until nRowsTrain, 0 until inputShaked.cols).toDenseMatrix
  val xtest = inputShaked(nRowsTrain to -1, 0 until inputShaked.cols).toDenseMatrix
  val ytrain = inputShaked(0 until nRowsTrain, -1).toDenseVector
  val ytest = inputShaked(nRowsTrain to -1, -1).toDenseVector
  (xtrain, xtest, ytrain, ytest)
}

def rootMeanSquaredError(ytrue: DenseVector[Double], ypred: DenseVector[Double]): Double = { //def rootMeanSquaredError(ytrue: breeze.linalg.DenseVector[Double], ypred: breeze.linalg.DenseVector[Double]): Double
  sqrt(sum(pow(ytrue - ypred, 2))/ytrue.size)
}

val data = readCSV2DenseMatrix(readerWoDubl, skip=1)                             //val data: breeze.linalg.DenseMatrix[Double] =
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   0.0   0.0   0.0    65.0  ... (28 total)
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   0.0   0.0   0.0    10.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   0.0   0.0   0.0    14.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  7.0   0.0   3.0   7.0   -3.0   62.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  1.0   0.0   0.0   1.0   0.0    58.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   NaN   0.0   0.0    60.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   NaN   0.0   0.0    68.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  1.0   0.0   1.0   1.0   -1.0   32.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   NaN   0.0   0.0    35.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   NaN   0.0   0.0    48.0  ...
                                                                                 //634995.0  0.0  463.0  1.0  0.0   0.0   NaN   0.0 ...
val colNames = DenseVector(readerWoDubl(0).toArray) // имена столбцов            //val colNames: breeze.linalg.DenseVector[String] = DenseVector(likes, Checkins, Returns, Category, commBase, comm24, comm48, comm24_1, diff2448, baseTime, length, shares, hrs, sun_pub, mon_pub, tue_pub, wed_pub, thu_pub, fri_pub, sat_pub, sun_base, mon_base, tue_base, wed_base, thu_base, fri_base, sat_base, output)

val (rowsBeforeRemoveNan, colsBeforeRemoveNan) = matrixShape(data)               //val rowsBeforeRemoveNan: Int = 40941
                                                                                 //val colsBeforeRemoveNan: Int = 28
println(s"--> Shape of DenseMatrix (before remove NaN): (${rowsBeforeRemoveNan}, ${colsBeforeRemoveNan})") //--> Shape of DenseMatrix (before remove NaN): (40941, 28)

nanColCounts(data, colNames)                                                     //val res1: breeze.linalg.DenseMatrix[String] =
                                                                                 //0   likes     0
                                                                                 //1   Checkins  0
                                                                                 //2   Returns   51
                                                                                 //3   Category  57
                                                                                 //4   commBase  60
                                                                                 //5   comm24    0
                                                                                 //6   comm48    48
                                                                                 //7   comm24_1  0
                                                                                 //8   diff2448  0
                                                                                 //9   baseTime  0
                                                                                 //10  length    0
                                                                                 //11  shares    2448
                                                                                 //12  hrs       0
                                                                                 //13  sun_pub   0
                                                                                 //14  mon_pub   1927
                                                                                 //15  tue_pub   0
                                                                                 //16  wed_pub   0
                                                                                 //17  thu_pub   3045
                                                                                 //18  fri_pub   0
                                                                                 //19  sat_pub   0
                                                                                 //20  sun_base  0
                                                                                 //21  mon_base  1969
                                                                                 //22  tue_base  0
                                                                                 //23  wed_base  0
                                                                                 //24  thu_base  0
                                                                                 //25  fri_base  0
                                                                                 //26  sat_base  0
                                                                                 //27  output    0

// удалить следующие столбцы: shares, mon_pub, thu_pub, mon_base
val lstIdxRemoveCols = List(11, 14, 17, 21)                                      //val lstIdxRemoveCols: List[Int] = List(11, 14, 17, 21)
val dataWoNanCols = removeColumns(data, lstIdxRemoveCols)                        //val dataWoNanCols: breeze.linalg.DenseMatrix[Double] =
                                                                                 //634995.0  0.0   166.0  0.0  0.0  0.0   65.0  0.0  463.0  24.0  ... (24 total)
                                                                                 //634995.0  0.0   132.0  0.0  0.0  0.0   10.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   133.0  0.0  0.0  0.0   14.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   131.0  0.0  0.0  3.0   62.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   142.0  0.0  0.0  0.0   58.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   166.0  0.0  0.0  NaN   60.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   145.0  0.0  0.0  NaN   68.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   157.0  0.0  0.0  1.0   32.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   177.0  0.0  0.0  NaN   35.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   126.0  1.0  0.0  NaN   48.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   188.0  1.0  0.0  NaN   52.0  0.0  ...
println(matrixShape(dataWoNanCols))                                              //(40941,24)
val dataWoNanRows = denseMatrixWoNanRows(dataWoNanCols)                          //val dataWoNanRows: breeze.linalg.DenseMatrix[Double] =
                                                                                 //634995.0  0.0   166.0  0.0  0.0  0.0   65.0  0.0  463.0  24.0  ... (24 total)
                                                                                 //634995.0  0.0   132.0  0.0  0.0  0.0   10.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   133.0  0.0  0.0  0.0   14.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   131.0  0.0  0.0  3.0   62.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   142.0  0.0  0.0  0.0   58.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   157.0  0.0  0.0  1.0   32.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  1.0   126.0  0.0  0.0  0.0   37.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   103.0  0.0  0.0  0.0   23.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   158.0  0.0  0.0  3.0   40.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   151.0  0.0  0.0  3.0   54.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   133.0  0.0  0.0  0.0   29.0  0.0  ...

val (rowsAfterRemoveNan, colsAfterRemoveNan) = matrixShape(dataWoNanRows)        //val rowsAfterRemoveNan: Int = 40726
                                                                                 //val colsAfterRemoveNan: Int = 24
println(s"--> Shape of DenseMatrix (after remove NaN): (${rowsAfterRemoveNan}, ${colsAfterRemoveNan})") //--> Shape of DenseMatrix (after remove NaN): (40726, 24)

//nanColCounts(dataWoNanRows, colNames)

// Подготовленный набор данных
val x = dataWoNanRows(::, 0 until dataWoNanRows.cols-1)                          //val x: breeze.linalg.DenseMatrix[Double] =
                                                                                 //634995.0  0.0   166.0  0.0  0.0  0.0   65.0  0.0  463.0  24.0  ... (23 total)
                                                                                 //634995.0  0.0   132.0  0.0  0.0  0.0   10.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   133.0  0.0  0.0  0.0   14.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   131.0  0.0  0.0  3.0   62.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   142.0  0.0  0.0  0.0   58.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   157.0  0.0  0.0  1.0   32.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  1.0   126.0  0.0  0.0  0.0   37.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   103.0  0.0  0.0  0.0   23.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   158.0  0.0  0.0  3.0   40.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   151.0  0.0  0.0  3.0   54.0  0.0  463.0  24.0  ...
                                                                                 //634995.0  0.0   133.0  0.0  0.0  0.0   29.0  0.0  463.0  24.0 ...

val (                                                                            //val xtrain: breeze.linalg.DenseMatrix[Double] =
  xtrain: DenseMatrix[Double],                                                   //1181430.0  2.0    85.0    0.0  0.0       0.0    21.0  0.0  ... (23 total)
  xtest: DenseMatrix[Double],                                                    //1128745.0  1.0    111.0   0.0  0.0       0.0    21.0  1.0  ...
  ytrain: DenseVector[Double],                                                   //2156275.0  16.0   72.0    0.0  114.0     143.0  42.0  1.0  ...
  ytest: DenseVector[Double]) = trainTestSplit(x, testSize = 0.15)               //17365.0    0.0    14.0    0.0  0.0       7.0    36.0  1.0  ...
                                                                                 //253349.0   0.0    29.0    0.0  0.0       0.0    60.0  0.0  ...
                                                                                 //6887.0     0.0    51.0    0.0  0.0       0.0    7.0   0.0  ...
                                                                                 //1009493.0  3.0    118.0   0.0  175714.0  27.0   68.0  0.0  ...
                                                                                 //70695.0    1.0    555.0   0.0  0.0       3.0    70.0  0.0  ...
                                                                                 //213787.0   13.0   57.0    0.0  0.0       0.0    15.0  0.0  ...
                                                                                 //867486.0   0.0    303.0   1.0  0.0       11.0   60.0  0.0  ...
                                                                                 //13221.0    0.0    392.0   0.0  0.0       0.0    2.0   0.0  ...
                                                                                 //4975857.0  106.0  111.0   0.0  15...

matrixShape(xtrain)                                                              //val res4: (Int, Int) = (34617,23)
matrixShape(xtest)                                                               //val res5: (Int, Int) = (6109,23)
ytrain.size                                                                      //val res6: Int = 34617
ytest.size                                                                       //val res7: Int = 6109

// Linear Regression
val lr = leastSquares(xtrain, ytrain)                                            //дек. 09, 2020 12:01:14 AM com.github.fommil.netlib.LAPACK <clinit>
                                                                                 //WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeSystemLAPACK
                                                                                 //дек. 09, 2020 12:01:14 AM com.github.fommil.jni.JniLoader liberalLoad
                                                                                 //INFO: successfully loaded C:\Users\ADM\AppData\Local\Temp\jniloader18324001010514755992netlib-native_ref-win-x86_64.dll
                                                                                 //val lr: breeze.stats.regression.LeastSquaresRegressionResult = <function1>
val ls = lasso(xtrain, ytrain, 0.05)                                             //val ls: breeze.stats.regression.LassoResult = <function1>

val lrcoefs = lr.coefficients // веса Linear Regression                          //val lrcoefs: breeze.linalg.DenseVector[Double] = DenseVector(1.2488277925092437E-24, 8.506193298607477E-6, 5.722591323598501E-20, -2.991281793248971E-15, 3.091037549953763E-22, -8.506193298607316E-6, 2.7114230266826855E-18, -1.7654736637527616E-15, -2.8076626032077137E-22, 1.1633758575476643E-16, 1.4663652770387144E-18, 1.1069244001901893E-18, 1.1622019877317934E-15, 5.146744144683396E-16, -8.506193298607699E-6, -3.373618794762849E-16, -1.2619170212399165E-18, -1.1521567121042748E-15, -2.6528211328022763E-15, -2.948008752807386E-15, -2.1334112469731315E-15, -1.0745258995614782E-18, 0.9999999999999919)
lrcoefs.foreach(                                                                 //1.2488277925092437E-24
  println                                                                        //8.506193298607477E-6
)                                                                                //5.722591323598501E-20
                                                                                 //-2.991281793248971E-15
                                                                                 //3.091037549953763E-22
                                                                                 //-8.506193298607316E-6
                                                                                 //2.7114230266826855E-18
                                                                                 //-1.7654736637527616E-15
                                                                                 //-2.8076626032077137E-22
                                                                                 //1.1633758575476643E-16
                                                                                 //1.4663652770387144E-18
                                                                                 //1.1069244001901893E-18
                                                                                 //1.1622019877317934E-15
                                                                                 //5.146744144683396E-16
                                                                                 //-8.506193298607699E-6
                                                                                 //-3.373618794762849E-16
                                                                                 //-1.2619170212399165E-18
                                                                                 //-1.1521567121042748E-15
                                                                                 //-2.6528211328022763E-15
                                                                                 //-2.948008752807386E-15
                                                                                 //-2.1334112469731315E-15
                                                                                 //-1.0745258995614782E-18
                                                                                 //0.9999999999999919

matrixShape(xtrain)                                                              //val res9: (Int, Int) = (34617,23)
lrcoefs.size                                                                     //val res10: Int = 23

val ypredTrain = xtrain*lrcoefs // прогноз на обучающем наборе данных            //дек. 09, 2020 12:01:15 AM com.github.fommil.netlib.BLAS <clinit>
                                                                                 //WARNING: Failed to load implementation from: com.github.fommil.netlib.NativeSystemBLAS
                                                                                 //дек. 09, 2020 12:01:15 AM com.github.fommil.jni.JniLoader load
                                                                                 //INFO: already loaded netlib-native_ref-win-x86_64.dll
                                                                                 //val ypredTrain: breeze.linalg.DenseVector[Double] = DenseVector(0.9999999999999959, 1.0854147976136985E-15, 1.215125743000273E-15, -9.65356760415952E-16, -1.0925696702057905E-15, 1.8221395702934659E-16, 0.9999999999999949, 0.9999999999999954, 1.6926924049192406E-15, -7.885919419622544E-18, 0.9999999999999944, 6.881999069588852E-16, -8.277016137360065E-16, 7.109383809667632E-16, -8.177044715416518E-16, -9.704931547782784E-16, 0.999999999999996, -9.05148698214556E-16, 7.91281703419908E-16, 2.933611631939485E-15, 1.1308300550122615E-15, 2.9719270024884044E-15, 1.1091701259896171E-15, 1.100334866046634E-15, -3.355771870550028E-16, -8.29251298042827E-16, 1.188723298325572E-15, -3.9167565217239916E-16, 6.955859454604356E-16, 4.158568195131971E-15, 0.9999999999999954, -5.522092581991032E-16, 1...
val ypredTest = xtest*lrcoefs // прогноз на тестовом наборе данных               //val ypredTest: breeze.linalg.DenseVector[Double] = DenseVector(4.1159788482377484E-16, 2.5858878370374434E-15, 2.6332475272590448E-15, -5.021735459789095E-16, 2.6014154982981E-15, 1.2442492132839678E-15, -9.26326768842156E-16, -1.932224782186408E-15, 1.7857102752324997E-15, -1.9614847170416726E-15, -4.593627588678387E-16, 1.0779942639747023E-15, 3.4125946584206883E-16, -3.5387778068554315E-16, 2.9504446162526557E-15, 3.2953638547709685E-16, 1.7301912136224301E-15, 1.0787832417218977E-15, 0.9999999999999943, 4.552719538825546E-17, 1.6942315864456162E-15, 3.3115458059537083E-15, 6.437006503190325E-16, 7.759715505387799E-16, 2.947674185576218E-15, 1.039785317378938E-15, 3.399723909619814E-16, 6.88457146477876E-16, -9.373795718768634E-17, 6.45755015709616E-16, 7.47724058992324E-16, 6.895382...

val rmseTrain = rootMeanSquaredError(ytrain, ypredTrain)                         //val rmseTrain: Double = 2.3694676777603796E-15
val rmseTest = rootMeanSquaredError(ytest, ypredTest)                            //val rmseTest: Double = 2.336072080500195E-15

// Решение с использованием псевдообратной матрицы
val coefsFromPinv = pinv(xtrain) * ytrain                                        //java.lang.OutOfMemoryError: Java heap space

val ypredPinvTrain = xtrain*coefsFromPinv // прогноз на обучающем наборе данных
val ypredPinvTest = xtest*coefsFromPinv // прогноз на тестовом наборе данных

val rmsePinvTrain = rootMeanSquaredError(ytrain, ypredPinvTrain)                 //                                                                                
val rmsePinvTest = rootMeanSquaredError(ytest, ypredPinvTest)                    //                                                                                
