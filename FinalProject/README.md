# Data Science in Practice Final Project

### Background
1. Emergency department congestion is representative of a dysfunctional system
2. There are many reasons, like the abusing of emergency medicine resource or dysfunctional information system
3. KAMERA developed a trans-hospital emergency department congestion real time  surveillance system. KAMERA provide some historical data from this system, and they hope they can move from surveillance to prediction.
4. Based on: [KAMERA-Emergency-Medicine-Challenge](http://dc.dsp.im/main/content/KAMERA-Emergency-Medicine-Challenge) hold by [DSP](http://dsp.im/)

### Goal
1. Predict total of all triage level (total)
2. Predict emergency department congestion light (light)
3. Predict number of appointment of EM (A12)
4. Predict number of waiting bed of EM (A15)

### Data
1. Part of KAMERA’s partners’ emergency
2. department’s operating data
3. Data Time: 2013/01/01 ~ 2013/12/31
4. Number of Hospitals : 11
5. Records: 21364 records
6. Fields: 37 fields

### Using libraries
1. [e1070](https://cran.r-project.org/web/packages/e1071/index.html): [libSVM](https://www.csie.ntu.edu.tw/~cjlin/libsvm/) is a library for Support Vector Machine developed by Chih-Chung Chang and Chih-Jen Lin of NTU
2. [dummies](https://cran.r-project.org/web/packages/dummies/dummies.pdf): dummies is a library to create dummy/indicator variables flexibly and efficiently.
