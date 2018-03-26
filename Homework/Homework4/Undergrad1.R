trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

############################
############################  Chopping up the data
############################

trainAdjChop <- trainData[1:4]
trainSub <- trainData[-1]

trainAdjChop[2] <- rowMeans(trainSub[, c(0:399)*3 + 1])
trainAdjChop[3] <- rowMeans(trainSub[, c(0:399)*3 + 2])
trainAdjChop[4] <- rowMeans(trainSub[, c(0:399)*3 + 3])
colnames(trainAdjChop)[2:4] <- c("wholeR", "wholeG", "wholeB")

trainAdjChop[5] <- rowMeans(trainSub[, c(0:199)*3 + 1])
trainAdjChop[6] <- rowMeans(trainSub[, c(0:199)*3 + 2])
trainAdjChop[7] <- rowMeans(trainSub[, c(0:199)*3 + 3])
colnames(trainAdjChop)[5:7] <- c("tophfR", "tophfG", "tophfB")

trainAdjChop[8] <- rowMeans(trainSub[, c(200:399)*3 + 1])
trainAdjChop[9] <- rowMeans(trainSub[, c(200:399)*3 + 2])
trainAdjChop[10] <- rowMeans(trainSub[, c(200:399)*3 + 3])
colnames(trainAdjChop)[8:10] <- c("btmhfR", "btmhfG", "btmhfB")

trainAdjChop[11] <- (rowMeans(trainSub[, c(0:9)*3 + 1]) +
                       rowMeans(trainSub[, c(20:29)*3 + 1]) +
                       rowMeans(trainSub[, c(40:49)*3 + 1]) +
                       rowMeans(trainSub[, c(60:69)*3 + 1]) +
                       rowMeans(trainSub[, c(80:89)*3 + 1]) +
                       rowMeans(trainSub[, c(100:109)*3 + 1]) +
                       rowMeans(trainSub[, c(120:129)*3 + 1]) +
                       rowMeans(trainSub[, c(140:149)*3 + 1]) +
                       rowMeans(trainSub[, c(160:169)*3 + 1]) +
                       rowMeans(trainSub[, c(180:189)*3 + 1]) +
                       rowMeans(trainSub[, c(200:209)*3 + 1]) +
                       rowMeans(trainSub[, c(220:229)*3 + 1]) +
                       rowMeans(trainSub[, c(240:249)*3 + 1]) +
                       rowMeans(trainSub[, c(260:269)*3 + 1]) +
                       rowMeans(trainSub[, c(280:289)*3 + 1]) +
                       rowMeans(trainSub[, c(300:309)*3 + 1]) +
                       rowMeans(trainSub[, c(320:329)*3 + 1]) +
                       rowMeans(trainSub[, c(340:349)*3 + 1]) +
                       rowMeans(trainSub[, c(360:369)*3 + 1]) +
                       rowMeans(trainSub[, c(380:389)*3 + 1]))/20
trainAdjChop[12] <- (rowMeans(trainSub[, c(0:9)*3 + 2]) +
                       rowMeans(trainSub[, c(20:29)*3 + 2]) +
                       rowMeans(trainSub[, c(40:49)*3 + 2]) +
                       rowMeans(trainSub[, c(60:69)*3 + 2]) +
                       rowMeans(trainSub[, c(80:89)*3 + 2]) +
                       rowMeans(trainSub[, c(100:109)*3 + 2]) +
                       rowMeans(trainSub[, c(120:129)*3 + 2]) +
                       rowMeans(trainSub[, c(140:149)*3 + 2]) +
                       rowMeans(trainSub[, c(160:169)*3 + 2]) +
                       rowMeans(trainSub[, c(180:189)*3 + 2]) +
                       rowMeans(trainSub[, c(200:209)*3 + 2]) +
                       rowMeans(trainSub[, c(220:229)*3 + 2]) +
                       rowMeans(trainSub[, c(240:249)*3 + 2]) +
                       rowMeans(trainSub[, c(260:269)*3 + 2]) +
                       rowMeans(trainSub[, c(280:289)*3 + 2]) +
                       rowMeans(trainSub[, c(300:309)*3 + 2]) +
                       rowMeans(trainSub[, c(320:329)*3 + 2]) +
                       rowMeans(trainSub[, c(340:349)*3 + 2]) +
                       rowMeans(trainSub[, c(360:369)*3 + 2]) +
                       rowMeans(trainSub[, c(380:389)*3 + 2]))/20
trainAdjChop[13] <- (rowMeans(trainSub[, c(0:9)*3 + 3]) +
                       rowMeans(trainSub[, c(20:29)*3 + 3]) +
                       rowMeans(trainSub[, c(40:49)*3 + 3]) +
                       rowMeans(trainSub[, c(60:69)*3 + 3]) +
                       rowMeans(trainSub[, c(80:89)*3 + 3]) +
                       rowMeans(trainSub[, c(100:109)*3 + 3]) +
                       rowMeans(trainSub[, c(120:129)*3 + 3]) +
                       rowMeans(trainSub[, c(140:149)*3 + 3]) +
                       rowMeans(trainSub[, c(160:169)*3 + 3]) +
                       rowMeans(trainSub[, c(180:189)*3 + 3]) +
                       rowMeans(trainSub[, c(200:209)*3 + 3]) +
                       rowMeans(trainSub[, c(220:229)*3 + 3]) +
                       rowMeans(trainSub[, c(240:249)*3 + 3]) +
                       rowMeans(trainSub[, c(260:269)*3 + 3]) +
                       rowMeans(trainSub[, c(280:289)*3 + 3]) +
                       rowMeans(trainSub[, c(300:309)*3 + 3]) +
                       rowMeans(trainSub[, c(320:329)*3 + 3]) +
                       rowMeans(trainSub[, c(340:349)*3 + 3]) +
                       rowMeans(trainSub[, c(360:369)*3 + 3]) +
                       rowMeans(trainSub[, c(380:389)*3 + 3]))/20
colnames(trainAdjChop)[11:13] <- c("lfthfR", "lfthfG", "lfthfB")

trainAdjChop[14] <- (rowMeans(trainSub[, c(10:19)*3 + 1]) +
                       rowMeans(trainSub[, c(30:39)*3 + 1]) +
                       rowMeans(trainSub[, c(50:59)*3 + 1]) +
                       rowMeans(trainSub[, c(70:79)*3 + 1]) +
                       rowMeans(trainSub[, c(90:99)*3 + 1]) +
                       rowMeans(trainSub[, c(110:119)*3 + 1]) +
                       rowMeans(trainSub[, c(130:139)*3 + 1]) +
                       rowMeans(trainSub[, c(150:159)*3 + 1]) +
                       rowMeans(trainSub[, c(170:179)*3 + 1]) +
                       rowMeans(trainSub[, c(190:199)*3 + 1]) +
                       rowMeans(trainSub[, c(210:219)*3 + 1]) +
                       rowMeans(trainSub[, c(230:239)*3 + 1]) +
                       rowMeans(trainSub[, c(250:259)*3 + 1]) +
                       rowMeans(trainSub[, c(270:279)*3 + 1]) +
                       rowMeans(trainSub[, c(290:299)*3 + 1]) +
                       rowMeans(trainSub[, c(310:319)*3 + 1]) +
                       rowMeans(trainSub[, c(330:339)*3 + 1]) +
                       rowMeans(trainSub[, c(350:359)*3 + 1]) +
                       rowMeans(trainSub[, c(370:379)*3 + 1]) +
                       rowMeans(trainSub[, c(390:399)*3 + 1]))/20
trainAdjChop[15] <- (rowMeans(trainSub[, c(10:19)*3 + 2]) +
                       rowMeans(trainSub[, c(30:39)*3 + 2]) +
                       rowMeans(trainSub[, c(50:59)*3 + 2]) +
                       rowMeans(trainSub[, c(70:79)*3 + 2]) +
                       rowMeans(trainSub[, c(90:99)*3 + 2]) +
                       rowMeans(trainSub[, c(110:119)*3 + 2]) +
                       rowMeans(trainSub[, c(130:139)*3 + 2]) +
                       rowMeans(trainSub[, c(150:159)*3 + 2]) +
                       rowMeans(trainSub[, c(170:179)*3 + 2]) +
                       rowMeans(trainSub[, c(190:199)*3 + 2]) +
                       rowMeans(trainSub[, c(210:219)*3 + 2]) +
                       rowMeans(trainSub[, c(230:239)*3 + 2]) +
                       rowMeans(trainSub[, c(250:259)*3 + 2]) +
                       rowMeans(trainSub[, c(270:279)*3 + 2]) +
                       rowMeans(trainSub[, c(290:299)*3 + 2]) +
                       rowMeans(trainSub[, c(310:319)*3 + 2]) +
                       rowMeans(trainSub[, c(330:339)*3 + 2]) +
                       rowMeans(trainSub[, c(350:359)*3 + 2]) +
                       rowMeans(trainSub[, c(370:379)*3 + 2]) +
                       rowMeans(trainSub[, c(390:399)*3 + 2]))/20
trainAdjChop[16] <- (rowMeans(trainSub[, c(10:19)*3 + 3]) +
                       rowMeans(trainSub[, c(30:39)*3 + 3]) +
                       rowMeans(trainSub[, c(50:59)*3 + 3]) +
                       rowMeans(trainSub[, c(70:79)*3 + 3]) +
                       rowMeans(trainSub[, c(90:99)*3 + 3]) +
                       rowMeans(trainSub[, c(110:119)*3 + 3]) +
                       rowMeans(trainSub[, c(130:139)*3 + 3]) +
                       rowMeans(trainSub[, c(150:159)*3 + 3]) +
                       rowMeans(trainSub[, c(170:179)*3 + 3]) +
                       rowMeans(trainSub[, c(190:199)*3 + 3]) +
                       rowMeans(trainSub[, c(210:219)*3 + 3]) +
                       rowMeans(trainSub[, c(230:239)*3 + 3]) +
                       rowMeans(trainSub[, c(250:259)*3 + 3]) +
                       rowMeans(trainSub[, c(270:279)*3 + 3]) +
                       rowMeans(trainSub[, c(290:299)*3 + 3]) +
                       rowMeans(trainSub[, c(310:319)*3 + 3]) +
                       rowMeans(trainSub[, c(330:339)*3 + 3]) +
                       rowMeans(trainSub[, c(350:359)*3 + 3]) +
                       rowMeans(trainSub[, c(370:379)*3 + 3]) +
                       rowMeans(trainSub[, c(390:399)*3 + 3]))/20
colnames(trainAdjChop)[14:16] <- c("rthfR", "rthfG", "rthfB")

trainAdjChop[17] <- rowMeans(trainSub[, c(0:299)*3 + 1])
trainAdjChop[18] <- rowMeans(trainSub[, c(0:299)*3 + 2])
trainAdjChop[19] <- rowMeans(trainSub[, c(0:299)*3 + 3])
colnames(trainAdjChop)[17:19] <- c("tp3.4R", "tp3.4G", "tp3.4B")

trainAdjChop[20] <- rowMeans(trainSub[, c(100:399)*3 + 1])
trainAdjChop[21] <- rowMeans(trainSub[, c(100:399)*3 + 2])
trainAdjChop[22] <- rowMeans(trainSub[, c(100:399)*3 + 3])
colnames(trainAdjChop)[20:22] <- c("btm3.4R", "btm3.4G", "btm3.4B")

trainAdjChop[23] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                       rowMeans(trainSub[, c(20:24)*3 + 1]) +
                       rowMeans(trainSub[, c(40:44)*3 + 1]) +
                       rowMeans(trainSub[, c(60:64)*3 + 1]) +
                       rowMeans(trainSub[, c(80:84)*3 + 1]) +
                       rowMeans(trainSub[, c(100:104)*3 + 1]) +
                       rowMeans(trainSub[, c(120:124)*3 + 1]) +
                       rowMeans(trainSub[, c(140:144)*3 + 1]) +
                       rowMeans(trainSub[, c(160:164)*3 + 1]) +
                       rowMeans(trainSub[, c(180:184)*3 + 1]) +
                       rowMeans(trainSub[, c(200:204)*3 + 1]) +
                       rowMeans(trainSub[, c(220:224)*3 + 1]) +
                       rowMeans(trainSub[, c(240:244)*3 + 1]) +
                       rowMeans(trainSub[, c(260:264)*3 + 1]) +
                       rowMeans(trainSub[, c(280:284)*3 + 1]) +
                       rowMeans(trainSub[, c(300:304)*3 + 1]) +
                       rowMeans(trainSub[, c(320:324)*3 + 1]) +
                       rowMeans(trainSub[, c(340:344)*3 + 1]) +
                       rowMeans(trainSub[, c(360:364)*3 + 1]) +
                       rowMeans(trainSub[, c(380:384)*3 + 1]))/20
trainAdjChop[24] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                       rowMeans(trainSub[, c(20:24)*3 + 2]) +
                       rowMeans(trainSub[, c(40:44)*3 + 2]) +
                       rowMeans(trainSub[, c(60:64)*3 + 2]) +
                       rowMeans(trainSub[, c(80:84)*3 + 2]) +
                       rowMeans(trainSub[, c(100:104)*3 + 2]) +
                       rowMeans(trainSub[, c(120:124)*3 + 2]) +
                       rowMeans(trainSub[, c(140:144)*3 + 2]) +
                       rowMeans(trainSub[, c(160:164)*3 + 2]) +
                       rowMeans(trainSub[, c(180:184)*3 + 2]) +
                       rowMeans(trainSub[, c(200:204)*3 + 2]) +
                       rowMeans(trainSub[, c(220:224)*3 + 2]) +
                       rowMeans(trainSub[, c(240:244)*3 + 2]) +
                       rowMeans(trainSub[, c(260:264)*3 + 2]) +
                       rowMeans(trainSub[, c(280:284)*3 + 2]) +
                       rowMeans(trainSub[, c(300:304)*3 + 2]) +
                       rowMeans(trainSub[, c(320:324)*3 + 2]) +
                       rowMeans(trainSub[, c(340:344)*3 + 2]) +
                       rowMeans(trainSub[, c(360:364)*3 + 2]) +
                       rowMeans(trainSub[, c(380:384)*3 + 2]))/20
trainAdjChop[25] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                       rowMeans(trainSub[, c(20:24)*3 + 3]) +
                       rowMeans(trainSub[, c(40:44)*3 + 3]) +
                       rowMeans(trainSub[, c(60:64)*3 + 3]) +
                       rowMeans(trainSub[, c(80:84)*3 + 3]) +
                       rowMeans(trainSub[, c(100:104)*3 + 3]) +
                       rowMeans(trainSub[, c(120:124)*3 + 3]) +
                       rowMeans(trainSub[, c(140:144)*3 + 3]) +
                       rowMeans(trainSub[, c(160:164)*3 + 3]) +
                       rowMeans(trainSub[, c(180:184)*3 + 3]) +
                       rowMeans(trainSub[, c(200:204)*3 + 3]) +
                       rowMeans(trainSub[, c(220:224)*3 + 3]) +
                       rowMeans(trainSub[, c(240:244)*3 + 3]) +
                       rowMeans(trainSub[, c(260:264)*3 + 3]) +
                       rowMeans(trainSub[, c(280:284)*3 + 3]) +
                       rowMeans(trainSub[, c(300:304)*3 + 3]) +
                       rowMeans(trainSub[, c(320:324)*3 + 3]) +
                       rowMeans(trainSub[, c(340:344)*3 + 3]) +
                       rowMeans(trainSub[, c(360:364)*3 + 3]) +
                       rowMeans(trainSub[, c(380:384)*3 + 3]))/20
colnames(trainAdjChop)[23:25] <- c("x4vert1R", "x4vert1G", "x4vert1B")

trainAdjChop[26] <- (rowMeans(trainSub[, c(5:9)*3 + 1]) +
                       rowMeans(trainSub[, c(25:29)*3 + 1]) +
                       rowMeans(trainSub[, c(45:49)*3 + 1]) +
                       rowMeans(trainSub[, c(65:69)*3 + 1]) +
                       rowMeans(trainSub[, c(85:89)*3 + 1]) +
                       rowMeans(trainSub[, c(105:109)*3 + 1]) +
                       rowMeans(trainSub[, c(125:129)*3 + 1]) +
                       rowMeans(trainSub[, c(145:149)*3 + 1]) +
                       rowMeans(trainSub[, c(165:169)*3 + 1]) +
                       rowMeans(trainSub[, c(185:189)*3 + 1]) +
                       rowMeans(trainSub[, c(205:209)*3 + 1]) +
                       rowMeans(trainSub[, c(225:229)*3 + 1]) +
                       rowMeans(trainSub[, c(245:249)*3 + 1]) +
                       rowMeans(trainSub[, c(265:269)*3 + 1]) +
                       rowMeans(trainSub[, c(285:289)*3 + 1]) +
                       rowMeans(trainSub[, c(305:309)*3 + 1]) +
                       rowMeans(trainSub[, c(325:329)*3 + 1]) +
                       rowMeans(trainSub[, c(345:349)*3 + 1]) +
                       rowMeans(trainSub[, c(365:369)*3 + 1]) +
                       rowMeans(trainSub[, c(385:389)*3 + 1]))/20
trainAdjChop[27] <- (rowMeans(trainSub[, c(5:9)*3 + 2]) +
                       rowMeans(trainSub[, c(25:29)*3 + 2]) +
                       rowMeans(trainSub[, c(45:49)*3 + 2]) +
                       rowMeans(trainSub[, c(65:69)*3 + 2]) +
                       rowMeans(trainSub[, c(85:89)*3 + 2]) +
                       rowMeans(trainSub[, c(105:109)*3 + 2]) +
                       rowMeans(trainSub[, c(125:129)*3 + 2]) +
                       rowMeans(trainSub[, c(145:149)*3 + 2]) +
                       rowMeans(trainSub[, c(165:169)*3 + 2]) +
                       rowMeans(trainSub[, c(185:189)*3 + 2]) +
                       rowMeans(trainSub[, c(205:209)*3 + 2]) +
                       rowMeans(trainSub[, c(225:229)*3 + 2]) +
                       rowMeans(trainSub[, c(245:249)*3 + 2]) +
                       rowMeans(trainSub[, c(265:269)*3 + 2]) +
                       rowMeans(trainSub[, c(285:289)*3 + 2]) +
                       rowMeans(trainSub[, c(305:309)*3 + 2]) +
                       rowMeans(trainSub[, c(325:329)*3 + 2]) +
                       rowMeans(trainSub[, c(345:349)*3 + 2]) +
                       rowMeans(trainSub[, c(365:369)*3 + 2]) +
                       rowMeans(trainSub[, c(385:389)*3 + 2]))/20
trainAdjChop[28] <- (rowMeans(trainSub[, c(5:9)*3 + 3]) +
                       rowMeans(trainSub[, c(25:29)*3 + 3]) +
                       rowMeans(trainSub[, c(45:49)*3 + 3]) +
                       rowMeans(trainSub[, c(65:69)*3 + 3]) +
                       rowMeans(trainSub[, c(85:89)*3 + 3]) +
                       rowMeans(trainSub[, c(105:109)*3 + 3]) +
                       rowMeans(trainSub[, c(125:129)*3 + 3]) +
                       rowMeans(trainSub[, c(145:149)*3 + 3]) +
                       rowMeans(trainSub[, c(165:169)*3 + 3]) +
                       rowMeans(trainSub[, c(185:189)*3 + 3]) +
                       rowMeans(trainSub[, c(205:209)*3 + 3]) +
                       rowMeans(trainSub[, c(225:229)*3 + 3]) +
                       rowMeans(trainSub[, c(245:249)*3 + 3]) +
                       rowMeans(trainSub[, c(265:269)*3 + 3]) +
                       rowMeans(trainSub[, c(285:289)*3 + 3]) +
                       rowMeans(trainSub[, c(305:309)*3 + 3]) +
                       rowMeans(trainSub[, c(325:329)*3 + 3]) +
                       rowMeans(trainSub[, c(345:349)*3 + 3]) +
                       rowMeans(trainSub[, c(365:369)*3 + 3]) +
                       rowMeans(trainSub[, c(385:389)*3 + 3]))/20
colnames(trainAdjChop)[26:28] <- c("x4vert2R", "x4vert2G", "x4vert2B")

trainAdjChop[29] <- (rowMeans(trainSub[, c(10:14)*3 + 1]) +
                       rowMeans(trainSub[, c(30:34)*3 + 1]) +
                       rowMeans(trainSub[, c(50:54)*3 + 1]) +
                       rowMeans(trainSub[, c(70:74)*3 + 1]) +
                       rowMeans(trainSub[, c(90:94)*3 + 1]) +
                       rowMeans(trainSub[, c(110:114)*3 + 1]) +
                       rowMeans(trainSub[, c(130:134)*3 + 1]) +
                       rowMeans(trainSub[, c(150:154)*3 + 1]) +
                       rowMeans(trainSub[, c(170:174)*3 + 1]) +
                       rowMeans(trainSub[, c(190:194)*3 + 1]) +
                       rowMeans(trainSub[, c(210:214)*3 + 1]) +
                       rowMeans(trainSub[, c(230:234)*3 + 1]) +
                       rowMeans(trainSub[, c(250:254)*3 + 1]) +
                       rowMeans(trainSub[, c(270:274)*3 + 1]) +
                       rowMeans(trainSub[, c(290:294)*3 + 1]) +
                       rowMeans(trainSub[, c(310:314)*3 + 1]) +
                       rowMeans(trainSub[, c(330:334)*3 + 1]) +
                       rowMeans(trainSub[, c(350:354)*3 + 1]) +
                       rowMeans(trainSub[, c(370:374)*3 + 1]) +
                       rowMeans(trainSub[, c(390:394)*3 + 1]))/20
trainAdjChop[30] <- (rowMeans(trainSub[, c(10:14)*3 + 2]) +
                       rowMeans(trainSub[, c(30:34)*3 + 2]) +
                       rowMeans(trainSub[, c(50:54)*3 + 2]) +
                       rowMeans(trainSub[, c(70:74)*3 + 2]) +
                       rowMeans(trainSub[, c(90:94)*3 + 2]) +
                       rowMeans(trainSub[, c(110:114)*3 + 2]) +
                       rowMeans(trainSub[, c(130:134)*3 + 2]) +
                       rowMeans(trainSub[, c(150:154)*3 + 2]) +
                       rowMeans(trainSub[, c(170:174)*3 + 2]) +
                       rowMeans(trainSub[, c(190:194)*3 + 2]) +
                       rowMeans(trainSub[, c(210:214)*3 + 2]) +
                       rowMeans(trainSub[, c(230:234)*3 + 2]) +
                       rowMeans(trainSub[, c(250:254)*3 + 2]) +
                       rowMeans(trainSub[, c(270:274)*3 + 2]) +
                       rowMeans(trainSub[, c(290:294)*3 + 2]) +
                       rowMeans(trainSub[, c(310:314)*3 + 2]) +
                       rowMeans(trainSub[, c(330:334)*3 + 2]) +
                       rowMeans(trainSub[, c(350:354)*3 + 2]) +
                       rowMeans(trainSub[, c(370:374)*3 + 2]) +
                       rowMeans(trainSub[, c(390:394)*3 + 2]))/20
trainAdjChop[31] <- (rowMeans(trainSub[, c(10:14)*3 + 3]) +
                       rowMeans(trainSub[, c(30:34)*3 + 3]) +
                       rowMeans(trainSub[, c(50:54)*3 + 3]) +
                       rowMeans(trainSub[, c(70:74)*3 + 3]) +
                       rowMeans(trainSub[, c(90:94)*3 + 3]) +
                       rowMeans(trainSub[, c(110:114)*3 + 3]) +
                       rowMeans(trainSub[, c(130:134)*3 + 3]) +
                       rowMeans(trainSub[, c(150:154)*3 + 3]) +
                       rowMeans(trainSub[, c(170:174)*3 + 3]) +
                       rowMeans(trainSub[, c(190:194)*3 + 3]) +
                       rowMeans(trainSub[, c(210:214)*3 + 3]) +
                       rowMeans(trainSub[, c(230:234)*3 + 3]) +
                       rowMeans(trainSub[, c(250:254)*3 + 3]) +
                       rowMeans(trainSub[, c(270:274)*3 + 3]) +
                       rowMeans(trainSub[, c(290:294)*3 + 3]) +
                       rowMeans(trainSub[, c(310:314)*3 + 3]) +
                       rowMeans(trainSub[, c(330:334)*3 + 3]) +
                       rowMeans(trainSub[, c(350:354)*3 + 3]) +
                       rowMeans(trainSub[, c(370:374)*3 + 3]) +
                       rowMeans(trainSub[, c(390:394)*3 + 3]))/20
colnames(trainAdjChop)[29:31] <- c("x4vert3R", "x4vert3G", "x4vert3B")

trainAdjChop[32] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                       rowMeans(trainSub[, c(35:39)*3 + 1]) +
                       rowMeans(trainSub[, c(55:59)*3 + 1]) +
                       rowMeans(trainSub[, c(75:79)*3 + 1]) +
                       rowMeans(trainSub[, c(95:99)*3 + 1]) +
                       rowMeans(trainSub[, c(115:119)*3 + 1]) +
                       rowMeans(trainSub[, c(135:139)*3 + 1]) +
                       rowMeans(trainSub[, c(155:159)*3 + 1]) +
                       rowMeans(trainSub[, c(175:179)*3 + 1]) +
                       rowMeans(trainSub[, c(195:199)*3 + 1]) +
                       rowMeans(trainSub[, c(215:219)*3 + 1]) +
                       rowMeans(trainSub[, c(235:239)*3 + 1]) +
                       rowMeans(trainSub[, c(255:259)*3 + 1]) +
                       rowMeans(trainSub[, c(275:279)*3 + 1]) +
                       rowMeans(trainSub[, c(295:299)*3 + 1]) +
                       rowMeans(trainSub[, c(315:319)*3 + 1]) +
                       rowMeans(trainSub[, c(335:339)*3 + 1]) +
                       rowMeans(trainSub[, c(355:359)*3 + 1]) +
                       rowMeans(trainSub[, c(375:379)*3 + 1]) +
                       rowMeans(trainSub[, c(395:399)*3 + 1]))/20
trainAdjChop[33] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                       rowMeans(trainSub[, c(35:39)*3 + 2]) +
                       rowMeans(trainSub[, c(55:59)*3 + 2]) +
                       rowMeans(trainSub[, c(75:79)*3 + 2]) +
                       rowMeans(trainSub[, c(95:99)*3 + 2]) +
                       rowMeans(trainSub[, c(115:119)*3 + 2]) +
                       rowMeans(trainSub[, c(135:139)*3 + 2]) +
                       rowMeans(trainSub[, c(155:159)*3 + 2]) +
                       rowMeans(trainSub[, c(175:179)*3 + 2]) +
                       rowMeans(trainSub[, c(195:199)*3 + 2]) +
                       rowMeans(trainSub[, c(215:219)*3 + 2]) +
                       rowMeans(trainSub[, c(235:239)*3 + 2]) +
                       rowMeans(trainSub[, c(255:259)*3 + 2]) +
                       rowMeans(trainSub[, c(275:279)*3 + 2]) +
                       rowMeans(trainSub[, c(295:299)*3 + 2]) +
                       rowMeans(trainSub[, c(315:319)*3 + 2]) +
                       rowMeans(trainSub[, c(335:339)*3 + 2]) +
                       rowMeans(trainSub[, c(355:359)*3 + 2]) +
                       rowMeans(trainSub[, c(375:379)*3 + 2]) +
                       rowMeans(trainSub[, c(395:399)*3 + 2]))/20
trainAdjChop[34] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                       rowMeans(trainSub[, c(35:39)*3 + 3]) +
                       rowMeans(trainSub[, c(55:59)*3 + 3]) +
                       rowMeans(trainSub[, c(75:79)*3 + 3]) +
                       rowMeans(trainSub[, c(95:99)*3 + 3]) +
                       rowMeans(trainSub[, c(115:119)*3 + 3]) +
                       rowMeans(trainSub[, c(135:139)*3 + 3]) +
                       rowMeans(trainSub[, c(155:159)*3 + 3]) +
                       rowMeans(trainSub[, c(175:179)*3 + 3]) +
                       rowMeans(trainSub[, c(195:199)*3 + 3]) +
                       rowMeans(trainSub[, c(215:219)*3 + 3]) +
                       rowMeans(trainSub[, c(235:239)*3 + 3]) +
                       rowMeans(trainSub[, c(255:259)*3 + 3]) +
                       rowMeans(trainSub[, c(275:279)*3 + 3]) +
                       rowMeans(trainSub[, c(295:299)*3 + 3]) +
                       rowMeans(trainSub[, c(315:319)*3 + 3]) +
                       rowMeans(trainSub[, c(335:339)*3 + 3]) +
                       rowMeans(trainSub[, c(355:359)*3 + 3]) +
                       rowMeans(trainSub[, c(375:379)*3 + 3]) +
                       rowMeans(trainSub[, c(395:399)*3 + 3]))/20
colnames(trainAdjChop)[32:34] <- c("x4vert4R", "x4vert4G", "x4vert4B")

trainAdjChop[35] <- rowMeans(trainSub[, c(0:99)*3 + 1])
trainAdjChop[36] <- rowMeans(trainSub[, c(0:99)*3 + 2])
trainAdjChop[37] <- rowMeans(trainSub[, c(0:99)*3 + 3])
colnames(trainAdjChop)[35:37] <- c("x4hori1R", "x4hori1G", "x4hori1B")

trainAdjChop[38] <- rowMeans(trainSub[, c(100:199)*3 + 1])
trainAdjChop[39] <- rowMeans(trainSub[, c(100:199)*3 + 2])
trainAdjChop[40] <- rowMeans(trainSub[, c(100:199)*3 + 3])
colnames(trainAdjChop)[38:40] <- c("x4hori2R", "x4hori2G", "x4hori2B")

trainAdjChop[41] <- rowMeans(trainSub[, c(200:299)*3 + 1])
trainAdjChop[42] <- rowMeans(trainSub[, c(200:299)*3 + 2])
trainAdjChop[43] <- rowMeans(trainSub[, c(200:299)*3 + 3])
colnames(trainAdjChop)[41:43] <- c("x4hori3R", "x4hori3G", "x4hori3B")

trainAdjChop[44] <- rowMeans(trainSub[, c(300:399)*3 + 1])
trainAdjChop[45] <- rowMeans(trainSub[, c(300:399)*3 + 2])
trainAdjChop[46] <- rowMeans(trainSub[, c(300:399)*3 + 3])
colnames(trainAdjChop)[44:46] <- c("x4hori4R", "x4hori4G", "x4hori4B")

trainAdjChop[47] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                       rowMeans(trainSub[, c(20:24)*3 + 1]) +
                       rowMeans(trainSub[, c(40:44)*3 + 1]) +
                       rowMeans(trainSub[, c(60:64)*3 + 1]) +
                       rowMeans(trainSub[, c(80:84)*3 + 1]) +
                       rowMeans(trainSub[, c(100:104)*3 + 1]) +
                       rowMeans(trainSub[, c(120:124)*3 + 1]) +
                       rowMeans(trainSub[, c(140:144)*3 + 1]) +
                       rowMeans(trainSub[, c(160:164)*3 + 1]) +
                       rowMeans(trainSub[, c(180:184)*3 + 1]))/10
trainAdjChop[48] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                       rowMeans(trainSub[, c(20:24)*3 + 2]) +
                       rowMeans(trainSub[, c(40:44)*3 + 2]) +
                       rowMeans(trainSub[, c(60:64)*3 + 2]) +
                       rowMeans(trainSub[, c(80:84)*3 + 2]) +
                       rowMeans(trainSub[, c(100:104)*3 + 2]) +
                       rowMeans(trainSub[, c(120:124)*3 + 2]) +
                       rowMeans(trainSub[, c(140:144)*3 + 2]) +
                       rowMeans(trainSub[, c(160:164)*3 + 2]) +
                       rowMeans(trainSub[, c(180:184)*3 + 2]))/10
trainAdjChop[49] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                       rowMeans(trainSub[, c(20:24)*3 + 3]) +
                       rowMeans(trainSub[, c(40:44)*3 + 3]) +
                       rowMeans(trainSub[, c(60:64)*3 + 3]) +
                       rowMeans(trainSub[, c(80:84)*3 + 3]) +
                       rowMeans(trainSub[, c(100:104)*3 + 3]) +
                       rowMeans(trainSub[, c(120:124)*3 + 3]) +
                       rowMeans(trainSub[, c(140:144)*3 + 3]) +
                       rowMeans(trainSub[, c(160:164)*3 + 3]) +
                       rowMeans(trainSub[, c(180:184)*3 + 3]))/10
colnames(trainAdjChop)[47:49] <- c("x8verts1R", "x8verts1G", "x8verts1B")

trainAdjChop[50] <- (rowMeans(trainSub[, c(200:204)*3 + 1]) +
                       rowMeans(trainSub[, c(220:224)*3 + 1]) +
                       rowMeans(trainSub[, c(240:244)*3 + 1]) +
                       rowMeans(trainSub[, c(260:264)*3 + 1]) +
                       rowMeans(trainSub[, c(280:284)*3 + 1]) +
                       rowMeans(trainSub[, c(300:304)*3 + 1]) +
                       rowMeans(trainSub[, c(320:324)*3 + 1]) +
                       rowMeans(trainSub[, c(340:344)*3 + 1]) +
                       rowMeans(trainSub[, c(360:364)*3 + 1]) +
                       rowMeans(trainSub[, c(380:384)*3 + 1]))/10
trainAdjChop[51] <- (rowMeans(trainSub[, c(200:204)*3 + 2]) +
                       rowMeans(trainSub[, c(220:224)*3 + 2]) +
                       rowMeans(trainSub[, c(240:244)*3 + 2]) +
                       rowMeans(trainSub[, c(260:264)*3 + 2]) +
                       rowMeans(trainSub[, c(280:284)*3 + 2]) +
                       rowMeans(trainSub[, c(300:304)*3 + 2]) +
                       rowMeans(trainSub[, c(320:324)*3 + 2]) +
                       rowMeans(trainSub[, c(340:344)*3 + 2]) +
                       rowMeans(trainSub[, c(360:364)*3 + 2]) +
                       rowMeans(trainSub[, c(380:384)*3 + 2]))/10
trainAdjChop[52] <- (rowMeans(trainSub[, c(200:204)*3 + 3]) +
                       rowMeans(trainSub[, c(220:224)*3 + 3]) +
                       rowMeans(trainSub[, c(240:244)*3 + 3]) +
                       rowMeans(trainSub[, c(260:264)*3 + 3]) +
                       rowMeans(trainSub[, c(280:284)*3 + 3]) +
                       rowMeans(trainSub[, c(300:304)*3 + 3]) +
                       rowMeans(trainSub[, c(320:324)*3 + 3]) +
                       rowMeans(trainSub[, c(340:344)*3 + 3]) +
                       rowMeans(trainSub[, c(360:364)*3 + 3]) +
                       rowMeans(trainSub[, c(380:384)*3 + 3]))/10
colnames(trainAdjChop)[50:52] <- c("x8verts2R", "x8verts2G", "x8verts2B")

trainAdjChop[53] <- (rowMeans(trainSub[, c(5:9)*3 + 1]) +
                       rowMeans(trainSub[, c(25:29)*3 + 1]) +
                       rowMeans(trainSub[, c(45:49)*3 + 1]) +
                       rowMeans(trainSub[, c(65:69)*3 + 1]) +
                       rowMeans(trainSub[, c(85:89)*3 + 1]) +
                       rowMeans(trainSub[, c(105:109)*3 + 1]) +
                       rowMeans(trainSub[, c(125:129)*3 + 1]) +
                       rowMeans(trainSub[, c(145:149)*3 + 1]) +
                       rowMeans(trainSub[, c(165:169)*3 + 1]) +
                       rowMeans(trainSub[, c(185:189)*3 + 1]))/10
trainAdjChop[54] <- (rowMeans(trainSub[, c(5:9)*3 + 2]) +
                       rowMeans(trainSub[, c(25:29)*3 + 2]) +
                       rowMeans(trainSub[, c(45:49)*3 + 2]) +
                       rowMeans(trainSub[, c(65:69)*3 + 2]) +
                       rowMeans(trainSub[, c(85:89)*3 + 2]) +
                       rowMeans(trainSub[, c(105:109)*3 + 2]) +
                       rowMeans(trainSub[, c(125:129)*3 + 2]) +
                       rowMeans(trainSub[, c(145:149)*3 + 2]) +
                       rowMeans(trainSub[, c(165:169)*3 + 2]) +
                       rowMeans(trainSub[, c(185:189)*3 + 2]))/10
trainAdjChop[55] <- (rowMeans(trainSub[, c(5:9)*3 + 3]) +
                       rowMeans(trainSub[, c(25:29)*3 + 3]) +
                       rowMeans(trainSub[, c(45:49)*3 + 3]) +
                       rowMeans(trainSub[, c(65:69)*3 + 3]) +
                       rowMeans(trainSub[, c(85:89)*3 + 3]) +
                       rowMeans(trainSub[, c(105:109)*3 + 3]) +
                       rowMeans(trainSub[, c(125:129)*3 + 3]) +
                       rowMeans(trainSub[, c(145:149)*3 + 3]) +
                       rowMeans(trainSub[, c(165:169)*3 + 3]) +
                       rowMeans(trainSub[, c(185:189)*3 + 3]))/10
colnames(trainAdjChop)[53:55] <- c("x8verts3R", "x8verts3G", "x8verts3B")


trainAdjChop[56] <- (rowMeans(trainSub[, c(205:209)*3 + 1]) +
                       rowMeans(trainSub[, c(225:229)*3 + 1]) +
                       rowMeans(trainSub[, c(245:249)*3 + 1]) +
                       rowMeans(trainSub[, c(265:269)*3 + 1]) +
                       rowMeans(trainSub[, c(285:289)*3 + 1]) +
                       rowMeans(trainSub[, c(305:309)*3 + 1]) +
                       rowMeans(trainSub[, c(325:329)*3 + 1]) +
                       rowMeans(trainSub[, c(345:349)*3 + 1]) +
                       rowMeans(trainSub[, c(365:369)*3 + 1]) +
                       rowMeans(trainSub[, c(385:389)*3 + 1]))/10
trainAdjChop[57] <- (rowMeans(trainSub[, c(205:209)*3 + 2]) +
                       rowMeans(trainSub[, c(225:229)*3 + 2]) +
                       rowMeans(trainSub[, c(245:249)*3 + 2]) +
                       rowMeans(trainSub[, c(265:269)*3 + 2]) +
                       rowMeans(trainSub[, c(285:289)*3 + 2]) +
                       rowMeans(trainSub[, c(305:309)*3 + 2]) +
                       rowMeans(trainSub[, c(325:329)*3 + 2]) +
                       rowMeans(trainSub[, c(345:349)*3 + 2]) +
                       rowMeans(trainSub[, c(365:369)*3 + 2]) +
                       rowMeans(trainSub[, c(385:389)*3 + 2]))/10
trainAdjChop[58] <- (rowMeans(trainSub[, c(205:209)*3 + 3]) +
                       rowMeans(trainSub[, c(225:229)*3 + 3]) +
                       rowMeans(trainSub[, c(245:249)*3 + 3]) +
                       rowMeans(trainSub[, c(265:269)*3 + 3]) +
                       rowMeans(trainSub[, c(285:289)*3 + 3]) +
                       rowMeans(trainSub[, c(305:309)*3 + 3]) +
                       rowMeans(trainSub[, c(325:329)*3 + 3]) +
                       rowMeans(trainSub[, c(345:349)*3 + 3]) +
                       rowMeans(trainSub[, c(365:369)*3 + 3]) +
                       rowMeans(trainSub[, c(385:389)*3 + 3]))/10
colnames(trainAdjChop)[56:58] <- c("x8verts4R", "x8verts4G", "x8verts4B")

trainAdjChop[59] <- (rowMeans(trainSub[, c(10:14)*3 + 1]) +
                       rowMeans(trainSub[, c(30:34)*3 + 1]) +
                       rowMeans(trainSub[, c(50:54)*3 + 1]) +
                       rowMeans(trainSub[, c(70:74)*3 + 1]) +
                       rowMeans(trainSub[, c(90:94)*3 + 1]) +
                       rowMeans(trainSub[, c(110:114)*3 + 1]) +
                       rowMeans(trainSub[, c(130:134)*3 + 1]) +
                       rowMeans(trainSub[, c(150:154)*3 + 1]) +
                       rowMeans(trainSub[, c(170:174)*3 + 1]) +
                       rowMeans(trainSub[, c(190:194)*3 + 1]))/10
trainAdjChop[60] <- (rowMeans(trainSub[, c(10:14)*3 + 2]) +
                       rowMeans(trainSub[, c(30:34)*3 + 2]) +
                       rowMeans(trainSub[, c(50:54)*3 + 2]) +
                       rowMeans(trainSub[, c(70:74)*3 + 2]) +
                       rowMeans(trainSub[, c(90:94)*3 + 2]) +
                       rowMeans(trainSub[, c(110:114)*3 + 2]) +
                       rowMeans(trainSub[, c(130:134)*3 + 2]) +
                       rowMeans(trainSub[, c(150:154)*3 + 2]) +
                       rowMeans(trainSub[, c(170:174)*3 + 2]) +
                       rowMeans(trainSub[, c(190:194)*3 + 2]))/10 
trainAdjChop[61] <- (rowMeans(trainSub[, c(10:14)*3 + 3]) +
                       rowMeans(trainSub[, c(30:34)*3 + 3]) +
                       rowMeans(trainSub[, c(50:54)*3 + 3]) +
                       rowMeans(trainSub[, c(70:74)*3 + 3]) +
                       rowMeans(trainSub[, c(90:94)*3 + 3]) +
                       rowMeans(trainSub[, c(110:114)*3 + 3]) +
                       rowMeans(trainSub[, c(130:134)*3 + 3]) +
                       rowMeans(trainSub[, c(150:154)*3 + 3]) +
                       rowMeans(trainSub[, c(170:174)*3 + 3]) +
                       rowMeans(trainSub[, c(190:194)*3 + 3]))/10
colnames(trainAdjChop)[59:61] <- c("x8verts5R", "x8verts5G", "x8verts5B")

trainAdjChop[62] <- (rowMeans(trainSub[, c(210:214)*3 + 1]) +
                       rowMeans(trainSub[, c(230:234)*3 + 1]) +
                       rowMeans(trainSub[, c(250:254)*3 + 1]) +
                       rowMeans(trainSub[, c(270:274)*3 + 1]) +
                       rowMeans(trainSub[, c(290:294)*3 + 1]) +
                       rowMeans(trainSub[, c(310:314)*3 + 1]) +
                       rowMeans(trainSub[, c(330:334)*3 + 1]) +
                       rowMeans(trainSub[, c(350:354)*3 + 1]) +
                       rowMeans(trainSub[, c(370:374)*3 + 1]) +
                       rowMeans(trainSub[, c(390:394)*3 + 1]))/10
trainAdjChop[63] <- (rowMeans(trainSub[, c(210:214)*3 + 2]) +
                       rowMeans(trainSub[, c(230:234)*3 + 2]) +
                       rowMeans(trainSub[, c(250:254)*3 + 2]) +
                       rowMeans(trainSub[, c(270:274)*3 + 2]) +
                       rowMeans(trainSub[, c(290:294)*3 + 2]) +
                       rowMeans(trainSub[, c(310:314)*3 + 2]) +
                       rowMeans(trainSub[, c(330:334)*3 + 2]) +
                       rowMeans(trainSub[, c(350:354)*3 + 2]) +
                       rowMeans(trainSub[, c(370:374)*3 + 2]) +
                       rowMeans(trainSub[, c(390:394)*3 + 2]))/10
trainAdjChop[64] <- (rowMeans(trainSub[, c(210:214)*3 + 3]) +
                       rowMeans(trainSub[, c(230:234)*3 + 3]) +
                       rowMeans(trainSub[, c(250:254)*3 + 3]) +
                       rowMeans(trainSub[, c(270:274)*3 + 3]) +
                       rowMeans(trainSub[, c(290:294)*3 + 3]) +
                       rowMeans(trainSub[, c(310:314)*3 + 3]) +
                       rowMeans(trainSub[, c(330:334)*3 + 3]) +
                       rowMeans(trainSub[, c(350:354)*3 + 3]) +
                       rowMeans(trainSub[, c(370:374)*3 + 3]) +
                       rowMeans(trainSub[, c(390:394)*3 + 3]))/10
colnames(trainAdjChop)[62:64] <- c("x8verts6R", "x8verts6G", "x8verts6B")

trainAdjChop[65] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                       rowMeans(trainSub[, c(35:39)*3 + 1]) +
                       rowMeans(trainSub[, c(55:59)*3 + 1]) +
                       rowMeans(trainSub[, c(75:79)*3 + 1]) +
                       rowMeans(trainSub[, c(95:99)*3 + 1]) +
                       rowMeans(trainSub[, c(115:119)*3 + 1]) +
                       rowMeans(trainSub[, c(135:139)*3 + 1]) +
                       rowMeans(trainSub[, c(155:159)*3 + 1]) +
                       rowMeans(trainSub[, c(175:179)*3 + 1]) +
                       rowMeans(trainSub[, c(195:199)*3 + 1]))/10
trainAdjChop[66] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                       rowMeans(trainSub[, c(35:39)*3 + 2]) +
                       rowMeans(trainSub[, c(55:59)*3 + 2]) +
                       rowMeans(trainSub[, c(75:79)*3 + 2]) +
                       rowMeans(trainSub[, c(95:99)*3 + 2]) +
                       rowMeans(trainSub[, c(115:119)*3 + 2]) +
                       rowMeans(trainSub[, c(135:139)*3 + 2]) +
                       rowMeans(trainSub[, c(155:159)*3 + 2]) +
                       rowMeans(trainSub[, c(175:179)*3 + 2]) +
                       rowMeans(trainSub[, c(195:199)*3 + 2]))/10 
trainAdjChop[67] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                       rowMeans(trainSub[, c(35:39)*3 + 3]) +
                       rowMeans(trainSub[, c(55:59)*3 + 3]) +
                       rowMeans(trainSub[, c(75:79)*3 + 3]) +
                       rowMeans(trainSub[, c(95:99)*3 + 3]) +
                       rowMeans(trainSub[, c(115:119)*3 + 3]) +
                       rowMeans(trainSub[, c(135:139)*3 + 3]) +
                       rowMeans(trainSub[, c(155:159)*3 + 3]) +
                       rowMeans(trainSub[, c(175:179)*3 + 3]) +
                       rowMeans(trainSub[, c(195:199)*3 + 3]))/10 
colnames(trainAdjChop)[65:67] <- c("x8verts7R", "x8verts7G", "x8verts7B")

trainAdjChop[68] <- (rowMeans(trainSub[, c(215:219)*3 + 1]) +
                       rowMeans(trainSub[, c(235:239)*3 + 1]) +
                       rowMeans(trainSub[, c(255:259)*3 + 1]) +
                       rowMeans(trainSub[, c(275:279)*3 + 1]) +
                       rowMeans(trainSub[, c(295:299)*3 + 1]) +
                       rowMeans(trainSub[, c(315:319)*3 + 1]) +
                       rowMeans(trainSub[, c(335:339)*3 + 1]) +
                       rowMeans(trainSub[, c(355:359)*3 + 1]) +
                       rowMeans(trainSub[, c(375:379)*3 + 1]) +
                       rowMeans(trainSub[, c(395:399)*3 + 1]))/10
trainAdjChop[69] <- (rowMeans(trainSub[, c(215:219)*3 + 2]) +
                       rowMeans(trainSub[, c(235:239)*3 + 2]) +
                       rowMeans(trainSub[, c(255:259)*3 + 2]) +
                       rowMeans(trainSub[, c(275:279)*3 + 2]) +
                       rowMeans(trainSub[, c(295:299)*3 + 2]) +
                       rowMeans(trainSub[, c(315:319)*3 + 2]) +
                       rowMeans(trainSub[, c(335:339)*3 + 2]) +
                       rowMeans(trainSub[, c(355:359)*3 + 2]) +
                       rowMeans(trainSub[, c(375:379)*3 + 2]) +
                       rowMeans(trainSub[, c(395:399)*3 + 2]))/10
trainAdjChop[70] <- (rowMeans(trainSub[, c(215:219)*3 + 3]) +
                       rowMeans(trainSub[, c(235:239)*3 + 3]) +
                       rowMeans(trainSub[, c(255:259)*3 + 3]) +
                       rowMeans(trainSub[, c(275:279)*3 + 3]) +
                       rowMeans(trainSub[, c(295:299)*3 + 3]) +
                       rowMeans(trainSub[, c(315:319)*3 + 3]) +
                       rowMeans(trainSub[, c(335:339)*3 + 3]) +
                       rowMeans(trainSub[, c(355:359)*3 + 3]) +
                       rowMeans(trainSub[, c(375:379)*3 + 3]) +
                       rowMeans(trainSub[, c(395:399)*3 + 3]))/10
colnames(trainAdjChop)[68:70] <- c("x8verts8R", "x8verts8G", "x8verts8B")

trainAdjChop[71] <- (rowMeans(trainSub[, c(0:9)*3 + 1]) +
                       rowMeans(trainSub[, c(20:29)*3 + 1]) +
                       rowMeans(trainSub[, c(40:49)*3 + 1]) +
                       rowMeans(trainSub[, c(60:69)*3 + 1]) +
                       rowMeans(trainSub[, c(80:89)*3 + 1]))/5
trainAdjChop[72] <- (rowMeans(trainSub[, c(0:9)*3 + 2]) +
                       rowMeans(trainSub[, c(20:29)*3 + 2]) +
                       rowMeans(trainSub[, c(40:49)*3 + 2]) +
                       rowMeans(trainSub[, c(60:69)*3 + 2]) +
                       rowMeans(trainSub[, c(80:89)*3 + 2]))/5  
trainAdjChop[73] <- (rowMeans(trainSub[, c(0:9)*3 + 3]) +
                       rowMeans(trainSub[, c(20:29)*3 + 3]) +
                       rowMeans(trainSub[, c(40:49)*3 + 3]) +
                       rowMeans(trainSub[, c(60:69)*3 + 3]) +
                       rowMeans(trainSub[, c(80:89)*3 + 3]))/5
colnames(trainAdjChop)[71:73] <- c("x8hori1R", "x8hori1G", "x8hori1B")

trainAdjChop[74] <- (rowMeans(trainSub[, c(100:109)*3 + 1]) +
                       rowMeans(trainSub[, c(120:129)*3 + 1]) +
                       rowMeans(trainSub[, c(140:149)*3 + 1]) +
                       rowMeans(trainSub[, c(160:169)*3 + 1]) +
                       rowMeans(trainSub[, c(180:189)*3 + 1]))/5
trainAdjChop[75] <- (rowMeans(trainSub[, c(100:109)*3 + 2]) +
                       rowMeans(trainSub[, c(120:129)*3 + 2]) +
                       rowMeans(trainSub[, c(140:149)*3 + 2]) +
                       rowMeans(trainSub[, c(160:169)*3 + 2]) +
                       rowMeans(trainSub[, c(180:189)*3 + 2]))/5
trainAdjChop[76] <- (rowMeans(trainSub[, c(100:109)*3 + 3]) +
                       rowMeans(trainSub[, c(120:129)*3 + 3]) +
                       rowMeans(trainSub[, c(140:149)*3 + 3]) +
                       rowMeans(trainSub[, c(160:169)*3 + 3]) +
                       rowMeans(trainSub[, c(180:189)*3 + 3]))/5
colnames(trainAdjChop)[74:76] <- c("x8hori2R", "x8hori2G", "x8hori2B")

trainAdjChop[77] <- (rowMeans(trainSub[, c(200:209)*3 + 1]) +
                       rowMeans(trainSub[, c(220:229)*3 + 1]) +
                       rowMeans(trainSub[, c(240:249)*3 + 1]) +
                       rowMeans(trainSub[, c(260:269)*3 + 1]) +
                       rowMeans(trainSub[, c(280:289)*3 + 1]))/5
trainAdjChop[78] <- (rowMeans(trainSub[, c(200:209)*3 + 2]) +
                       rowMeans(trainSub[, c(220:229)*3 + 2]) +
                       rowMeans(trainSub[, c(240:249)*3 + 2]) +
                       rowMeans(trainSub[, c(260:269)*3 + 2]) +
                       rowMeans(trainSub[, c(280:289)*3 + 2]))/5
trainAdjChop[79] <- (rowMeans(trainSub[, c(200:209)*3 + 3]) +
                       rowMeans(trainSub[, c(220:229)*3 + 3]) +
                       rowMeans(trainSub[, c(240:249)*3 + 3]) +
                       rowMeans(trainSub[, c(260:269)*3 + 3]) +
                       rowMeans(trainSub[, c(280:289)*3 + 3]))/5
colnames(trainAdjChop)[77:79] <- c("x8hori3R", "x8hori3G", "x8hori3B")                    

trainAdjChop[80] <- (rowMeans(trainSub[, c(300:309)*3 + 1]) +
                       rowMeans(trainSub[, c(320:329)*3 + 1]) +
                       rowMeans(trainSub[, c(340:349)*3 + 1]) +
                       rowMeans(trainSub[, c(360:369)*3 + 1]) +
                       rowMeans(trainSub[, c(380:389)*3 + 1]))/5
trainAdjChop[81] <- (rowMeans(trainSub[, c(300:309)*3 + 2]) +
                       rowMeans(trainSub[, c(320:329)*3 + 2]) +
                       rowMeans(trainSub[, c(340:349)*3 + 2]) +
                       rowMeans(trainSub[, c(360:369)*3 + 2]) +
                       rowMeans(trainSub[, c(380:389)*3 + 2]))/5
trainAdjChop[82] <- (rowMeans(trainSub[, c(300:309)*3 + 3]) +
                       rowMeans(trainSub[, c(320:329)*3 + 3]) +
                       rowMeans(trainSub[, c(340:349)*3 + 3]) +
                       rowMeans(trainSub[, c(360:369)*3 + 3]) +
                       rowMeans(trainSub[, c(380:389)*3 + 3]))/5
colnames(trainAdjChop)[80:82] <- c("x8hori4R", "x8hori4G", "x8hori4B")

trainAdjChop[83] <- (rowMeans(trainSub[, c(10:19)*3 + 1]) +
                       rowMeans(trainSub[, c(30:39)*3 + 1]) +
                       rowMeans(trainSub[, c(50:59)*3 + 1]) +
                       rowMeans(trainSub[, c(70:79)*3 + 1]) +
                       rowMeans(trainSub[, c(90:99)*3 + 1]))/5
trainAdjChop[84] <- (rowMeans(trainSub[, c(10:19)*3 + 2]) +
                       rowMeans(trainSub[, c(30:39)*3 + 2]) +
                       rowMeans(trainSub[, c(50:59)*3 + 2]) +
                       rowMeans(trainSub[, c(70:79)*3 + 2]) +
                       rowMeans(trainSub[, c(90:99)*3 + 2]))/5
trainAdjChop[85] <- (rowMeans(trainSub[, c(10:19)*3 + 3]) +
                       rowMeans(trainSub[, c(30:39)*3 + 3]) +
                       rowMeans(trainSub[, c(50:59)*3 + 3]) +
                       rowMeans(trainSub[, c(70:79)*3 + 3]) +
                       rowMeans(trainSub[, c(90:99)*3 + 3]))/5
colnames(trainAdjChop)[83:85] <- c("x8hori5R", "x8hori5G", "x8hori5B")

trainAdjChop[86] <- (rowMeans(trainSub[, c(110:119)*3 + 1]) +
                       rowMeans(trainSub[, c(130:139)*3 + 1]) +
                       rowMeans(trainSub[, c(150:159)*3 + 1]) +
                       rowMeans(trainSub[, c(170:179)*3 + 1]) +
                       rowMeans(trainSub[, c(190:199)*3 + 1]))/5
trainAdjChop[87] <- (rowMeans(trainSub[, c(110:119)*3 + 2]) +
                       rowMeans(trainSub[, c(130:139)*3 + 2]) +
                       rowMeans(trainSub[, c(150:159)*3 + 2]) +
                       rowMeans(trainSub[, c(170:179)*3 + 2]) +
                       rowMeans(trainSub[, c(190:199)*3 + 2]))/5
trainAdjChop[88] <- (rowMeans(trainSub[, c(110:119)*3 + 3]) +
                       rowMeans(trainSub[, c(130:139)*3 + 3]) +
                       rowMeans(trainSub[, c(150:159)*3 + 3]) +
                       rowMeans(trainSub[, c(170:179)*3 + 3]) +
                       rowMeans(trainSub[, c(190:199)*3 + 3]))/5
colnames(trainAdjChop)[86:88] <- c("x8hori6R", "x8hori6G", "x8hori6B")

trainAdjChop[89] <- (rowMeans(trainSub[, c(210:219)*3 + 1]) +
                       rowMeans(trainSub[, c(230:239)*3 + 1]) +
                       rowMeans(trainSub[, c(250:259)*3 + 1]) +
                       rowMeans(trainSub[, c(270:279)*3 + 1]) +
                       rowMeans(trainSub[, c(290:299)*3 + 1]))/5
trainAdjChop[90] <- (rowMeans(trainSub[, c(210:219)*3 + 2]) +
                       rowMeans(trainSub[, c(230:239)*3 + 2]) +
                       rowMeans(trainSub[, c(250:259)*3 + 2]) +
                       rowMeans(trainSub[, c(270:279)*3 + 2]) +
                       rowMeans(trainSub[, c(290:299)*3 + 2]))/5
trainAdjChop[91] <- (rowMeans(trainSub[, c(210:219)*3 + 3]) +
                       rowMeans(trainSub[, c(230:239)*3 + 3]) +
                       rowMeans(trainSub[, c(250:259)*3 + 3]) +
                       rowMeans(trainSub[, c(270:279)*3 + 3]) +
                       rowMeans(trainSub[, c(290:299)*3 + 3]))/5
colnames(trainAdjChop)[89:91] <- c("x8hori7R", "x8hori7G", "x8hori7B")

trainAdjChop[92] <- (rowMeans(trainSub[, c(310:319)*3 + 1]) +
                       rowMeans(trainSub[, c(330:339)*3 + 1]) +
                       rowMeans(trainSub[, c(350:359)*3 + 1]) +
                       rowMeans(trainSub[, c(370:379)*3 + 1]) +
                       rowMeans(trainSub[, c(390:399)*3 + 1]))/5
trainAdjChop[93] <- (rowMeans(trainSub[, c(310:319)*3 + 2]) +
                       rowMeans(trainSub[, c(330:339)*3 + 2]) +
                       rowMeans(trainSub[, c(350:359)*3 + 2]) +
                       rowMeans(trainSub[, c(370:379)*3 + 2]) +
                       rowMeans(trainSub[, c(390:399)*3 + 2]))/5
trainAdjChop[94] <- (rowMeans(trainSub[, c(310:319)*3 + 3]) +
                       rowMeans(trainSub[, c(330:339)*3 + 3]) +
                       rowMeans(trainSub[, c(350:359)*3 + 3]) +
                       rowMeans(trainSub[, c(370:379)*3 + 3]) +
                       rowMeans(trainSub[, c(390:399)*3 + 3]))/5
colnames(trainAdjChop)[92:94] <- c("x8hori8R", "x8hori8G", "x8hori8B")

trainAdjChop[95] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                       rowMeans(trainSub[, c(20:24)*3 + 1]) +
                       rowMeans(trainSub[, c(40:44)*3 + 1]) +
                       rowMeans(trainSub[, c(60:64)*3 + 1]) +
                       rowMeans(trainSub[, c(80:84)*3 + 1]) +
                       rowMeans(trainSub[, c(300:304)*3 + 1]) +
                       rowMeans(trainSub[, c(320:324)*3 + 1]) +
                       rowMeans(trainSub[, c(340:344)*3 + 1]) +
                       rowMeans(trainSub[, c(360:364)*3 + 1]) +
                       rowMeans(trainSub[, c(380:384)*3 + 1]) +
                       rowMeans(trainSub[, c(15:19)*3 + 1]) +
                       rowMeans(trainSub[, c(35:39)*3 + 1]) +
                       rowMeans(trainSub[, c(55:59)*3 + 1]) +
                       rowMeans(trainSub[, c(75:79)*3 + 1]) +
                       rowMeans(trainSub[, c(95:99)*3 + 1]) +
                       rowMeans(trainSub[, c(315:319)*3 + 1]) +
                       rowMeans(trainSub[, c(335:339)*3 + 1]) +
                       rowMeans(trainSub[, c(355:359)*3 + 1]) +
                       rowMeans(trainSub[, c(375:379)*3 + 1]) +
                       rowMeans(trainSub[, c(395:399)*3 + 1]))/20
trainAdjChop[96] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                       rowMeans(trainSub[, c(20:24)*3 + 2]) +
                       rowMeans(trainSub[, c(40:44)*3 + 2]) +
                       rowMeans(trainSub[, c(60:64)*3 + 2]) +
                       rowMeans(trainSub[, c(80:84)*3 + 2]) +
                       rowMeans(trainSub[, c(300:304)*3 + 2]) +
                       rowMeans(trainSub[, c(320:324)*3 + 2]) +
                       rowMeans(trainSub[, c(340:344)*3 + 2]) +
                       rowMeans(trainSub[, c(360:364)*3 + 2]) +
                       rowMeans(trainSub[, c(380:384)*3 + 2]) +
                       rowMeans(trainSub[, c(15:19)*3 + 2]) +
                       rowMeans(trainSub[, c(35:39)*3 + 2]) +
                       rowMeans(trainSub[, c(55:59)*3 + 2]) +
                       rowMeans(trainSub[, c(75:79)*3 + 2]) +
                       rowMeans(trainSub[, c(95:99)*3 + 2]) +
                       rowMeans(trainSub[, c(315:319)*3 + 2]) +
                       rowMeans(trainSub[, c(335:339)*3 + 2]) +
                       rowMeans(trainSub[, c(355:359)*3 + 2]) +
                       rowMeans(trainSub[, c(375:379)*3 + 2]) +
                       rowMeans(trainSub[, c(395:399)*3 + 2]))/20 
trainAdjChop[97] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                       rowMeans(trainSub[, c(20:24)*3 + 3]) +
                       rowMeans(trainSub[, c(40:44)*3 + 3]) +
                       rowMeans(trainSub[, c(60:64)*3 + 3]) +
                       rowMeans(trainSub[, c(80:84)*3 + 3]) +
                       rowMeans(trainSub[, c(300:304)*3 + 3]) +
                       rowMeans(trainSub[, c(320:324)*3 + 3]) +
                       rowMeans(trainSub[, c(340:344)*3 + 3]) +
                       rowMeans(trainSub[, c(360:364)*3 + 3]) +
                       rowMeans(trainSub[, c(380:384)*3 + 3]) +
                       rowMeans(trainSub[, c(15:19)*3 + 3]) +
                       rowMeans(trainSub[, c(35:39)*3 + 3]) +
                       rowMeans(trainSub[, c(55:59)*3 + 3]) +
                       rowMeans(trainSub[, c(75:79)*3 + 3]) +
                       rowMeans(trainSub[, c(95:99)*3 + 3]) +
                       rowMeans(trainSub[, c(315:319)*3 + 3]) +
                       rowMeans(trainSub[, c(335:339)*3 + 3]) +
                       rowMeans(trainSub[, c(355:359)*3 + 3]) +
                       rowMeans(trainSub[, c(375:379)*3 + 3]) +
                       rowMeans(trainSub[, c(395:399)*3 + 3]))/20
colnames(trainAdjChop)[95:97] <- c("cornersR", "cornersG", "cornersB")

trainAdjChop[98] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                       rowMeans(trainSub[, c(20:24)*3 + 1]) +
                       rowMeans(trainSub[, c(40:44)*3 + 1]) +
                       rowMeans(trainSub[, c(60:64)*3 + 1]) +
                       rowMeans(trainSub[, c(80:84)*3 + 1]))/5
trainAdjChop[99] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                       rowMeans(trainSub[, c(20:24)*3 + 2]) +
                       rowMeans(trainSub[, c(40:44)*3 + 2]) +
                       rowMeans(trainSub[, c(60:64)*3 + 2]) +
                       rowMeans(trainSub[, c(80:84)*3 + 2]))/5
trainAdjChop[100] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                        rowMeans(trainSub[, c(20:24)*3 + 3]) +
                        rowMeans(trainSub[, c(40:44)*3 + 3]) +
                        rowMeans(trainSub[, c(60:64)*3 + 3]) +
                        rowMeans(trainSub[, c(80:84)*3 + 3]))/5
colnames(trainAdjChop)[98:100] <- c("TLcornerR", "TLcornerG", "TLcornerB")

trainAdjChop[101] <- (rowMeans(trainSub[, c(300:304)*3 + 1]) +
                        rowMeans(trainSub[, c(320:324)*3 + 1]) +
                        rowMeans(trainSub[, c(340:344)*3 + 1]) +
                        rowMeans(trainSub[, c(360:364)*3 + 1]) +
                        rowMeans(trainSub[, c(380:384)*3 + 1]))/5
trainAdjChop[102] <- (rowMeans(trainSub[, c(300:304)*3 + 2]) +
                        rowMeans(trainSub[, c(320:324)*3 + 2]) +
                        rowMeans(trainSub[, c(340:344)*3 + 2]) +
                        rowMeans(trainSub[, c(360:364)*3 + 2]) +
                        rowMeans(trainSub[, c(380:384)*3 + 2]))/5
trainAdjChop[103] <- (rowMeans(trainSub[, c(300:304)*3 + 3]) +
                        rowMeans(trainSub[, c(320:324)*3 + 3]) +
                        rowMeans(trainSub[, c(340:344)*3 + 3]) +
                        rowMeans(trainSub[, c(360:364)*3 + 3]) +
                        rowMeans(trainSub[, c(380:384)*3 + 3]))/5
colnames(trainAdjChop)[101:103] <- c("TRcornerR", "TRcornerG", "TRcornerB")

trainAdjChop[104] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                        rowMeans(trainSub[, c(35:39)*3 + 1]) +
                        rowMeans(trainSub[, c(55:59)*3 + 1]) +
                        rowMeans(trainSub[, c(75:79)*3 + 1]) +
                        rowMeans(trainSub[, c(95:99)*3 + 1]))/5
trainAdjChop[105] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                        rowMeans(trainSub[, c(35:39)*3 + 2]) +
                        rowMeans(trainSub[, c(55:59)*3 + 2]) +
                        rowMeans(trainSub[, c(75:79)*3 + 2]) +
                        rowMeans(trainSub[, c(95:99)*3 + 2]))/5
trainAdjChop[106] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                        rowMeans(trainSub[, c(35:39)*3 + 3]) +
                        rowMeans(trainSub[, c(55:59)*3 + 3]) +
                        rowMeans(trainSub[, c(75:79)*3 + 3]) +
                        rowMeans(trainSub[, c(95:99)*3 + 3]))/5
colnames(trainAdjChop)[104:106] <- c("BLcornerR", "BLcornerG", "BLcornerB")

trainAdjChop[107] <- (rowMeans(trainSub[, c(315:319)*3 + 1]) +
                        rowMeans(trainSub[, c(335:339)*3 + 1]) +
                        rowMeans(trainSub[, c(355:359)*3 + 1]) +
                        rowMeans(trainSub[, c(375:379)*3 + 1]) +
                        rowMeans(trainSub[, c(395:399)*3 + 1]))/5
trainAdjChop[108] <- (rowMeans(trainSub[, c(315:319)*3 + 2]) +
                        rowMeans(trainSub[, c(335:339)*3 + 2]) +
                        rowMeans(trainSub[, c(355:359)*3 + 2]) +
                        rowMeans(trainSub[, c(375:379)*3 + 2]) +
                        rowMeans(trainSub[, c(395:399)*3 + 2]))/5
trainAdjChop[109] <- (rowMeans(trainSub[, c(315:319)*3 + 3]) +
                        rowMeans(trainSub[, c(335:339)*3 + 3]) +
                        rowMeans(trainSub[, c(355:359)*3 + 3]) +
                        rowMeans(trainSub[, c(375:379)*3 + 3]) +
                        rowMeans(trainSub[, c(395:399)*3 + 3]))/5
colnames(trainAdjChop)[107:109] <- c("BRcornerR", "BRcornerG", "BRcornerB")

trainAdjChop[110] <- (rowMeans(trainSub[, c(105:114)*3 + 1]) +
                        rowMeans(trainSub[, c(125:134)*3 + 1]) +
                        rowMeans(trainSub[, c(145:154)*3 + 1]) +
                        rowMeans(trainSub[, c(165:174)*3 + 1]) +
                        rowMeans(trainSub[, c(185:194)*3 + 1]) +
                        rowMeans(trainSub[, c(205:214)*3 + 1]) +
                        rowMeans(trainSub[, c(225:234)*3 + 1]) +
                        rowMeans(trainSub[, c(245:254)*3 + 1]) +
                        rowMeans(trainSub[, c(265:274)*3 + 1]) +
                        rowMeans(trainSub[, c(285:294)*3 + 1]))/10
trainAdjChop[111] <- (rowMeans(trainSub[, c(105:114)*3 + 2]) +
                        rowMeans(trainSub[, c(125:134)*3 + 2]) +
                        rowMeans(trainSub[, c(145:154)*3 + 2]) +
                        rowMeans(trainSub[, c(165:174)*3 + 2]) +
                        rowMeans(trainSub[, c(185:194)*3 + 2]) +
                        rowMeans(trainSub[, c(205:214)*3 + 2]) +
                        rowMeans(trainSub[, c(225:234)*3 + 2]) +
                        rowMeans(trainSub[, c(245:254)*3 + 2]) +
                        rowMeans(trainSub[, c(265:274)*3 + 2]) +
                        rowMeans(trainSub[, c(285:294)*3 + 2]))/10
trainAdjChop[112] <- (rowMeans(trainSub[, c(105:114)*3 + 3]) +
                        rowMeans(trainSub[, c(125:134)*3 + 3]) +
                        rowMeans(trainSub[, c(145:154)*3 + 3]) +
                        rowMeans(trainSub[, c(165:174)*3 + 3]) +
                        rowMeans(trainSub[, c(185:194)*3 + 3]) +
                        rowMeans(trainSub[, c(205:214)*3 + 3]) +
                        rowMeans(trainSub[, c(225:234)*3 + 3]) +
                        rowMeans(trainSub[, c(245:254)*3 + 3]) +
                        rowMeans(trainSub[, c(265:274)*3 + 3]) +
                        rowMeans(trainSub[, c(285:294)*3 + 3]))/10
colnames(trainAdjChop)[110:112] <- c("centerMainR", "centerMainG", "centerMainB")

trainAdjChop[113] <- (rowMeans(trainSub[, c(105:109)*3 + 1]) +
                        rowMeans(trainSub[, c(125:129)*3 + 1]) +
                        rowMeans(trainSub[, c(145:149)*3 + 1]) +
                        rowMeans(trainSub[, c(165:169)*3 + 1]) +
                        rowMeans(trainSub[, c(185:189)*3 + 1]))/5
trainAdjChop[114] <- (rowMeans(trainSub[, c(105:109)*3 + 2]) +
                        rowMeans(trainSub[, c(125:129)*3 + 2]) +
                        rowMeans(trainSub[, c(145:149)*3 + 2]) +
                        rowMeans(trainSub[, c(165:169)*3 + 2]) +
                        rowMeans(trainSub[, c(185:189)*3 + 2]))/5
trainAdjChop[115] <- (rowMeans(trainSub[, c(105:109)*3 + 3]) +
                        rowMeans(trainSub[, c(125:129)*3 + 3]) +
                        rowMeans(trainSub[, c(145:149)*3 + 3]) +
                        rowMeans(trainSub[, c(165:169)*3 + 3]) +
                        rowMeans(trainSub[, c(185:189)*3 + 3]))/5
colnames(trainAdjChop)[113:115] <- c("TLcenterR", "TLcenterG", "TLcenterB")

trainAdjChop[116] <- (rowMeans(trainSub[, c(110:114)*3 + 1]) +
                        rowMeans(trainSub[, c(130:134)*3 + 1]) +
                        rowMeans(trainSub[, c(150:154)*3 + 1]) +
                        rowMeans(trainSub[, c(170:174)*3 + 1]) +
                        rowMeans(trainSub[, c(190:194)*3 + 1]))/5
trainAdjChop[117] <- (rowMeans(trainSub[, c(110:114)*3 + 2]) +
                        rowMeans(trainSub[, c(130:134)*3 + 2]) +
                        rowMeans(trainSub[, c(150:154)*3 + 2]) +
                        rowMeans(trainSub[, c(170:174)*3 + 2]) +
                        rowMeans(trainSub[, c(190:194)*3 + 2]))/5
trainAdjChop[118] <- (rowMeans(trainSub[, c(110:114)*3 + 3]) +
                        rowMeans(trainSub[, c(130:134)*3 + 3]) +
                        rowMeans(trainSub[, c(150:154)*3 + 3]) +
                        rowMeans(trainSub[, c(170:174)*3 + 3]) +
                        rowMeans(trainSub[, c(190:194)*3 + 3]))/5
colnames(trainAdjChop)[116:118] <- c("TRcenterR", "TRcenterG", "TRcenterB")

trainAdjChop[119] <- (rowMeans(trainSub[, c(205:209)*3 + 1]) +
                        rowMeans(trainSub[, c(225:229)*3 + 1]) +
                        rowMeans(trainSub[, c(245:249)*3 + 1]) +
                        rowMeans(trainSub[, c(265:269)*3 + 1]) +
                        rowMeans(trainSub[, c(285:289)*3 + 1]))/5
trainAdjChop[120] <- (rowMeans(trainSub[, c(205:209)*3 + 2]) +
                        rowMeans(trainSub[, c(225:229)*3 + 2]) +
                        rowMeans(trainSub[, c(245:249)*3 + 2]) +
                        rowMeans(trainSub[, c(265:269)*3 + 2]) +
                        rowMeans(trainSub[, c(285:289)*3 + 2]))/5
trainAdjChop[121] <- (rowMeans(trainSub[, c(205:209)*3 + 3]) +
                        rowMeans(trainSub[, c(225:229)*3 + 3]) +
                        rowMeans(trainSub[, c(245:249)*3 + 3]) +
                        rowMeans(trainSub[, c(265:269)*3 + 3]) +
                        rowMeans(trainSub[, c(285:289)*3 + 3]))/5
colnames(trainAdjChop)[119:121] <- c("BLcenterR", "BLcenterG", "BLcenterB")

trainAdjChop[122] <- (rowMeans(trainSub[, c(210:214)*3 + 1]) +
                        rowMeans(trainSub[, c(230:234)*3 + 1]) +
                        rowMeans(trainSub[, c(250:254)*3 + 1]) +
                        rowMeans(trainSub[, c(270:274)*3 + 1]) +
                        rowMeans(trainSub[, c(290:294)*3 + 1]))/5
trainAdjChop[123] <- (rowMeans(trainSub[, c(210:214)*3 + 2]) +
                        rowMeans(trainSub[, c(230:234)*3 + 2]) +
                        rowMeans(trainSub[, c(250:254)*3 + 2]) +
                        rowMeans(trainSub[, c(270:274)*3 + 2]) +
                        rowMeans(trainSub[, c(290:294)*3 + 2]))/5
trainAdjChop[124] <- (rowMeans(trainSub[, c(210:214)*3 + 3]) +
                        rowMeans(trainSub[, c(230:234)*3 + 3]) +
                        rowMeans(trainSub[, c(250:254)*3 + 3]) +
                        rowMeans(trainSub[, c(270:274)*3 + 3]) +
                        rowMeans(trainSub[, c(290:294)*3 + 3]))/5
colnames(trainAdjChop)[122:124] <- c("BRcenterR", "BRcenterG", "BRcenterB")

for(i in 0:40){
trainAdjChop[125 + i] <- (trainAdjChop[(i*3)+2] + trainAdjChop[(i*3)+3] + trainAdjChop[(i*3)+4]) / 3
}

################################
################################ Chopping up the test data
################################

extraColTestData <- cbind(class = 'blah', testData)

trainChopTest <- extraColTestData[1:4]

trainSub <- testData

trainChopTest[2] <- rowMeans(trainSub[, c(0:399)*3 + 1])
trainChopTest[3] <- rowMeans(trainSub[, c(0:399)*3 + 2])
trainChopTest[4] <- rowMeans(trainSub[, c(0:399)*3 + 3])
colnames(trainChopTest)[2:4] <- c("wholeR", "wholeG", "wholeB")

trainChopTest[5] <- rowMeans(trainSub[, c(0:199)*3 + 1])
trainChopTest[6] <- rowMeans(trainSub[, c(0:199)*3 + 2])
trainChopTest[7] <- rowMeans(trainSub[, c(0:199)*3 + 3])
colnames(trainChopTest)[5:7] <- c("tophfR", "tophfG", "tophfB")

trainChopTest[8] <- rowMeans(trainSub[, c(200:399)*3 + 1])
trainChopTest[9] <- rowMeans(trainSub[, c(200:399)*3 + 2])
trainChopTest[10] <- rowMeans(trainSub[, c(200:399)*3 + 3])
colnames(trainChopTest)[8:10] <- c("btmhfR", "btmhfG", "btmhfB")

trainChopTest[11] <- (rowMeans(trainSub[, c(0:9)*3 + 1]) +
                        rowMeans(trainSub[, c(20:29)*3 + 1]) +
                        rowMeans(trainSub[, c(40:49)*3 + 1]) +
                        rowMeans(trainSub[, c(60:69)*3 + 1]) +
                        rowMeans(trainSub[, c(80:89)*3 + 1]) +
                        rowMeans(trainSub[, c(100:109)*3 + 1]) +
                        rowMeans(trainSub[, c(120:129)*3 + 1]) +
                        rowMeans(trainSub[, c(140:149)*3 + 1]) +
                        rowMeans(trainSub[, c(160:169)*3 + 1]) +
                        rowMeans(trainSub[, c(180:189)*3 + 1]) +
                        rowMeans(trainSub[, c(200:209)*3 + 1]) +
                        rowMeans(trainSub[, c(220:229)*3 + 1]) +
                        rowMeans(trainSub[, c(240:249)*3 + 1]) +
                        rowMeans(trainSub[, c(260:269)*3 + 1]) +
                        rowMeans(trainSub[, c(280:289)*3 + 1]) +
                        rowMeans(trainSub[, c(300:309)*3 + 1]) +
                        rowMeans(trainSub[, c(320:329)*3 + 1]) +
                        rowMeans(trainSub[, c(340:349)*3 + 1]) +
                        rowMeans(trainSub[, c(360:369)*3 + 1]) +
                        rowMeans(trainSub[, c(380:389)*3 + 1]))/20
trainChopTest[12] <- (rowMeans(trainSub[, c(0:9)*3 + 2]) +
                        rowMeans(trainSub[, c(20:29)*3 + 2]) +
                        rowMeans(trainSub[, c(40:49)*3 + 2]) +
                        rowMeans(trainSub[, c(60:69)*3 + 2]) +
                        rowMeans(trainSub[, c(80:89)*3 + 2]) +
                        rowMeans(trainSub[, c(100:109)*3 + 2]) +
                        rowMeans(trainSub[, c(120:129)*3 + 2]) +
                        rowMeans(trainSub[, c(140:149)*3 + 2]) +
                        rowMeans(trainSub[, c(160:169)*3 + 2]) +
                        rowMeans(trainSub[, c(180:189)*3 + 2]) +
                        rowMeans(trainSub[, c(200:209)*3 + 2]) +
                        rowMeans(trainSub[, c(220:229)*3 + 2]) +
                        rowMeans(trainSub[, c(240:249)*3 + 2]) +
                        rowMeans(trainSub[, c(260:269)*3 + 2]) +
                        rowMeans(trainSub[, c(280:289)*3 + 2]) +
                        rowMeans(trainSub[, c(300:309)*3 + 2]) +
                        rowMeans(trainSub[, c(320:329)*3 + 2]) +
                        rowMeans(trainSub[, c(340:349)*3 + 2]) +
                        rowMeans(trainSub[, c(360:369)*3 + 2]) +
                        rowMeans(trainSub[, c(380:389)*3 + 2]))/20
trainChopTest[13] <- (rowMeans(trainSub[, c(0:9)*3 + 3]) +
                        rowMeans(trainSub[, c(20:29)*3 + 3]) +
                        rowMeans(trainSub[, c(40:49)*3 + 3]) +
                        rowMeans(trainSub[, c(60:69)*3 + 3]) +
                        rowMeans(trainSub[, c(80:89)*3 + 3]) +
                        rowMeans(trainSub[, c(100:109)*3 + 3]) +
                        rowMeans(trainSub[, c(120:129)*3 + 3]) +
                        rowMeans(trainSub[, c(140:149)*3 + 3]) +
                        rowMeans(trainSub[, c(160:169)*3 + 3]) +
                        rowMeans(trainSub[, c(180:189)*3 + 3]) +
                        rowMeans(trainSub[, c(200:209)*3 + 3]) +
                        rowMeans(trainSub[, c(220:229)*3 + 3]) +
                        rowMeans(trainSub[, c(240:249)*3 + 3]) +
                        rowMeans(trainSub[, c(260:269)*3 + 3]) +
                        rowMeans(trainSub[, c(280:289)*3 + 3]) +
                        rowMeans(trainSub[, c(300:309)*3 + 3]) +
                        rowMeans(trainSub[, c(320:329)*3 + 3]) +
                        rowMeans(trainSub[, c(340:349)*3 + 3]) +
                        rowMeans(trainSub[, c(360:369)*3 + 3]) +
                        rowMeans(trainSub[, c(380:389)*3 + 3]))/20
colnames(trainChopTest)[11:13] <- c("lfthfR", "lfthfG", "lfthfB")

trainChopTest[14] <- (rowMeans(trainSub[, c(10:19)*3 + 1]) +
                        rowMeans(trainSub[, c(30:39)*3 + 1]) +
                        rowMeans(trainSub[, c(50:59)*3 + 1]) +
                        rowMeans(trainSub[, c(70:79)*3 + 1]) +
                        rowMeans(trainSub[, c(90:99)*3 + 1]) +
                        rowMeans(trainSub[, c(110:119)*3 + 1]) +
                        rowMeans(trainSub[, c(130:139)*3 + 1]) +
                        rowMeans(trainSub[, c(150:159)*3 + 1]) +
                        rowMeans(trainSub[, c(170:179)*3 + 1]) +
                        rowMeans(trainSub[, c(190:199)*3 + 1]) +
                        rowMeans(trainSub[, c(210:219)*3 + 1]) +
                        rowMeans(trainSub[, c(230:239)*3 + 1]) +
                        rowMeans(trainSub[, c(250:259)*3 + 1]) +
                        rowMeans(trainSub[, c(270:279)*3 + 1]) +
                        rowMeans(trainSub[, c(290:299)*3 + 1]) +
                        rowMeans(trainSub[, c(310:319)*3 + 1]) +
                        rowMeans(trainSub[, c(330:339)*3 + 1]) +
                        rowMeans(trainSub[, c(350:359)*3 + 1]) +
                        rowMeans(trainSub[, c(370:379)*3 + 1]) +
                        rowMeans(trainSub[, c(390:399)*3 + 1]))/20
trainChopTest[15] <- (rowMeans(trainSub[, c(10:19)*3 + 2]) +
                        rowMeans(trainSub[, c(30:39)*3 + 2]) +
                        rowMeans(trainSub[, c(50:59)*3 + 2]) +
                        rowMeans(trainSub[, c(70:79)*3 + 2]) +
                        rowMeans(trainSub[, c(90:99)*3 + 2]) +
                        rowMeans(trainSub[, c(110:119)*3 + 2]) +
                        rowMeans(trainSub[, c(130:139)*3 + 2]) +
                        rowMeans(trainSub[, c(150:159)*3 + 2]) +
                        rowMeans(trainSub[, c(170:179)*3 + 2]) +
                        rowMeans(trainSub[, c(190:199)*3 + 2]) +
                        rowMeans(trainSub[, c(210:219)*3 + 2]) +
                        rowMeans(trainSub[, c(230:239)*3 + 2]) +
                        rowMeans(trainSub[, c(250:259)*3 + 2]) +
                        rowMeans(trainSub[, c(270:279)*3 + 2]) +
                        rowMeans(trainSub[, c(290:299)*3 + 2]) +
                        rowMeans(trainSub[, c(310:319)*3 + 2]) +
                        rowMeans(trainSub[, c(330:339)*3 + 2]) +
                        rowMeans(trainSub[, c(350:359)*3 + 2]) +
                        rowMeans(trainSub[, c(370:379)*3 + 2]) +
                        rowMeans(trainSub[, c(390:399)*3 + 2]))/20
trainChopTest[16] <- (rowMeans(trainSub[, c(10:19)*3 + 3]) +
                        rowMeans(trainSub[, c(30:39)*3 + 3]) +
                        rowMeans(trainSub[, c(50:59)*3 + 3]) +
                        rowMeans(trainSub[, c(70:79)*3 + 3]) +
                        rowMeans(trainSub[, c(90:99)*3 + 3]) +
                        rowMeans(trainSub[, c(110:119)*3 + 3]) +
                        rowMeans(trainSub[, c(130:139)*3 + 3]) +
                        rowMeans(trainSub[, c(150:159)*3 + 3]) +
                        rowMeans(trainSub[, c(170:179)*3 + 3]) +
                        rowMeans(trainSub[, c(190:199)*3 + 3]) +
                        rowMeans(trainSub[, c(210:219)*3 + 3]) +
                        rowMeans(trainSub[, c(230:239)*3 + 3]) +
                        rowMeans(trainSub[, c(250:259)*3 + 3]) +
                        rowMeans(trainSub[, c(270:279)*3 + 3]) +
                        rowMeans(trainSub[, c(290:299)*3 + 3]) +
                        rowMeans(trainSub[, c(310:319)*3 + 3]) +
                        rowMeans(trainSub[, c(330:339)*3 + 3]) +
                        rowMeans(trainSub[, c(350:359)*3 + 3]) +
                        rowMeans(trainSub[, c(370:379)*3 + 3]) +
                        rowMeans(trainSub[, c(390:399)*3 + 3]))/20
colnames(trainChopTest)[14:16] <- c("rthfR", "rthfG", "rthfB")

trainChopTest[17] <- rowMeans(trainSub[, c(0:299)*3 + 1])
trainChopTest[18] <- rowMeans(trainSub[, c(0:299)*3 + 2])
trainChopTest[19] <- rowMeans(trainSub[, c(0:299)*3 + 3])
colnames(trainChopTest)[17:19] <- c("tp3.4R", "tp3.4G", "tp3.4B")

trainChopTest[20] <- rowMeans(trainSub[, c(100:399)*3 + 1])
trainChopTest[21] <- rowMeans(trainSub[, c(100:399)*3 + 2])
trainChopTest[22] <- rowMeans(trainSub[, c(100:399)*3 + 3])
colnames(trainChopTest)[20:22] <- c("btm3.4R", "btm3.4G", "btm3.4B")

trainChopTest[23] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                        rowMeans(trainSub[, c(20:24)*3 + 1]) +
                        rowMeans(trainSub[, c(40:44)*3 + 1]) +
                        rowMeans(trainSub[, c(60:64)*3 + 1]) +
                        rowMeans(trainSub[, c(80:84)*3 + 1]) +
                        rowMeans(trainSub[, c(100:104)*3 + 1]) +
                        rowMeans(trainSub[, c(120:124)*3 + 1]) +
                        rowMeans(trainSub[, c(140:144)*3 + 1]) +
                        rowMeans(trainSub[, c(160:164)*3 + 1]) +
                        rowMeans(trainSub[, c(180:184)*3 + 1]) +
                        rowMeans(trainSub[, c(200:204)*3 + 1]) +
                        rowMeans(trainSub[, c(220:224)*3 + 1]) +
                        rowMeans(trainSub[, c(240:244)*3 + 1]) +
                        rowMeans(trainSub[, c(260:264)*3 + 1]) +
                        rowMeans(trainSub[, c(280:284)*3 + 1]) +
                        rowMeans(trainSub[, c(300:304)*3 + 1]) +
                        rowMeans(trainSub[, c(320:324)*3 + 1]) +
                        rowMeans(trainSub[, c(340:344)*3 + 1]) +
                        rowMeans(trainSub[, c(360:364)*3 + 1]) +
                        rowMeans(trainSub[, c(380:384)*3 + 1]))/20
trainChopTest[24] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                        rowMeans(trainSub[, c(20:24)*3 + 2]) +
                        rowMeans(trainSub[, c(40:44)*3 + 2]) +
                        rowMeans(trainSub[, c(60:64)*3 + 2]) +
                        rowMeans(trainSub[, c(80:84)*3 + 2]) +
                        rowMeans(trainSub[, c(100:104)*3 + 2]) +
                        rowMeans(trainSub[, c(120:124)*3 + 2]) +
                        rowMeans(trainSub[, c(140:144)*3 + 2]) +
                        rowMeans(trainSub[, c(160:164)*3 + 2]) +
                        rowMeans(trainSub[, c(180:184)*3 + 2]) +
                        rowMeans(trainSub[, c(200:204)*3 + 2]) +
                        rowMeans(trainSub[, c(220:224)*3 + 2]) +
                        rowMeans(trainSub[, c(240:244)*3 + 2]) +
                        rowMeans(trainSub[, c(260:264)*3 + 2]) +
                        rowMeans(trainSub[, c(280:284)*3 + 2]) +
                        rowMeans(trainSub[, c(300:304)*3 + 2]) +
                        rowMeans(trainSub[, c(320:324)*3 + 2]) +
                        rowMeans(trainSub[, c(340:344)*3 + 2]) +
                        rowMeans(trainSub[, c(360:364)*3 + 2]) +
                        rowMeans(trainSub[, c(380:384)*3 + 2]))/20
trainChopTest[25] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                        rowMeans(trainSub[, c(20:24)*3 + 3]) +
                        rowMeans(trainSub[, c(40:44)*3 + 3]) +
                        rowMeans(trainSub[, c(60:64)*3 + 3]) +
                        rowMeans(trainSub[, c(80:84)*3 + 3]) +
                        rowMeans(trainSub[, c(100:104)*3 + 3]) +
                        rowMeans(trainSub[, c(120:124)*3 + 3]) +
                        rowMeans(trainSub[, c(140:144)*3 + 3]) +
                        rowMeans(trainSub[, c(160:164)*3 + 3]) +
                        rowMeans(trainSub[, c(180:184)*3 + 3]) +
                        rowMeans(trainSub[, c(200:204)*3 + 3]) +
                        rowMeans(trainSub[, c(220:224)*3 + 3]) +
                        rowMeans(trainSub[, c(240:244)*3 + 3]) +
                        rowMeans(trainSub[, c(260:264)*3 + 3]) +
                        rowMeans(trainSub[, c(280:284)*3 + 3]) +
                        rowMeans(trainSub[, c(300:304)*3 + 3]) +
                        rowMeans(trainSub[, c(320:324)*3 + 3]) +
                        rowMeans(trainSub[, c(340:344)*3 + 3]) +
                        rowMeans(trainSub[, c(360:364)*3 + 3]) +
                        rowMeans(trainSub[, c(380:384)*3 + 3]))/20
colnames(trainChopTest)[23:25] <- c("x4vert1R", "x4vert1G", "x4vert1B")

trainChopTest[26] <- (rowMeans(trainSub[, c(5:9)*3 + 1]) +
                        rowMeans(trainSub[, c(25:29)*3 + 1]) +
                        rowMeans(trainSub[, c(45:49)*3 + 1]) +
                        rowMeans(trainSub[, c(65:69)*3 + 1]) +
                        rowMeans(trainSub[, c(85:89)*3 + 1]) +
                        rowMeans(trainSub[, c(105:109)*3 + 1]) +
                        rowMeans(trainSub[, c(125:129)*3 + 1]) +
                        rowMeans(trainSub[, c(145:149)*3 + 1]) +
                        rowMeans(trainSub[, c(165:169)*3 + 1]) +
                        rowMeans(trainSub[, c(185:189)*3 + 1]) +
                        rowMeans(trainSub[, c(205:209)*3 + 1]) +
                        rowMeans(trainSub[, c(225:229)*3 + 1]) +
                        rowMeans(trainSub[, c(245:249)*3 + 1]) +
                        rowMeans(trainSub[, c(265:269)*3 + 1]) +
                        rowMeans(trainSub[, c(285:289)*3 + 1]) +
                        rowMeans(trainSub[, c(305:309)*3 + 1]) +
                        rowMeans(trainSub[, c(325:329)*3 + 1]) +
                        rowMeans(trainSub[, c(345:349)*3 + 1]) +
                        rowMeans(trainSub[, c(365:369)*3 + 1]) +
                        rowMeans(trainSub[, c(385:389)*3 + 1]))/20
trainChopTest[27] <- (rowMeans(trainSub[, c(5:9)*3 + 2]) +
                        rowMeans(trainSub[, c(25:29)*3 + 2]) +
                        rowMeans(trainSub[, c(45:49)*3 + 2]) +
                        rowMeans(trainSub[, c(65:69)*3 + 2]) +
                        rowMeans(trainSub[, c(85:89)*3 + 2]) +
                        rowMeans(trainSub[, c(105:109)*3 + 2]) +
                        rowMeans(trainSub[, c(125:129)*3 + 2]) +
                        rowMeans(trainSub[, c(145:149)*3 + 2]) +
                        rowMeans(trainSub[, c(165:169)*3 + 2]) +
                        rowMeans(trainSub[, c(185:189)*3 + 2]) +
                        rowMeans(trainSub[, c(205:209)*3 + 2]) +
                        rowMeans(trainSub[, c(225:229)*3 + 2]) +
                        rowMeans(trainSub[, c(245:249)*3 + 2]) +
                        rowMeans(trainSub[, c(265:269)*3 + 2]) +
                        rowMeans(trainSub[, c(285:289)*3 + 2]) +
                        rowMeans(trainSub[, c(305:309)*3 + 2]) +
                        rowMeans(trainSub[, c(325:329)*3 + 2]) +
                        rowMeans(trainSub[, c(345:349)*3 + 2]) +
                        rowMeans(trainSub[, c(365:369)*3 + 2]) +
                        rowMeans(trainSub[, c(385:389)*3 + 2]))/20
trainChopTest[28] <- (rowMeans(trainSub[, c(5:9)*3 + 3]) +
                        rowMeans(trainSub[, c(25:29)*3 + 3]) +
                        rowMeans(trainSub[, c(45:49)*3 + 3]) +
                        rowMeans(trainSub[, c(65:69)*3 + 3]) +
                        rowMeans(trainSub[, c(85:89)*3 + 3]) +
                        rowMeans(trainSub[, c(105:109)*3 + 3]) +
                        rowMeans(trainSub[, c(125:129)*3 + 3]) +
                        rowMeans(trainSub[, c(145:149)*3 + 3]) +
                        rowMeans(trainSub[, c(165:169)*3 + 3]) +
                        rowMeans(trainSub[, c(185:189)*3 + 3]) +
                        rowMeans(trainSub[, c(205:209)*3 + 3]) +
                        rowMeans(trainSub[, c(225:229)*3 + 3]) +
                        rowMeans(trainSub[, c(245:249)*3 + 3]) +
                        rowMeans(trainSub[, c(265:269)*3 + 3]) +
                        rowMeans(trainSub[, c(285:289)*3 + 3]) +
                        rowMeans(trainSub[, c(305:309)*3 + 3]) +
                        rowMeans(trainSub[, c(325:329)*3 + 3]) +
                        rowMeans(trainSub[, c(345:349)*3 + 3]) +
                        rowMeans(trainSub[, c(365:369)*3 + 3]) +
                        rowMeans(trainSub[, c(385:389)*3 + 3]))/20
colnames(trainChopTest)[26:28] <- c("x4vert2R", "x4vert2G", "x4vert2B")

trainChopTest[29] <- (rowMeans(trainSub[, c(10:14)*3 + 1]) +
                        rowMeans(trainSub[, c(30:34)*3 + 1]) +
                        rowMeans(trainSub[, c(50:54)*3 + 1]) +
                        rowMeans(trainSub[, c(70:74)*3 + 1]) +
                        rowMeans(trainSub[, c(90:94)*3 + 1]) +
                        rowMeans(trainSub[, c(110:114)*3 + 1]) +
                        rowMeans(trainSub[, c(130:134)*3 + 1]) +
                        rowMeans(trainSub[, c(150:154)*3 + 1]) +
                        rowMeans(trainSub[, c(170:174)*3 + 1]) +
                        rowMeans(trainSub[, c(190:194)*3 + 1]) +
                        rowMeans(trainSub[, c(210:214)*3 + 1]) +
                        rowMeans(trainSub[, c(230:234)*3 + 1]) +
                        rowMeans(trainSub[, c(250:254)*3 + 1]) +
                        rowMeans(trainSub[, c(270:274)*3 + 1]) +
                        rowMeans(trainSub[, c(290:294)*3 + 1]) +
                        rowMeans(trainSub[, c(310:314)*3 + 1]) +
                        rowMeans(trainSub[, c(330:334)*3 + 1]) +
                        rowMeans(trainSub[, c(350:354)*3 + 1]) +
                        rowMeans(trainSub[, c(370:374)*3 + 1]) +
                        rowMeans(trainSub[, c(390:394)*3 + 1]))/20
trainChopTest[30] <- (rowMeans(trainSub[, c(10:14)*3 + 2]) +
                        rowMeans(trainSub[, c(30:34)*3 + 2]) +
                        rowMeans(trainSub[, c(50:54)*3 + 2]) +
                        rowMeans(trainSub[, c(70:74)*3 + 2]) +
                        rowMeans(trainSub[, c(90:94)*3 + 2]) +
                        rowMeans(trainSub[, c(110:114)*3 + 2]) +
                        rowMeans(trainSub[, c(130:134)*3 + 2]) +
                        rowMeans(trainSub[, c(150:154)*3 + 2]) +
                        rowMeans(trainSub[, c(170:174)*3 + 2]) +
                        rowMeans(trainSub[, c(190:194)*3 + 2]) +
                        rowMeans(trainSub[, c(210:214)*3 + 2]) +
                        rowMeans(trainSub[, c(230:234)*3 + 2]) +
                        rowMeans(trainSub[, c(250:254)*3 + 2]) +
                        rowMeans(trainSub[, c(270:274)*3 + 2]) +
                        rowMeans(trainSub[, c(290:294)*3 + 2]) +
                        rowMeans(trainSub[, c(310:314)*3 + 2]) +
                        rowMeans(trainSub[, c(330:334)*3 + 2]) +
                        rowMeans(trainSub[, c(350:354)*3 + 2]) +
                        rowMeans(trainSub[, c(370:374)*3 + 2]) +
                        rowMeans(trainSub[, c(390:394)*3 + 2]))/20
trainChopTest[31] <- (rowMeans(trainSub[, c(10:14)*3 + 3]) +
                        rowMeans(trainSub[, c(30:34)*3 + 3]) +
                        rowMeans(trainSub[, c(50:54)*3 + 3]) +
                        rowMeans(trainSub[, c(70:74)*3 + 3]) +
                        rowMeans(trainSub[, c(90:94)*3 + 3]) +
                        rowMeans(trainSub[, c(110:114)*3 + 3]) +
                        rowMeans(trainSub[, c(130:134)*3 + 3]) +
                        rowMeans(trainSub[, c(150:154)*3 + 3]) +
                        rowMeans(trainSub[, c(170:174)*3 + 3]) +
                        rowMeans(trainSub[, c(190:194)*3 + 3]) +
                        rowMeans(trainSub[, c(210:214)*3 + 3]) +
                        rowMeans(trainSub[, c(230:234)*3 + 3]) +
                        rowMeans(trainSub[, c(250:254)*3 + 3]) +
                        rowMeans(trainSub[, c(270:274)*3 + 3]) +
                        rowMeans(trainSub[, c(290:294)*3 + 3]) +
                        rowMeans(trainSub[, c(310:314)*3 + 3]) +
                        rowMeans(trainSub[, c(330:334)*3 + 3]) +
                        rowMeans(trainSub[, c(350:354)*3 + 3]) +
                        rowMeans(trainSub[, c(370:374)*3 + 3]) +
                        rowMeans(trainSub[, c(390:394)*3 + 3]))/20
colnames(trainChopTest)[29:31] <- c("x4vert3R", "x4vert3G", "x4vert3B")

trainChopTest[32] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                        rowMeans(trainSub[, c(35:39)*3 + 1]) +
                        rowMeans(trainSub[, c(55:59)*3 + 1]) +
                        rowMeans(trainSub[, c(75:79)*3 + 1]) +
                        rowMeans(trainSub[, c(95:99)*3 + 1]) +
                        rowMeans(trainSub[, c(115:119)*3 + 1]) +
                        rowMeans(trainSub[, c(135:139)*3 + 1]) +
                        rowMeans(trainSub[, c(155:159)*3 + 1]) +
                        rowMeans(trainSub[, c(175:179)*3 + 1]) +
                        rowMeans(trainSub[, c(195:199)*3 + 1]) +
                        rowMeans(trainSub[, c(215:219)*3 + 1]) +
                        rowMeans(trainSub[, c(235:239)*3 + 1]) +
                        rowMeans(trainSub[, c(255:259)*3 + 1]) +
                        rowMeans(trainSub[, c(275:279)*3 + 1]) +
                        rowMeans(trainSub[, c(295:299)*3 + 1]) +
                        rowMeans(trainSub[, c(315:319)*3 + 1]) +
                        rowMeans(trainSub[, c(335:339)*3 + 1]) +
                        rowMeans(trainSub[, c(355:359)*3 + 1]) +
                        rowMeans(trainSub[, c(375:379)*3 + 1]) +
                        rowMeans(trainSub[, c(395:399)*3 + 1]))/20
trainChopTest[33] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                        rowMeans(trainSub[, c(35:39)*3 + 2]) +
                        rowMeans(trainSub[, c(55:59)*3 + 2]) +
                        rowMeans(trainSub[, c(75:79)*3 + 2]) +
                        rowMeans(trainSub[, c(95:99)*3 + 2]) +
                        rowMeans(trainSub[, c(115:119)*3 + 2]) +
                        rowMeans(trainSub[, c(135:139)*3 + 2]) +
                        rowMeans(trainSub[, c(155:159)*3 + 2]) +
                        rowMeans(trainSub[, c(175:179)*3 + 2]) +
                        rowMeans(trainSub[, c(195:199)*3 + 2]) +
                        rowMeans(trainSub[, c(215:219)*3 + 2]) +
                        rowMeans(trainSub[, c(235:239)*3 + 2]) +
                        rowMeans(trainSub[, c(255:259)*3 + 2]) +
                        rowMeans(trainSub[, c(275:279)*3 + 2]) +
                        rowMeans(trainSub[, c(295:299)*3 + 2]) +
                        rowMeans(trainSub[, c(315:319)*3 + 2]) +
                        rowMeans(trainSub[, c(335:339)*3 + 2]) +
                        rowMeans(trainSub[, c(355:359)*3 + 2]) +
                        rowMeans(trainSub[, c(375:379)*3 + 2]) +
                        rowMeans(trainSub[, c(395:399)*3 + 2]))/20
trainChopTest[34] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                        rowMeans(trainSub[, c(35:39)*3 + 3]) +
                        rowMeans(trainSub[, c(55:59)*3 + 3]) +
                        rowMeans(trainSub[, c(75:79)*3 + 3]) +
                        rowMeans(trainSub[, c(95:99)*3 + 3]) +
                        rowMeans(trainSub[, c(115:119)*3 + 3]) +
                        rowMeans(trainSub[, c(135:139)*3 + 3]) +
                        rowMeans(trainSub[, c(155:159)*3 + 3]) +
                        rowMeans(trainSub[, c(175:179)*3 + 3]) +
                        rowMeans(trainSub[, c(195:199)*3 + 3]) +
                        rowMeans(trainSub[, c(215:219)*3 + 3]) +
                        rowMeans(trainSub[, c(235:239)*3 + 3]) +
                        rowMeans(trainSub[, c(255:259)*3 + 3]) +
                        rowMeans(trainSub[, c(275:279)*3 + 3]) +
                        rowMeans(trainSub[, c(295:299)*3 + 3]) +
                        rowMeans(trainSub[, c(315:319)*3 + 3]) +
                        rowMeans(trainSub[, c(335:339)*3 + 3]) +
                        rowMeans(trainSub[, c(355:359)*3 + 3]) +
                        rowMeans(trainSub[, c(375:379)*3 + 3]) +
                        rowMeans(trainSub[, c(395:399)*3 + 3]))/20
colnames(trainChopTest)[32:34] <- c("x4vert4R", "x4vert4G", "x4vert4B")

trainChopTest[35] <- rowMeans(trainSub[, c(0:99)*3 + 1])
trainChopTest[36] <- rowMeans(trainSub[, c(0:99)*3 + 2])
trainChopTest[37] <- rowMeans(trainSub[, c(0:99)*3 + 3])
colnames(trainChopTest)[35:37] <- c("x4hori1R", "x4hori1G", "x4hori1B")

trainChopTest[38] <- rowMeans(trainSub[, c(100:199)*3 + 1])
trainChopTest[39] <- rowMeans(trainSub[, c(100:199)*3 + 2])
trainChopTest[40] <- rowMeans(trainSub[, c(100:199)*3 + 3])
colnames(trainChopTest)[38:40] <- c("x4hori2R", "x4hori2G", "x4hori2B")

trainChopTest[41] <- rowMeans(trainSub[, c(200:299)*3 + 1])
trainChopTest[42] <- rowMeans(trainSub[, c(200:299)*3 + 2])
trainChopTest[43] <- rowMeans(trainSub[, c(200:299)*3 + 3])
colnames(trainChopTest)[41:43] <- c("x4hori3R", "x4hori3G", "x4hori3B")

trainChopTest[44] <- rowMeans(trainSub[, c(300:399)*3 + 1])
trainChopTest[45] <- rowMeans(trainSub[, c(300:399)*3 + 2])
trainChopTest[46] <- rowMeans(trainSub[, c(300:399)*3 + 3])
colnames(trainChopTest)[44:46] <- c("x4hori4R", "x4hori4G", "x4hori4B")

trainChopTest[47] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                        rowMeans(trainSub[, c(20:24)*3 + 1]) +
                        rowMeans(trainSub[, c(40:44)*3 + 1]) +
                        rowMeans(trainSub[, c(60:64)*3 + 1]) +
                        rowMeans(trainSub[, c(80:84)*3 + 1]) +
                        rowMeans(trainSub[, c(100:104)*3 + 1]) +
                        rowMeans(trainSub[, c(120:124)*3 + 1]) +
                        rowMeans(trainSub[, c(140:144)*3 + 1]) +
                        rowMeans(trainSub[, c(160:164)*3 + 1]) +
                        rowMeans(trainSub[, c(180:184)*3 + 1]))/10
trainChopTest[48] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                        rowMeans(trainSub[, c(20:24)*3 + 2]) +
                        rowMeans(trainSub[, c(40:44)*3 + 2]) +
                        rowMeans(trainSub[, c(60:64)*3 + 2]) +
                        rowMeans(trainSub[, c(80:84)*3 + 2]) +
                        rowMeans(trainSub[, c(100:104)*3 + 2]) +
                        rowMeans(trainSub[, c(120:124)*3 + 2]) +
                        rowMeans(trainSub[, c(140:144)*3 + 2]) +
                        rowMeans(trainSub[, c(160:164)*3 + 2]) +
                        rowMeans(trainSub[, c(180:184)*3 + 2]))/10
trainChopTest[49] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                        rowMeans(trainSub[, c(20:24)*3 + 3]) +
                        rowMeans(trainSub[, c(40:44)*3 + 3]) +
                        rowMeans(trainSub[, c(60:64)*3 + 3]) +
                        rowMeans(trainSub[, c(80:84)*3 + 3]) +
                        rowMeans(trainSub[, c(100:104)*3 + 3]) +
                        rowMeans(trainSub[, c(120:124)*3 + 3]) +
                        rowMeans(trainSub[, c(140:144)*3 + 3]) +
                        rowMeans(trainSub[, c(160:164)*3 + 3]) +
                        rowMeans(trainSub[, c(180:184)*3 + 3]))/10
colnames(trainChopTest)[47:49] <- c("x8verts1R", "x8verts1G", "x8verts1B")

trainChopTest[50] <- (rowMeans(trainSub[, c(200:204)*3 + 1]) +
                        rowMeans(trainSub[, c(220:224)*3 + 1]) +
                        rowMeans(trainSub[, c(240:244)*3 + 1]) +
                        rowMeans(trainSub[, c(260:264)*3 + 1]) +
                        rowMeans(trainSub[, c(280:284)*3 + 1]) +
                        rowMeans(trainSub[, c(300:304)*3 + 1]) +
                        rowMeans(trainSub[, c(320:324)*3 + 1]) +
                        rowMeans(trainSub[, c(340:344)*3 + 1]) +
                        rowMeans(trainSub[, c(360:364)*3 + 1]) +
                        rowMeans(trainSub[, c(380:384)*3 + 1]))/10
trainChopTest[51] <- (rowMeans(trainSub[, c(200:204)*3 + 2]) +
                        rowMeans(trainSub[, c(220:224)*3 + 2]) +
                        rowMeans(trainSub[, c(240:244)*3 + 2]) +
                        rowMeans(trainSub[, c(260:264)*3 + 2]) +
                        rowMeans(trainSub[, c(280:284)*3 + 2]) +
                        rowMeans(trainSub[, c(300:304)*3 + 2]) +
                        rowMeans(trainSub[, c(320:324)*3 + 2]) +
                        rowMeans(trainSub[, c(340:344)*3 + 2]) +
                        rowMeans(trainSub[, c(360:364)*3 + 2]) +
                        rowMeans(trainSub[, c(380:384)*3 + 2]))/10
trainChopTest[52] <- (rowMeans(trainSub[, c(200:204)*3 + 3]) +
                        rowMeans(trainSub[, c(220:224)*3 + 3]) +
                        rowMeans(trainSub[, c(240:244)*3 + 3]) +
                        rowMeans(trainSub[, c(260:264)*3 + 3]) +
                        rowMeans(trainSub[, c(280:284)*3 + 3]) +
                        rowMeans(trainSub[, c(300:304)*3 + 3]) +
                        rowMeans(trainSub[, c(320:324)*3 + 3]) +
                        rowMeans(trainSub[, c(340:344)*3 + 3]) +
                        rowMeans(trainSub[, c(360:364)*3 + 3]) +
                        rowMeans(trainSub[, c(380:384)*3 + 3]))/10
colnames(trainChopTest)[50:52] <- c("x8verts2R", "x8verts2G", "x8verts2B")

trainChopTest[53] <- (rowMeans(trainSub[, c(5:9)*3 + 1]) +
                        rowMeans(trainSub[, c(25:29)*3 + 1]) +
                        rowMeans(trainSub[, c(45:49)*3 + 1]) +
                        rowMeans(trainSub[, c(65:69)*3 + 1]) +
                        rowMeans(trainSub[, c(85:89)*3 + 1]) +
                        rowMeans(trainSub[, c(105:109)*3 + 1]) +
                        rowMeans(trainSub[, c(125:129)*3 + 1]) +
                        rowMeans(trainSub[, c(145:149)*3 + 1]) +
                        rowMeans(trainSub[, c(165:169)*3 + 1]) +
                        rowMeans(trainSub[, c(185:189)*3 + 1]))/10
trainChopTest[54] <- (rowMeans(trainSub[, c(5:9)*3 + 2]) +
                        rowMeans(trainSub[, c(25:29)*3 + 2]) +
                        rowMeans(trainSub[, c(45:49)*3 + 2]) +
                        rowMeans(trainSub[, c(65:69)*3 + 2]) +
                        rowMeans(trainSub[, c(85:89)*3 + 2]) +
                        rowMeans(trainSub[, c(105:109)*3 + 2]) +
                        rowMeans(trainSub[, c(125:129)*3 + 2]) +
                        rowMeans(trainSub[, c(145:149)*3 + 2]) +
                        rowMeans(trainSub[, c(165:169)*3 + 2]) +
                        rowMeans(trainSub[, c(185:189)*3 + 2]))/10
trainChopTest[55] <- (rowMeans(trainSub[, c(5:9)*3 + 3]) +
                        rowMeans(trainSub[, c(25:29)*3 + 3]) +
                        rowMeans(trainSub[, c(45:49)*3 + 3]) +
                        rowMeans(trainSub[, c(65:69)*3 + 3]) +
                        rowMeans(trainSub[, c(85:89)*3 + 3]) +
                        rowMeans(trainSub[, c(105:109)*3 + 3]) +
                        rowMeans(trainSub[, c(125:129)*3 + 3]) +
                        rowMeans(trainSub[, c(145:149)*3 + 3]) +
                        rowMeans(trainSub[, c(165:169)*3 + 3]) +
                        rowMeans(trainSub[, c(185:189)*3 + 3]))/10
colnames(trainChopTest)[53:55] <- c("x8verts3R", "x8verts3G", "x8verts3B")


trainChopTest[56] <- (rowMeans(trainSub[, c(205:209)*3 + 1]) +
                        rowMeans(trainSub[, c(225:229)*3 + 1]) +
                        rowMeans(trainSub[, c(245:249)*3 + 1]) +
                        rowMeans(trainSub[, c(265:269)*3 + 1]) +
                        rowMeans(trainSub[, c(285:289)*3 + 1]) +
                        rowMeans(trainSub[, c(305:309)*3 + 1]) +
                        rowMeans(trainSub[, c(325:329)*3 + 1]) +
                        rowMeans(trainSub[, c(345:349)*3 + 1]) +
                        rowMeans(trainSub[, c(365:369)*3 + 1]) +
                        rowMeans(trainSub[, c(385:389)*3 + 1]))/10
trainChopTest[57] <- (rowMeans(trainSub[, c(205:209)*3 + 2]) +
                        rowMeans(trainSub[, c(225:229)*3 + 2]) +
                        rowMeans(trainSub[, c(245:249)*3 + 2]) +
                        rowMeans(trainSub[, c(265:269)*3 + 2]) +
                        rowMeans(trainSub[, c(285:289)*3 + 2]) +
                        rowMeans(trainSub[, c(305:309)*3 + 2]) +
                        rowMeans(trainSub[, c(325:329)*3 + 2]) +
                        rowMeans(trainSub[, c(345:349)*3 + 2]) +
                        rowMeans(trainSub[, c(365:369)*3 + 2]) +
                        rowMeans(trainSub[, c(385:389)*3 + 2]))/10
trainChopTest[58] <- (rowMeans(trainSub[, c(205:209)*3 + 3]) +
                        rowMeans(trainSub[, c(225:229)*3 + 3]) +
                        rowMeans(trainSub[, c(245:249)*3 + 3]) +
                        rowMeans(trainSub[, c(265:269)*3 + 3]) +
                        rowMeans(trainSub[, c(285:289)*3 + 3]) +
                        rowMeans(trainSub[, c(305:309)*3 + 3]) +
                        rowMeans(trainSub[, c(325:329)*3 + 3]) +
                        rowMeans(trainSub[, c(345:349)*3 + 3]) +
                        rowMeans(trainSub[, c(365:369)*3 + 3]) +
                        rowMeans(trainSub[, c(385:389)*3 + 3]))/10
colnames(trainChopTest)[56:58] <- c("x8verts4R", "x8verts4G", "x8verts4B")

trainChopTest[59] <- (rowMeans(trainSub[, c(10:14)*3 + 1]) +
                        rowMeans(trainSub[, c(30:34)*3 + 1]) +
                        rowMeans(trainSub[, c(50:54)*3 + 1]) +
                        rowMeans(trainSub[, c(70:74)*3 + 1]) +
                        rowMeans(trainSub[, c(90:94)*3 + 1]) +
                        rowMeans(trainSub[, c(110:114)*3 + 1]) +
                        rowMeans(trainSub[, c(130:134)*3 + 1]) +
                        rowMeans(trainSub[, c(150:154)*3 + 1]) +
                        rowMeans(trainSub[, c(170:174)*3 + 1]) +
                        rowMeans(trainSub[, c(190:194)*3 + 1]))/10
trainChopTest[60] <- (rowMeans(trainSub[, c(10:14)*3 + 2]) +
                        rowMeans(trainSub[, c(30:34)*3 + 2]) +
                        rowMeans(trainSub[, c(50:54)*3 + 2]) +
                        rowMeans(trainSub[, c(70:74)*3 + 2]) +
                        rowMeans(trainSub[, c(90:94)*3 + 2]) +
                        rowMeans(trainSub[, c(110:114)*3 + 2]) +
                        rowMeans(trainSub[, c(130:134)*3 + 2]) +
                        rowMeans(trainSub[, c(150:154)*3 + 2]) +
                        rowMeans(trainSub[, c(170:174)*3 + 2]) +
                        rowMeans(trainSub[, c(190:194)*3 + 2]))/10 
trainChopTest[61] <- (rowMeans(trainSub[, c(10:14)*3 + 3]) +
                        rowMeans(trainSub[, c(30:34)*3 + 3]) +
                        rowMeans(trainSub[, c(50:54)*3 + 3]) +
                        rowMeans(trainSub[, c(70:74)*3 + 3]) +
                        rowMeans(trainSub[, c(90:94)*3 + 3]) +
                        rowMeans(trainSub[, c(110:114)*3 + 3]) +
                        rowMeans(trainSub[, c(130:134)*3 + 3]) +
                        rowMeans(trainSub[, c(150:154)*3 + 3]) +
                        rowMeans(trainSub[, c(170:174)*3 + 3]) +
                        rowMeans(trainSub[, c(190:194)*3 + 3]))/10
colnames(trainChopTest)[59:61] <- c("x8verts5R", "x8verts5G", "x8verts5B")

trainChopTest[62] <- (rowMeans(trainSub[, c(210:214)*3 + 1]) +
                        rowMeans(trainSub[, c(230:234)*3 + 1]) +
                        rowMeans(trainSub[, c(250:254)*3 + 1]) +
                        rowMeans(trainSub[, c(270:274)*3 + 1]) +
                        rowMeans(trainSub[, c(290:294)*3 + 1]) +
                        rowMeans(trainSub[, c(310:314)*3 + 1]) +
                        rowMeans(trainSub[, c(330:334)*3 + 1]) +
                        rowMeans(trainSub[, c(350:354)*3 + 1]) +
                        rowMeans(trainSub[, c(370:374)*3 + 1]) +
                        rowMeans(trainSub[, c(390:394)*3 + 1]))/10
trainChopTest[63] <- (rowMeans(trainSub[, c(210:214)*3 + 2]) +
                        rowMeans(trainSub[, c(230:234)*3 + 2]) +
                        rowMeans(trainSub[, c(250:254)*3 + 2]) +
                        rowMeans(trainSub[, c(270:274)*3 + 2]) +
                        rowMeans(trainSub[, c(290:294)*3 + 2]) +
                        rowMeans(trainSub[, c(310:314)*3 + 2]) +
                        rowMeans(trainSub[, c(330:334)*3 + 2]) +
                        rowMeans(trainSub[, c(350:354)*3 + 2]) +
                        rowMeans(trainSub[, c(370:374)*3 + 2]) +
                        rowMeans(trainSub[, c(390:394)*3 + 2]))/10
trainChopTest[64] <- (rowMeans(trainSub[, c(210:214)*3 + 3]) +
                        rowMeans(trainSub[, c(230:234)*3 + 3]) +
                        rowMeans(trainSub[, c(250:254)*3 + 3]) +
                        rowMeans(trainSub[, c(270:274)*3 + 3]) +
                        rowMeans(trainSub[, c(290:294)*3 + 3]) +
                        rowMeans(trainSub[, c(310:314)*3 + 3]) +
                        rowMeans(trainSub[, c(330:334)*3 + 3]) +
                        rowMeans(trainSub[, c(350:354)*3 + 3]) +
                        rowMeans(trainSub[, c(370:374)*3 + 3]) +
                        rowMeans(trainSub[, c(390:394)*3 + 3]))/10
colnames(trainChopTest)[62:64] <- c("x8verts6R", "x8verts6G", "x8verts6B")

trainChopTest[65] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                        rowMeans(trainSub[, c(35:39)*3 + 1]) +
                        rowMeans(trainSub[, c(55:59)*3 + 1]) +
                        rowMeans(trainSub[, c(75:79)*3 + 1]) +
                        rowMeans(trainSub[, c(95:99)*3 + 1]) +
                        rowMeans(trainSub[, c(115:119)*3 + 1]) +
                        rowMeans(trainSub[, c(135:139)*3 + 1]) +
                        rowMeans(trainSub[, c(155:159)*3 + 1]) +
                        rowMeans(trainSub[, c(175:179)*3 + 1]) +
                        rowMeans(trainSub[, c(195:199)*3 + 1]))/10
trainChopTest[66] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                        rowMeans(trainSub[, c(35:39)*3 + 2]) +
                        rowMeans(trainSub[, c(55:59)*3 + 2]) +
                        rowMeans(trainSub[, c(75:79)*3 + 2]) +
                        rowMeans(trainSub[, c(95:99)*3 + 2]) +
                        rowMeans(trainSub[, c(115:119)*3 + 2]) +
                        rowMeans(trainSub[, c(135:139)*3 + 2]) +
                        rowMeans(trainSub[, c(155:159)*3 + 2]) +
                        rowMeans(trainSub[, c(175:179)*3 + 2]) +
                        rowMeans(trainSub[, c(195:199)*3 + 2]))/10 
trainChopTest[67] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                        rowMeans(trainSub[, c(35:39)*3 + 3]) +
                        rowMeans(trainSub[, c(55:59)*3 + 3]) +
                        rowMeans(trainSub[, c(75:79)*3 + 3]) +
                        rowMeans(trainSub[, c(95:99)*3 + 3]) +
                        rowMeans(trainSub[, c(115:119)*3 + 3]) +
                        rowMeans(trainSub[, c(135:139)*3 + 3]) +
                        rowMeans(trainSub[, c(155:159)*3 + 3]) +
                        rowMeans(trainSub[, c(175:179)*3 + 3]) +
                        rowMeans(trainSub[, c(195:199)*3 + 3]))/10 
colnames(trainChopTest)[65:67] <- c("x8verts7R", "x8verts7G", "x8verts7B")

trainChopTest[68] <- (rowMeans(trainSub[, c(215:219)*3 + 1]) +
                        rowMeans(trainSub[, c(235:239)*3 + 1]) +
                        rowMeans(trainSub[, c(255:259)*3 + 1]) +
                        rowMeans(trainSub[, c(275:279)*3 + 1]) +
                        rowMeans(trainSub[, c(295:299)*3 + 1]) +
                        rowMeans(trainSub[, c(315:319)*3 + 1]) +
                        rowMeans(trainSub[, c(335:339)*3 + 1]) +
                        rowMeans(trainSub[, c(355:359)*3 + 1]) +
                        rowMeans(trainSub[, c(375:379)*3 + 1]) +
                        rowMeans(trainSub[, c(395:399)*3 + 1]))/10
trainChopTest[69] <- (rowMeans(trainSub[, c(215:219)*3 + 2]) +
                        rowMeans(trainSub[, c(235:239)*3 + 2]) +
                        rowMeans(trainSub[, c(255:259)*3 + 2]) +
                        rowMeans(trainSub[, c(275:279)*3 + 2]) +
                        rowMeans(trainSub[, c(295:299)*3 + 2]) +
                        rowMeans(trainSub[, c(315:319)*3 + 2]) +
                        rowMeans(trainSub[, c(335:339)*3 + 2]) +
                        rowMeans(trainSub[, c(355:359)*3 + 2]) +
                        rowMeans(trainSub[, c(375:379)*3 + 2]) +
                        rowMeans(trainSub[, c(395:399)*3 + 2]))/10
trainChopTest[70] <- (rowMeans(trainSub[, c(215:219)*3 + 3]) +
                        rowMeans(trainSub[, c(235:239)*3 + 3]) +
                        rowMeans(trainSub[, c(255:259)*3 + 3]) +
                        rowMeans(trainSub[, c(275:279)*3 + 3]) +
                        rowMeans(trainSub[, c(295:299)*3 + 3]) +
                        rowMeans(trainSub[, c(315:319)*3 + 3]) +
                        rowMeans(trainSub[, c(335:339)*3 + 3]) +
                        rowMeans(trainSub[, c(355:359)*3 + 3]) +
                        rowMeans(trainSub[, c(375:379)*3 + 3]) +
                        rowMeans(trainSub[, c(395:399)*3 + 3]))/10
colnames(trainChopTest)[68:70] <- c("x8verts8R", "x8verts8G", "x8verts8B")

trainChopTest[71] <- (rowMeans(trainSub[, c(0:9)*3 + 1]) +
                        rowMeans(trainSub[, c(20:29)*3 + 1]) +
                        rowMeans(trainSub[, c(40:49)*3 + 1]) +
                        rowMeans(trainSub[, c(60:69)*3 + 1]) +
                        rowMeans(trainSub[, c(80:89)*3 + 1]))/5
trainChopTest[72] <- (rowMeans(trainSub[, c(0:9)*3 + 2]) +
                        rowMeans(trainSub[, c(20:29)*3 + 2]) +
                        rowMeans(trainSub[, c(40:49)*3 + 2]) +
                        rowMeans(trainSub[, c(60:69)*3 + 2]) +
                        rowMeans(trainSub[, c(80:89)*3 + 2]))/5  
trainChopTest[73] <- (rowMeans(trainSub[, c(0:9)*3 + 3]) +
                        rowMeans(trainSub[, c(20:29)*3 + 3]) +
                        rowMeans(trainSub[, c(40:49)*3 + 3]) +
                        rowMeans(trainSub[, c(60:69)*3 + 3]) +
                        rowMeans(trainSub[, c(80:89)*3 + 3]))/5
colnames(trainChopTest)[71:73] <- c("x8hori1R", "x8hori1G", "x8hori1B")

trainChopTest[74] <- (rowMeans(trainSub[, c(100:109)*3 + 1]) +
                        rowMeans(trainSub[, c(120:129)*3 + 1]) +
                        rowMeans(trainSub[, c(140:149)*3 + 1]) +
                        rowMeans(trainSub[, c(160:169)*3 + 1]) +
                        rowMeans(trainSub[, c(180:189)*3 + 1]))/5
trainChopTest[75] <- (rowMeans(trainSub[, c(100:109)*3 + 2]) +
                        rowMeans(trainSub[, c(120:129)*3 + 2]) +
                        rowMeans(trainSub[, c(140:149)*3 + 2]) +
                        rowMeans(trainSub[, c(160:169)*3 + 2]) +
                        rowMeans(trainSub[, c(180:189)*3 + 2]))/5
trainChopTest[76] <- (rowMeans(trainSub[, c(100:109)*3 + 3]) +
                        rowMeans(trainSub[, c(120:129)*3 + 3]) +
                        rowMeans(trainSub[, c(140:149)*3 + 3]) +
                        rowMeans(trainSub[, c(160:169)*3 + 3]) +
                        rowMeans(trainSub[, c(180:189)*3 + 3]))/5
colnames(trainChopTest)[74:76] <- c("x8hori2R", "x8hori2G", "x8hori2B")

trainChopTest[77] <- (rowMeans(trainSub[, c(200:209)*3 + 1]) +
                        rowMeans(trainSub[, c(220:229)*3 + 1]) +
                        rowMeans(trainSub[, c(240:249)*3 + 1]) +
                        rowMeans(trainSub[, c(260:269)*3 + 1]) +
                        rowMeans(trainSub[, c(280:289)*3 + 1]))/5
trainChopTest[78] <- (rowMeans(trainSub[, c(200:209)*3 + 2]) +
                        rowMeans(trainSub[, c(220:229)*3 + 2]) +
                        rowMeans(trainSub[, c(240:249)*3 + 2]) +
                        rowMeans(trainSub[, c(260:269)*3 + 2]) +
                        rowMeans(trainSub[, c(280:289)*3 + 2]))/5
trainChopTest[79] <- (rowMeans(trainSub[, c(200:209)*3 + 3]) +
                        rowMeans(trainSub[, c(220:229)*3 + 3]) +
                        rowMeans(trainSub[, c(240:249)*3 + 3]) +
                        rowMeans(trainSub[, c(260:269)*3 + 3]) +
                        rowMeans(trainSub[, c(280:289)*3 + 3]))/5
colnames(trainChopTest)[77:79] <- c("x8hori3R", "x8hori3G", "x8hori3B")                    

trainChopTest[80] <- (rowMeans(trainSub[, c(300:309)*3 + 1]) +
                        rowMeans(trainSub[, c(320:329)*3 + 1]) +
                        rowMeans(trainSub[, c(340:349)*3 + 1]) +
                        rowMeans(trainSub[, c(360:369)*3 + 1]) +
                        rowMeans(trainSub[, c(380:389)*3 + 1]))/5
trainChopTest[81] <- (rowMeans(trainSub[, c(300:309)*3 + 2]) +
                        rowMeans(trainSub[, c(320:329)*3 + 2]) +
                        rowMeans(trainSub[, c(340:349)*3 + 2]) +
                        rowMeans(trainSub[, c(360:369)*3 + 2]) +
                        rowMeans(trainSub[, c(380:389)*3 + 2]))/5
trainChopTest[82] <- (rowMeans(trainSub[, c(300:309)*3 + 3]) +
                        rowMeans(trainSub[, c(320:329)*3 + 3]) +
                        rowMeans(trainSub[, c(340:349)*3 + 3]) +
                        rowMeans(trainSub[, c(360:369)*3 + 3]) +
                        rowMeans(trainSub[, c(380:389)*3 + 3]))/5
colnames(trainChopTest)[80:82] <- c("x8hori4R", "x8hori4G", "x8hori4B")

trainChopTest[83] <- (rowMeans(trainSub[, c(10:19)*3 + 1]) +
                        rowMeans(trainSub[, c(30:39)*3 + 1]) +
                        rowMeans(trainSub[, c(50:59)*3 + 1]) +
                        rowMeans(trainSub[, c(70:79)*3 + 1]) +
                        rowMeans(trainSub[, c(90:99)*3 + 1]))/5
trainChopTest[84] <- (rowMeans(trainSub[, c(10:19)*3 + 2]) +
                        rowMeans(trainSub[, c(30:39)*3 + 2]) +
                        rowMeans(trainSub[, c(50:59)*3 + 2]) +
                        rowMeans(trainSub[, c(70:79)*3 + 2]) +
                        rowMeans(trainSub[, c(90:99)*3 + 2]))/5
trainChopTest[85] <- (rowMeans(trainSub[, c(10:19)*3 + 3]) +
                        rowMeans(trainSub[, c(30:39)*3 + 3]) +
                        rowMeans(trainSub[, c(50:59)*3 + 3]) +
                        rowMeans(trainSub[, c(70:79)*3 + 3]) +
                        rowMeans(trainSub[, c(90:99)*3 + 3]))/5
colnames(trainChopTest)[83:85] <- c("x8hori5R", "x8hori5G", "x8hori5B")

trainChopTest[86] <- (rowMeans(trainSub[, c(110:119)*3 + 1]) +
                        rowMeans(trainSub[, c(130:139)*3 + 1]) +
                        rowMeans(trainSub[, c(150:159)*3 + 1]) +
                        rowMeans(trainSub[, c(170:179)*3 + 1]) +
                        rowMeans(trainSub[, c(190:199)*3 + 1]))/5
trainChopTest[87] <- (rowMeans(trainSub[, c(110:119)*3 + 2]) +
                        rowMeans(trainSub[, c(130:139)*3 + 2]) +
                        rowMeans(trainSub[, c(150:159)*3 + 2]) +
                        rowMeans(trainSub[, c(170:179)*3 + 2]) +
                        rowMeans(trainSub[, c(190:199)*3 + 2]))/5
trainChopTest[88] <- (rowMeans(trainSub[, c(110:119)*3 + 3]) +
                        rowMeans(trainSub[, c(130:139)*3 + 3]) +
                        rowMeans(trainSub[, c(150:159)*3 + 3]) +
                        rowMeans(trainSub[, c(170:179)*3 + 3]) +
                        rowMeans(trainSub[, c(190:199)*3 + 3]))/5
colnames(trainChopTest)[86:88] <- c("x8hori6R", "x8hori6G", "x8hori6B")

trainChopTest[89] <- (rowMeans(trainSub[, c(210:219)*3 + 1]) +
                        rowMeans(trainSub[, c(230:239)*3 + 1]) +
                        rowMeans(trainSub[, c(250:259)*3 + 1]) +
                        rowMeans(trainSub[, c(270:279)*3 + 1]) +
                        rowMeans(trainSub[, c(290:299)*3 + 1]))/5
trainChopTest[90] <- (rowMeans(trainSub[, c(210:219)*3 + 2]) +
                        rowMeans(trainSub[, c(230:239)*3 + 2]) +
                        rowMeans(trainSub[, c(250:259)*3 + 2]) +
                        rowMeans(trainSub[, c(270:279)*3 + 2]) +
                        rowMeans(trainSub[, c(290:299)*3 + 2]))/5
trainChopTest[91] <- (rowMeans(trainSub[, c(210:219)*3 + 3]) +
                        rowMeans(trainSub[, c(230:239)*3 + 3]) +
                        rowMeans(trainSub[, c(250:259)*3 + 3]) +
                        rowMeans(trainSub[, c(270:279)*3 + 3]) +
                        rowMeans(trainSub[, c(290:299)*3 + 3]))/5
colnames(trainChopTest)[89:91] <- c("x8hori7R", "x8hori7G", "x8hori7B")

trainChopTest[92] <- (rowMeans(trainSub[, c(310:319)*3 + 1]) +
                        rowMeans(trainSub[, c(330:339)*3 + 1]) +
                        rowMeans(trainSub[, c(350:359)*3 + 1]) +
                        rowMeans(trainSub[, c(370:379)*3 + 1]) +
                        rowMeans(trainSub[, c(390:399)*3 + 1]))/5
trainChopTest[93] <- (rowMeans(trainSub[, c(310:319)*3 + 2]) +
                        rowMeans(trainSub[, c(330:339)*3 + 2]) +
                        rowMeans(trainSub[, c(350:359)*3 + 2]) +
                        rowMeans(trainSub[, c(370:379)*3 + 2]) +
                        rowMeans(trainSub[, c(390:399)*3 + 2]))/5
trainChopTest[94] <- (rowMeans(trainSub[, c(310:319)*3 + 3]) +
                        rowMeans(trainSub[, c(330:339)*3 + 3]) +
                        rowMeans(trainSub[, c(350:359)*3 + 3]) +
                        rowMeans(trainSub[, c(370:379)*3 + 3]) +
                        rowMeans(trainSub[, c(390:399)*3 + 3]))/5
colnames(trainChopTest)[92:94] <- c("x8hori8R", "x8hori8G", "x8hori8B")

trainChopTest[95] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                        rowMeans(trainSub[, c(20:24)*3 + 1]) +
                        rowMeans(trainSub[, c(40:44)*3 + 1]) +
                        rowMeans(trainSub[, c(60:64)*3 + 1]) +
                        rowMeans(trainSub[, c(80:84)*3 + 1]) +
                        rowMeans(trainSub[, c(300:304)*3 + 1]) +
                        rowMeans(trainSub[, c(320:324)*3 + 1]) +
                        rowMeans(trainSub[, c(340:344)*3 + 1]) +
                        rowMeans(trainSub[, c(360:364)*3 + 1]) +
                        rowMeans(trainSub[, c(380:384)*3 + 1]) +
                        rowMeans(trainSub[, c(15:19)*3 + 1]) +
                        rowMeans(trainSub[, c(35:39)*3 + 1]) +
                        rowMeans(trainSub[, c(55:59)*3 + 1]) +
                        rowMeans(trainSub[, c(75:79)*3 + 1]) +
                        rowMeans(trainSub[, c(95:99)*3 + 1]) +
                        rowMeans(trainSub[, c(315:319)*3 + 1]) +
                        rowMeans(trainSub[, c(335:339)*3 + 1]) +
                        rowMeans(trainSub[, c(355:359)*3 + 1]) +
                        rowMeans(trainSub[, c(375:379)*3 + 1]) +
                        rowMeans(trainSub[, c(395:399)*3 + 1]))/20
trainChopTest[96] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                        rowMeans(trainSub[, c(20:24)*3 + 2]) +
                        rowMeans(trainSub[, c(40:44)*3 + 2]) +
                        rowMeans(trainSub[, c(60:64)*3 + 2]) +
                        rowMeans(trainSub[, c(80:84)*3 + 2]) +
                        rowMeans(trainSub[, c(300:304)*3 + 2]) +
                        rowMeans(trainSub[, c(320:324)*3 + 2]) +
                        rowMeans(trainSub[, c(340:344)*3 + 2]) +
                        rowMeans(trainSub[, c(360:364)*3 + 2]) +
                        rowMeans(trainSub[, c(380:384)*3 + 2]) +
                        rowMeans(trainSub[, c(15:19)*3 + 2]) +
                        rowMeans(trainSub[, c(35:39)*3 + 2]) +
                        rowMeans(trainSub[, c(55:59)*3 + 2]) +
                        rowMeans(trainSub[, c(75:79)*3 + 2]) +
                        rowMeans(trainSub[, c(95:99)*3 + 2]) +
                        rowMeans(trainSub[, c(315:319)*3 + 2]) +
                        rowMeans(trainSub[, c(335:339)*3 + 2]) +
                        rowMeans(trainSub[, c(355:359)*3 + 2]) +
                        rowMeans(trainSub[, c(375:379)*3 + 2]) +
                        rowMeans(trainSub[, c(395:399)*3 + 2]))/20 
trainChopTest[97] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                        rowMeans(trainSub[, c(20:24)*3 + 3]) +
                        rowMeans(trainSub[, c(40:44)*3 + 3]) +
                        rowMeans(trainSub[, c(60:64)*3 + 3]) +
                        rowMeans(trainSub[, c(80:84)*3 + 3]) +
                        rowMeans(trainSub[, c(300:304)*3 + 3]) +
                        rowMeans(trainSub[, c(320:324)*3 + 3]) +
                        rowMeans(trainSub[, c(340:344)*3 + 3]) +
                        rowMeans(trainSub[, c(360:364)*3 + 3]) +
                        rowMeans(trainSub[, c(380:384)*3 + 3]) +
                        rowMeans(trainSub[, c(15:19)*3 + 3]) +
                        rowMeans(trainSub[, c(35:39)*3 + 3]) +
                        rowMeans(trainSub[, c(55:59)*3 + 3]) +
                        rowMeans(trainSub[, c(75:79)*3 + 3]) +
                        rowMeans(trainSub[, c(95:99)*3 + 3]) +
                        rowMeans(trainSub[, c(315:319)*3 + 3]) +
                        rowMeans(trainSub[, c(335:339)*3 + 3]) +
                        rowMeans(trainSub[, c(355:359)*3 + 3]) +
                        rowMeans(trainSub[, c(375:379)*3 + 3]) +
                        rowMeans(trainSub[, c(395:399)*3 + 3]))/20
colnames(trainChopTest)[95:97] <- c("cornersR", "cornersG", "cornersB")

trainChopTest[98] <- (rowMeans(trainSub[, c(0:4)*3 + 1]) +
                        rowMeans(trainSub[, c(20:24)*3 + 1]) +
                        rowMeans(trainSub[, c(40:44)*3 + 1]) +
                        rowMeans(trainSub[, c(60:64)*3 + 1]) +
                        rowMeans(trainSub[, c(80:84)*3 + 1]))/5
trainChopTest[99] <- (rowMeans(trainSub[, c(0:4)*3 + 2]) +
                        rowMeans(trainSub[, c(20:24)*3 + 2]) +
                        rowMeans(trainSub[, c(40:44)*3 + 2]) +
                        rowMeans(trainSub[, c(60:64)*3 + 2]) +
                        rowMeans(trainSub[, c(80:84)*3 + 2]))/5
trainChopTest[100] <- (rowMeans(trainSub[, c(0:4)*3 + 3]) +
                         rowMeans(trainSub[, c(20:24)*3 + 3]) +
                         rowMeans(trainSub[, c(40:44)*3 + 3]) +
                         rowMeans(trainSub[, c(60:64)*3 + 3]) +
                         rowMeans(trainSub[, c(80:84)*3 + 3]))/5
colnames(trainChopTest)[98:100] <- c("TLcornerR", "TLcornerG", "TLcornerB")

trainChopTest[101] <- (rowMeans(trainSub[, c(300:304)*3 + 1]) +
                         rowMeans(trainSub[, c(320:324)*3 + 1]) +
                         rowMeans(trainSub[, c(340:344)*3 + 1]) +
                         rowMeans(trainSub[, c(360:364)*3 + 1]) +
                         rowMeans(trainSub[, c(380:384)*3 + 1]))/5
trainChopTest[102] <- (rowMeans(trainSub[, c(300:304)*3 + 2]) +
                         rowMeans(trainSub[, c(320:324)*3 + 2]) +
                         rowMeans(trainSub[, c(340:344)*3 + 2]) +
                         rowMeans(trainSub[, c(360:364)*3 + 2]) +
                         rowMeans(trainSub[, c(380:384)*3 + 2]))/5
trainChopTest[103] <- (rowMeans(trainSub[, c(300:304)*3 + 3]) +
                         rowMeans(trainSub[, c(320:324)*3 + 3]) +
                         rowMeans(trainSub[, c(340:344)*3 + 3]) +
                         rowMeans(trainSub[, c(360:364)*3 + 3]) +
                         rowMeans(trainSub[, c(380:384)*3 + 3]))/5
colnames(trainChopTest)[101:103] <- c("TRcornerR", "TRcornerG", "TRcornerB")

trainChopTest[104] <- (rowMeans(trainSub[, c(15:19)*3 + 1]) +
                         rowMeans(trainSub[, c(35:39)*3 + 1]) +
                         rowMeans(trainSub[, c(55:59)*3 + 1]) +
                         rowMeans(trainSub[, c(75:79)*3 + 1]) +
                         rowMeans(trainSub[, c(95:99)*3 + 1]))/5
trainChopTest[105] <- (rowMeans(trainSub[, c(15:19)*3 + 2]) +
                         rowMeans(trainSub[, c(35:39)*3 + 2]) +
                         rowMeans(trainSub[, c(55:59)*3 + 2]) +
                         rowMeans(trainSub[, c(75:79)*3 + 2]) +
                         rowMeans(trainSub[, c(95:99)*3 + 2]))/5
trainChopTest[106] <- (rowMeans(trainSub[, c(15:19)*3 + 3]) +
                         rowMeans(trainSub[, c(35:39)*3 + 3]) +
                         rowMeans(trainSub[, c(55:59)*3 + 3]) +
                         rowMeans(trainSub[, c(75:79)*3 + 3]) +
                         rowMeans(trainSub[, c(95:99)*3 + 3]))/5
colnames(trainChopTest)[104:106] <- c("BLcornerR", "BLcornerG", "BLcornerB")

trainChopTest[107] <- (rowMeans(trainSub[, c(315:319)*3 + 1]) +
                         rowMeans(trainSub[, c(335:339)*3 + 1]) +
                         rowMeans(trainSub[, c(355:359)*3 + 1]) +
                         rowMeans(trainSub[, c(375:379)*3 + 1]) +
                         rowMeans(trainSub[, c(395:399)*3 + 1]))/5
trainChopTest[108] <- (rowMeans(trainSub[, c(315:319)*3 + 2]) +
                         rowMeans(trainSub[, c(335:339)*3 + 2]) +
                         rowMeans(trainSub[, c(355:359)*3 + 2]) +
                         rowMeans(trainSub[, c(375:379)*3 + 2]) +
                         rowMeans(trainSub[, c(395:399)*3 + 2]))/5
trainChopTest[109] <- (rowMeans(trainSub[, c(315:319)*3 + 3]) +
                         rowMeans(trainSub[, c(335:339)*3 + 3]) +
                         rowMeans(trainSub[, c(355:359)*3 + 3]) +
                         rowMeans(trainSub[, c(375:379)*3 + 3]) +
                         rowMeans(trainSub[, c(395:399)*3 + 3]))/5
colnames(trainChopTest)[107:109] <- c("BRcornerR", "BRcornerG", "BRcornerB")

trainChopTest[110] <- (rowMeans(trainSub[, c(105:114)*3 + 1]) +
                         rowMeans(trainSub[, c(125:134)*3 + 1]) +
                         rowMeans(trainSub[, c(145:154)*3 + 1]) +
                         rowMeans(trainSub[, c(165:174)*3 + 1]) +
                         rowMeans(trainSub[, c(185:194)*3 + 1]) +
                         rowMeans(trainSub[, c(205:214)*3 + 1]) +
                         rowMeans(trainSub[, c(225:234)*3 + 1]) +
                         rowMeans(trainSub[, c(245:254)*3 + 1]) +
                         rowMeans(trainSub[, c(265:274)*3 + 1]) +
                         rowMeans(trainSub[, c(285:294)*3 + 1]))/10
trainChopTest[111] <- (rowMeans(trainSub[, c(105:114)*3 + 2]) +
                         rowMeans(trainSub[, c(125:134)*3 + 2]) +
                         rowMeans(trainSub[, c(145:154)*3 + 2]) +
                         rowMeans(trainSub[, c(165:174)*3 + 2]) +
                         rowMeans(trainSub[, c(185:194)*3 + 2]) +
                         rowMeans(trainSub[, c(205:214)*3 + 2]) +
                         rowMeans(trainSub[, c(225:234)*3 + 2]) +
                         rowMeans(trainSub[, c(245:254)*3 + 2]) +
                         rowMeans(trainSub[, c(265:274)*3 + 2]) +
                         rowMeans(trainSub[, c(285:294)*3 + 2]))/10
trainChopTest[112] <- (rowMeans(trainSub[, c(105:114)*3 + 3]) +
                         rowMeans(trainSub[, c(125:134)*3 + 3]) +
                         rowMeans(trainSub[, c(145:154)*3 + 3]) +
                         rowMeans(trainSub[, c(165:174)*3 + 3]) +
                         rowMeans(trainSub[, c(185:194)*3 + 3]) +
                         rowMeans(trainSub[, c(205:214)*3 + 3]) +
                         rowMeans(trainSub[, c(225:234)*3 + 3]) +
                         rowMeans(trainSub[, c(245:254)*3 + 3]) +
                         rowMeans(trainSub[, c(265:274)*3 + 3]) +
                         rowMeans(trainSub[, c(285:294)*3 + 3]))/10
colnames(trainChopTest)[110:112] <- c("centerMainR", "centerMainG", "centerMainB")

trainChopTest[113] <- (rowMeans(trainSub[, c(105:109)*3 + 1]) +
                         rowMeans(trainSub[, c(125:129)*3 + 1]) +
                         rowMeans(trainSub[, c(145:149)*3 + 1]) +
                         rowMeans(trainSub[, c(165:169)*3 + 1]) +
                         rowMeans(trainSub[, c(185:189)*3 + 1]))/5
trainChopTest[114] <- (rowMeans(trainSub[, c(105:109)*3 + 2]) +
                         rowMeans(trainSub[, c(125:129)*3 + 2]) +
                         rowMeans(trainSub[, c(145:149)*3 + 2]) +
                         rowMeans(trainSub[, c(165:169)*3 + 2]) +
                         rowMeans(trainSub[, c(185:189)*3 + 2]))/5
trainChopTest[115] <- (rowMeans(trainSub[, c(105:109)*3 + 3]) +
                         rowMeans(trainSub[, c(125:129)*3 + 3]) +
                         rowMeans(trainSub[, c(145:149)*3 + 3]) +
                         rowMeans(trainSub[, c(165:169)*3 + 3]) +
                         rowMeans(trainSub[, c(185:189)*3 + 3]))/5
colnames(trainChopTest)[113:115] <- c("TLcenterR", "TLcenterG", "TLcenterB")

trainChopTest[116] <- (rowMeans(trainSub[, c(110:114)*3 + 1]) +
                         rowMeans(trainSub[, c(130:134)*3 + 1]) +
                         rowMeans(trainSub[, c(150:154)*3 + 1]) +
                         rowMeans(trainSub[, c(170:174)*3 + 1]) +
                         rowMeans(trainSub[, c(190:194)*3 + 1]))/5
trainChopTest[117] <- (rowMeans(trainSub[, c(110:114)*3 + 2]) +
                         rowMeans(trainSub[, c(130:134)*3 + 2]) +
                         rowMeans(trainSub[, c(150:154)*3 + 2]) +
                         rowMeans(trainSub[, c(170:174)*3 + 2]) +
                         rowMeans(trainSub[, c(190:194)*3 + 2]))/5
trainChopTest[118] <- (rowMeans(trainSub[, c(110:114)*3 + 3]) +
                         rowMeans(trainSub[, c(130:134)*3 + 3]) +
                         rowMeans(trainSub[, c(150:154)*3 + 3]) +
                         rowMeans(trainSub[, c(170:174)*3 + 3]) +
                         rowMeans(trainSub[, c(190:194)*3 + 3]))/5
colnames(trainChopTest)[116:118] <- c("TRcenterR", "TRcenterG", "TRcenterB")

trainChopTest[119] <- (rowMeans(trainSub[, c(205:209)*3 + 1]) +
                         rowMeans(trainSub[, c(225:229)*3 + 1]) +
                         rowMeans(trainSub[, c(245:249)*3 + 1]) +
                         rowMeans(trainSub[, c(265:269)*3 + 1]) +
                         rowMeans(trainSub[, c(285:289)*3 + 1]))/5
trainChopTest[120] <- (rowMeans(trainSub[, c(205:209)*3 + 2]) +
                         rowMeans(trainSub[, c(225:229)*3 + 2]) +
                         rowMeans(trainSub[, c(245:249)*3 + 2]) +
                         rowMeans(trainSub[, c(265:269)*3 + 2]) +
                         rowMeans(trainSub[, c(285:289)*3 + 2]))/5
trainChopTest[121] <- (rowMeans(trainSub[, c(205:209)*3 + 3]) +
                         rowMeans(trainSub[, c(225:229)*3 + 3]) +
                         rowMeans(trainSub[, c(245:249)*3 + 3]) +
                         rowMeans(trainSub[, c(265:269)*3 + 3]) +
                         rowMeans(trainSub[, c(285:289)*3 + 3]))/5
colnames(trainChopTest)[119:121] <- c("BLcenterR", "BLcenterG", "BLcenterB")

trainChopTest[122] <- (rowMeans(trainSub[, c(210:214)*3 + 1]) +
                         rowMeans(trainSub[, c(230:234)*3 + 1]) +
                         rowMeans(trainSub[, c(250:254)*3 + 1]) +
                         rowMeans(trainSub[, c(270:274)*3 + 1]) +
                         rowMeans(trainSub[, c(290:294)*3 + 1]))/5
trainChopTest[123] <- (rowMeans(trainSub[, c(210:214)*3 + 2]) +
                         rowMeans(trainSub[, c(230:234)*3 + 2]) +
                         rowMeans(trainSub[, c(250:254)*3 + 2]) +
                         rowMeans(trainSub[, c(270:274)*3 + 2]) +
                         rowMeans(trainSub[, c(290:294)*3 + 2]))/5
trainChopTest[124] <- (rowMeans(trainSub[, c(210:214)*3 + 3]) +
                         rowMeans(trainSub[, c(230:234)*3 + 3]) +
                         rowMeans(trainSub[, c(250:254)*3 + 3]) +
                         rowMeans(trainSub[, c(270:274)*3 + 3]) +
                         rowMeans(trainSub[, c(290:294)*3 + 3]))/5
colnames(trainChopTest)[122:124] <- c("BRcenterR", "BRcenterG", "BRcenterB")

trainChopTest[125] <- (trainChopTest[2] + trainChopTest[3] + trainChopTest[4]) / 3
trainChopTest[126] <- (trainChopTest[5] + trainChopTest[6] + trainChopTest[7]) / 3
trainChopTest[127] <- (trainChopTest[8] + trainChopTest[9] + trainChopTest[10]) / 3
for(i in 128:165){
trainChopTest[i] <- (trainChopTest[3*(i-128) + 11] + trainChopTest[3*(i-128)+12] + trainChopTest[3*(i-128)+13]) / 3
}

##############################
############################## Final train and test data
##############################

train <- trainAdjChop
train[166:(166+1200)] <- trainData[-1]

dim(train)

test <- trainChopTest
test[166:(166+1200)] <- testData

dim(test)
