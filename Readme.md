![Github actions](https://github.com/ChinouneMehdi/PrayerTimes/workflows/Github%20actions/badge.svg)

# مواقيت الصلاة

برنامج لحساب مواقيت الصلاة مع أخذ التضاريس بعين الإعتبار

لبناء البرنامج عليك تثبيت إحدى مصنفات برامج فوترن (Fortran Compiler)

في أوبنتو أكتب التالي
```
sudo apt install gfortran cmake make
```
ثم أنشئ مجلدا للبناء وأكتب التعليمات الآتية:
```
mkdir build && cd build
cmake ..
make
```
ثم ما عليك إلا تحرير الملف "data.dat" بتغيير المنطقة الزمنية ودائرة العرض و خط الطول والشهر (مع السنة) المراد حساب مواقيت الصلاة له.

بعدها أكتب الأمر التالي
```
./main
```
ستظهر لك النتائج عبر الشاشة, كما يمكنك الإطلاع على الملف "output.dat" الذي يحتوي على النتائج.

***
**المراجع** :

http://www.icoproject.org/article/2001_salat.html
http://praytimes.org/wiki/Prayer_Times_Calculation
https://www.pveducation.org/pvcdrom/welcome-to-pvcdrom/properties-of-sunlight
