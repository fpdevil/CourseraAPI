# (Haskell bindings for Coursera Web API)

This is a haskell based service for invking the Coursera API.

Very much a work in progress :running:

Finished till now.
 * Raw Service invocation done :heavy_check_mark:

Tasks
 * Additional operations and provide a single point of entry  :x:

For building and running as a project using cabal:

```
> cabal build
Building CourseraAPI-0.1.0.0...
Preprocessing executable 'CourseraAPI' for CourseraAPI-0.1.0.0...
[1 of 6] Compiling Partners         ( src/Partners.hs, dist/build/CourseraAPI/CourseraAPI-tmp/Partners.o )
[2 of 6] Compiling Instructors      ( src/Instructors.hs, dist/build/CourseraAPI/CourseraAPI-tmp/Instructors.o )
[3 of 6] Compiling Courses          ( src/Courses.hs, dist/build/CourseraAPI/CourseraAPI-tmp/Courses.o )
[4 of 6] Compiling Helpers          ( src/Helpers.hs, dist/build/CourseraAPI/CourseraAPI-tmp/Helpers.o )
[5 of 6] Compiling CourseraAPI      ( src/CourseraAPI.hs, dist/build/CourseraAPI/CourseraAPI-tmp/CourseraAPI.o )
[6 of 6] Compiling Main             ( src/Main.hs, dist/build/CourseraAPI/CourseraAPI-tmp/Main.o )
Linking dist/build/CourseraAPI/CourseraAPI ...

```

If no options are provided while running the compiled build, it will
show the available operations.
```
> dist/build/CourseraAPI/CourseraAPI
## Response from CourseraAPI ##
Invoke any of the following operations:
getCourseByName
getCourseByShortName
getCourseById
getPartnersById
getPartnerUniversities
getPartnerUniversitiesByShortName
getInstructorById
getInstructorByName
```

With the options...
```
> dist/build/CourseraAPI/CourseraAPI getCourseByName
## Response from CourseraAPI ##
"Creative Programming for Digital Media & Mobile Apps"
"Gamification"
"Dealing With Missing Data"
"Vital Signs: Understanding What the Body Is Telling Us"
"Modern Art & Ideas"
"The Evolving Universe"
"Introduction to Big Data"
"LearnToMod For Educators"
"Water: The Essential Resource"
"Bioinformatic Methods I"
"Machine Learning Capstone: An Intelligent Application with Deep Learning"
```

###GHCI
    And here's the way to run from ghci :exclamation:

> If the call is a success and able to fetch the data, we get response in list :+1:

```haskell
fmap (take 10) getCourseByName

-- Here is a succesful response
["Creative Programming for Digital Media & Mobile Apps",
 "Gamification",
 "Vital Signs: Understanding What the Body Is Telling Us",
 "Modern Art & Ideas","The Evolving Universe",
 "Introduction to Big Data",
 "LearnToMod For Educators",
 "Water: The Essential Resource",
 "Bioinformatic Methods I",
 "Programming Cloud Services for Android Handheld Systems: Security"]
```

> Else we get an empty list [] :-1:

### Version
      1.0.0

### TODO
    There is a lot to be added.
    * Test Cases
    * Filtering the results
    * Include details for other service based on filtering of the results. And above all accommodate through the main.

### Links referred:
 * [Indisputable LYAH](http://learnyouahaskell.com) This is the book which got me into Haskell
 * [CourseraAPI](https://tech.coursera.org/app-platform/catalog/) Contains all details of the Coursera exposed API's.
 * [SOH](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json) Documentation from School Of Haskell
