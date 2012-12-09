module CourseraSample

type UserInfo =
    { Id : int
      EmailAddress : string
      FullName : string 
      Expiry : int64          
      Locale : string 
      TimeZone : string }

type Topic = 
    { Id : int
      Name : string
      ShortName : string
      ShortDescription : string
      PreviewLink : string
      SocialLink : string
      Video : string
      SmallIcon : string
      SmallIconHover : string
      LargeIcon : string
      Instructor : string
      Universities : University list 
      Categories : Category list }
    
and University =
    { Id : int
      Name : string
      ShortName : string
      Description : string }

and Category =
    { Id : int
      Name : string
      ShortName : string
      Description : string }

type Course =
    { Id : int
      Topic : Topic
      Duration : string
      StartDate : string
      HomeLink : string
      HasStarted : bool 
      HasFinished : bool 
      LectureSections : LectureSection list }

and LectureSection =
    { Title : string
      Completed : bool
      Lectures : Lecture list }

and Lecture = 
    { Id : int
      Title : string
      VideoUrl : string
      Viewed : bool }
