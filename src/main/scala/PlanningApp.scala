import scala.collection.mutable.ListBuffer
import java.util.Date
import java.text.SimpleDateFormat
import scala.io.StdIn._

class frontEnd(){
    def main(args: Array[String]): Unit = {
        println("Welcome to Brian's Planning Application")
        
  }
  def inputDate(): Date = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      val inputDate = readLine("Input Date in format yyyy-MM-dd HH:mm:ss")
      return dateFormat.parse(inputDate)
  }
}

class Schedule(name: String){
    var listEvents = ListBuffer[Event]()
    
    def addEvent(event: Event) = {
        val confirmation = checkConflicts(event)
        if(confirmation)(listEvents += event)
        
    }

    def deleteEvent(event: Event) = listEvents-=event

    def checkConflicts(event: Event): Boolean = {
        var eventCleared = true
        for(e <- listEvents){
            //check if events conflict
            if(event.endTime.isBefore(e.startTime)||event.startTime.isAfter(e.endTime)){
                //events do not conflict
            }else{
                eventCleared = resolveConflict(event,e)
                if (eventCleared == false) return eventCleared
            }
        }
        return eventCleared
    }

    def resolveConflict(newEvent: Event, oldEvent: Event): Boolean = {
        var eventCleared = false
        println(newEvent.title + " and " + oldEvent.title + " occur at overlapping times. How would you like to resolve this conflict?")
        println("1. Reschedule " + newEvent.title)
        println("2. Reschedule " + oldEvent.title)
        println("3. Keep both events at this time")
        println("4. Cancel " + newEvent.title)
        val choice  = readInt()
        choice match{
            case 1 => eventCleared = newEvent.reschedule()
            case 2 => eventCleared = oldEvent.reschedule()
            case 3 => eventCleared = true
            case 4 => eventCleared = false
            case default => eventCleared = false
        }
        return eventCleared
    }

}

class Event(title: String, startTime: Date, endTime: Date, location: String, travelTime: Double = 0, schedule: Schedule, frontEnd: frontEnd){
    val title = title
    var startTime = startTime
    var endTime = endTime
    var location = location
    var travelTime = travelTime

    def reschedule(): Boolean = {
        print("Start Time ")
        startTime = frontEnd.inputDate()
        println()
        print("End Time ")
        endTime = frontEnd.inputDate()
        println
        return schedule.checkConflicts(this)
    }
    
}