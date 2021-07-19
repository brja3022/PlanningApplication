import scala.collection.mutable.ListBuffer
import java.util.Date
import java.text.SimpleDateFormat
import scala.io.StdIn._
import scala.collection.mutable.Map

object M{
    def main(args: Array[String]): Unit = {
        println("Welcome to Brian's Planning Application")
        val name = readLine("Insert Name")
        val schedule = new Schedule(name)
        val frontEnd = new FrontEnd()
        val firstEvent = frontEnd.createEvent(schedule)

    }
}

class FrontEnd(){

  def inputDate(): Date = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      val inputDate = readLine("Input Date in format yyyy-MM-dd HH:mm:ss")
      return dateFormat.parse(inputDate)
  }

  def createEvent(schedule: Schedule, frontEnd: FrontEnd = this): Event = {
      val title = readLine("Event Title ")
      println("Start Date/Time ")
      val start = inputDate()
      println("End Date/Time ")
      val end = inputDate()
      val location = readLine("Event Location ")
      println("Travel Time ")
      val tt = readDouble()
      return new Event(title,start,end,location,tt,schedule,frontEnd)
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
            if(event.getEndTime().before(e.getStartTime())||event.getStartTime().after(e.getEndTime())){
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
        println(newEvent.getTitle() + " and " + oldEvent.getTitle() + " occur at overlapping times. How would you like to resolve this conflict?")
        println("1. Reschedule " + newEvent.getTitle())
        println("2. Reschedule " + oldEvent.getTitle())
        println("3. Keep both events at this time")
        println("4. Cancel " + newEvent.getTitle())
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

    def frequentLocations(): Map[String,Int] ={
        var commonLocations = scala.collection.mutable.Map[String,Int]()
        for (event <- listEvents){
            if(commonLocations.contains(event.getLocation())){
                commonLocations(event.getLocation() ) = commonLocations(event.getLocation())+1
            }else{
                commonLocations +=(event.getLocation()-> 1)
            }
        }
        return commonLocations
    }

}

class Event(eventTitle: String, eventStartTime: Date, eventEndTime: Date, eventLocation: String, eventTravelTime: Double = 0, schedule: Schedule, frontEnd: FrontEnd){
    var title = eventTitle
    var startTime = eventStartTime
    var endTime = eventEndTime
    var location = eventLocation
    var travelTime = eventTravelTime

    def getTitle(): String = return title
    def getStartTime(): Date = return startTime
    def getEndTime(): Date = return endTime
    def getLocation(): String = return location
    def getTravelTime(): Double = return travelTime

    def reschedule(): Boolean = {
        print("Start Time ")
        startTime = frontEnd.inputDate()
        println()
        print("End Time ")
        endTime = frontEnd.inputDate()
        println
        return schedule.checkConflicts(this)
    }

    def alert() {
        val current = new Date()
        val timeToEvent = current.getTime()-getStartTime.getTime()
        if(timeToEvent < getTravelTime()+5){
            println(getTitle() + " will start in " + timeToEvent)
        }
    }
    
}