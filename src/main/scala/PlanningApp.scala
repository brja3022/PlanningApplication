import scala.collection.mutable.ListBuffer
import java.util.Date

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
        }
        return eventCleared
    }

}

class Event(title: String, startTime: Date, endTime: Date, location: String, travelTime: Double = 0){
    
}