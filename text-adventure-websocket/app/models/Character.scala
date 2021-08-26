package models

class Character (var health:Int = 15, var items:String = "", var hasKey:Boolean = false) {
    
    var maxHealth:Int = 15

    def decreaseHealth(amount:Int){
        health -= amount
        if (health < 0){
            health = 0
        }
    }
  
    def increaseHealth(amount:Int){
          health += amount
    }

    def isAlive(): Boolean={
        health <= 0
    }

    def addKey(){
        hasKey = true
        items = "Key"
    }

    def useKey(){
        hasKey = false
        items = ""
    }

    def reset(){
        health = maxHealth
        items = ""
        hasKey = false
    }
}
