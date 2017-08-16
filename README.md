Generic System
==============

[![Join the chat at https://gitter.im/genericsystem/GenericSystem](https://badges.gitter.im/genericsystem/GenericSystem.svg)](https://gitter.im/genericsystem/GenericSystem?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

What is Generic System ?
------------------------
Generic System (GS) is an innovative **open source** information engine implemented in Java 
and is at once persistent, transactional, concurrenciel and flexible.
Generic System  fits into the NoSQL databases ecosystem by implementing a full Object Logic
and **with no dependency to a relational database**.

Check the example below :
-------------------------

### First example
```java
// Create a persistent engine named myDataBase
Engine engine = new Engine("myDataBase", System.getenv("HOME") + "/my_directory_path");

// Create a Vehicle with a Power
Generic vehicle = engine.addInstance("Vehicle");
Generic power = vehicle.addAttribute("Power");

// Instantiate a Vehicle with a Power 233
Generic myVehicle = vehicle.addInstance("myVehicle");
myVehicle.addHolder(power, 233);

// Persist changes
engine.getCurrentCache().flush();
```

More examples are available here: [Example project](https://github.com/genericsystem/genericsystem2014/tree/master/gs-example).

Outstanding facts :
-------------------
Some of the key highlights include:
* Persistent : Generic System is running in memory and has its own persistence mechanism. At startup, the GS Engine retrieve the most recent archived image
in a directory for this purpose and build the information system. Periodically GS creates images of the system that establishes a restore points.
Finally, when the engine stops, it archives a final image of the system to stop time.
* Transactional : Generic System is a transactional information system. The GS transaction can be compared to the relational database transaction.
 It can read, write, edit and delete data. All Writings are performed through a transaction which ensures they all run at the same time t. 
 The internal design of Generic System ensures that any reading of an object or set of objects done at a time t is reproducible. This arrangement guarantees transaction 
 isolation within the meaning of dirty reads, non-reproducible reads and phantom reads. For information,  to improve system availability, Read operations have generally 
 priority over Writing operations.
* Concurrentiel : Generic System allows multiple work in parallel. Algorithms are based on those called MVCC (multi-version Concurrency Control). They provide to GS users
 correctly isolated transactions and perfectly coherent requested data.
* Flexible : Generic System is recognized as highly restructurable as it provides the ability of changing the information structure in the same way that the information itself,
i.e. transactional and concurrentiel. Thus and unlike relational databases, GS allows to change the information  structure on the spot. 
A major shortcoming of relational databases is that the structure of the database is not treated in the same way that the data itself. 
It irremediably deteriorates the agility and evolutivity of applications by freezing the data structure on which they depend on.
This alternative is inconsistent  with the trend of modern application development methodologies  when specifications of a project have to evolve. 
Again, GS  favors to address the information structure as any other information, i.e. transactional and concurrentiel, allowing great flexibility.

Prerequisites
-------------

To operate generic system, it is essential to observe the instructions detailed below:
*  Install Java 8 (JRE)

* Set in the pom.xml of your project, the Generic Repository System:
```xml
<repository>
    <id>middlewarefactory</id>
    <url>http://middlewarefactory.com/repository</url>
    <releases>
        <enabled>true</enabled>
        <updatePolicy>daily</updatePolicy>
    </releases>
    <snapshots>
        <enabled>true</enabled>
        <updatePolicy>daily</updatePolicy>
    </snapshots>
</repository>
```

* Add dependency to Generic System in the pom.xml of your project:
```xml
<dependency>
    <groupId>org.genericsystem</groupId>
    <artifactId>gs-kernel</artifactId>
    <version>4.0-SNAPSHOT</version>
</dependency>
```

License
-------

The content of this repository is released under the License Apache version 2.0 as provided in the LICENSE file that accompanied this code.
