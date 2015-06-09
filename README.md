Generic System
==============

What is Generic System ?
------------------------
Generic System (GS) est un moteur d’information **open-source** développé par la société [Middleware Factory](http://www.middlewarefactory.com/).
Ce moteur innovant implémenté en Java est à la fois persistant, transactionnel, concurrentiel et restructurable.
Generic System vient s’intégrer dans l’écosystème des bases de données NoSQL, en mettant en œuvre une logique tout objet, et sans **aucune dépendance à une base de données relationnelle**.

Examples
--------

### First example
    // Create an engine named myDataBase and which is persistent
	Engine engine = new Engine("myDataBase", System.getenv("HOME") + "/my_directory_path");
	
    // Create a Vehicle with a Power
	Generic vehicle = engine.addInstance("Vehicle");
	Generic power = vehicle.addAttribute("Power");
	
    // Instantiate a Vehicle with a Power 233
    Generic myVehicle = vehicle.addInstance("myVehicle");
    myVehicle.addHolder(power, 233);
    
    // Persist changes
    engine.getCurrentCache().flush();

To see other examples, report to the page of the [Example project](https://github.com/genericsystem/genericsystem2014/tree/master/gs-example).

Key highlights
--------------
Some of the key highlights include:
* Persistant : Generic System est exécuté en mémoire et possède son propre mécanisme de persistance.
Au démarrage, le moteur de Generic System récupère l’image archivée la plus récente (dans un répertoire prévu à cet effet) et construit le système d’information.
Périodiquement, Generic System crée des images du système qui constituent des points de restauration.
Enfin, lorsque le moteur s’arrête, il archive une dernière image du système à l’instant d’arrêt.
* Transactionnel : Generic System est un système d’information transactionnel.
La transaction Generic System peut être comparée à la transaction d’une base de données relationnelle.
Elle sait lire, écrire, modifier et supprimer les données.  
Toutes les écritures sont exécutées au travers d’une transaction qui veille à ce qu’elles s’exécutent toutes à un même instant t.
La conception interne de Generic System garantit que toute lecture d’un objet ou d’un ensemble d’objets, faite à un instant t, est reproductible.
Cette disposition a permis de garantir l’isolation des transactions au sens des lectures sales, des lectures non reproductibles et des lectures fantômes.  
Pour information les opérations de lecture sont globalement prioritaires sur les opérations d’écritures afin d’améliorer la disponibilité du système.
* Concurrentiel : Generic System permet à plusieurs personnes de travailler en parallèle.
Les algorithmes sont inspirés de ceux dits [MVCC (MultiVersion Concurrency Control)](http://en.wikipedia.org/wiki/Multiversion_concurrency_control).
Ils assurent aux utilisateurs de Generic System des transactions correctement isolées et des données requêtées parfaitement cohérentes.
* Souple et personnalisable : Generic System est dit fortement restructurable car il offre la possibilité de modifier la structure de l’information de la même manière que l’information elle-même (c’est à dire de manière transactionnelle et concurrentielle).
Ainsi, et contrairement aux bases de données relationnelles, Generic System permet de modifier à chaud la structure de l’information.  
Un des défauts majeurs des bases de données relationnelles est que la structure de la base de données n’est pas adressée de la même manière que les données elles-mêmes.
Bien que ce choix soit raisonnable du point de vue de la complexité des développements et de la sécurité au sens large, il dégrade irrémédiablement la flexibilité et l’évolutivité des applications en figeant la structure des données sur laquelle elles reposent.
Ce choix est tout simplement en contradiction directe avec ce que préconisent les méthodologies modernes de développement d’applications, dans lesquelles on a besoin de faire évoluer le cahier des charges au cours d’un projet.  
Encore une fois, Generic System fait ici le choix d’adresser la structure de l’information comme n’importe quelle autre information, c’est à dire de manière transactionnelle et concurrentielle, ce qui permet une grande souplesse.

Prerequisites
-------------

Pour faire fonctionner Generic System, il est nécessaire :
* d'installer Java 8 (JRE)

* de configurer dans le pom.xml de votre projet, le repository Generic System :
<pre>
    &lt;repository&gt;
    	&lt;id&gt;middlewarefactory&lt;/id&gt;
    	&lt;url&gt;http://genericsystem.org/repository&lt;/url&gt;
    	&lt;releases&gt;
    		&lt;enabled&gt;true&lt;/enabled&gt;
    		&lt;updatePolicy&gt;daily&lt;/updatePolicy&gt;
    	&lt;/releases&gt;
    &lt;/repository&gt;
</pre>

* d'ajouter la dépendance à Generic System, toujours dans le pom.xml de votre projet :
<pre>
    &lt;dependency&gt;
    	&lt;groupId&gt;org.genericsystem&lt;/groupId&gt;
    	&lt;artifactId&gt;gs-mutability&lt;/artifactId&gt;
    	&lt;version&gt;3.0-SNAPSHOT&lt;/version&gt;
    &lt;/dependency&gt;
</pre>

Licence
-------

The content of this repository is released under the Licence Apache version 2.0 as provided in the LICENSE file that accompanied this code.
