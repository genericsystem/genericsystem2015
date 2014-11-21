import ceylon.collection {
	HashSet,
	linked,
	Hashtable,
	ArrayList
}

shared alias Value => Integer | String | Float | Boolean | Character;

class Vertex(meta,supers,content,components) satisfies Signature {
	shared actual Vertex? meta;
	shared actual Vertex[] supers;
	shared actual Value? content;
	shared actual Vertex?[] components;
	
	variable Vertex|Finished firstInstance=finished;
	variable Vertex|Finished firstInheriting=finished;
	variable Vertex|Finished firstComposite=finished;
	
	variable Vertex|Finished nextInstance=finished;
	variable <Vertex|Finished>[] nextInheritings =[for(Vertex superV in supers) finished];
	variable <Vertex|Finished>[] nextComposites = [for(Vertex component in HashSet<Vertex>(linked,  Hashtable(), components.coalesced)) finished];
	
	Vertex|Finished nextInheriting(Vertex superV) {
		if(exists index = supers.firstIndexWhere((Vertex vertex) => vertex===superV)){
			if(exists result = nextInheritings[index]) {
				return result;
			}
		}
		return finished;
		
	}
	
	Vertex|Finished nextComposite(Vertex component){
		if(exists index = components.firstIndexWhere((Vertex vertex) => vertex===component)){
			if(exists result = nextComposites[index]) {
				return result;
			}
		}       
		return finished;   
	}
	
	shared {Vertex*} instances =>  Snapshot(firstInstance,(vertex)=>vertex.nextInstance);
	shared {Vertex*} inheritings =>  Snapshot(firstInheriting,(vertex)=>vertex.nextInheriting(this));
	shared {Vertex*} composites =>  Snapshot(firstComposite,(vertex)=>vertex.nextComposite(this));
	
	shared Integer level => 1+(meta?.level else -1);
	
	shared Vertex addInstance(Value? content,Vertex?[]  components=[])=> addInheritingInstance([], content, components);
	
	shared Vertex addInheriting(Vertex? meta, Value? content,Vertex?[]  components=[])=> Vertex(meta,[this], content, components).plug();
	
	shared Vertex addInheritingInstance(Vertex[] supers,Value? content,Vertex?[]  components=[])=> Vertex(this, supers, content, components).plug();
	
	shared Boolean inheritsFrom(Vertex superVertex) {
		if (this==superVertex) {
			return true;
		}
		//if (level != superVertex.level) {
		//    return false;
		//}
		return supers.any((vertex) => vertex.inheritsFrom(superVertex));
	}
	shared Boolean instanceOf(Vertex metaVertex)=>meta?.inheritsFrom(metaVertex) else false;//this.inheritsFrom(metaVertex) ;
	shared {Vertex*} getComposites(Vertex metaComposite) =>  composites.filter((Vertex composite) => composite.instanceOf(metaComposite));
	
	
	Vertex plug(){
		nextInstance = meta?.firstInstance else finished;
		nextInheritings = [for(superV in supers) superV.firstInheriting];
		nextComposites = [for(component in HashSet<Vertex>(linked,  Hashtable(), components.coalesced)) component.firstComposite]; 
		if(exists meta) {
			meta.firstInstance=this;
		}
		for(Vertex superV in supers) {
			superV.firstInheriting = this;
		}     
		for(Vertex component in HashSet(linked,  Hashtable(), components.coalesced)) {
			component.firstComposite = this;
		}
		return this;
	}
	
	shared actual String string => content?.string else "<null>";
	
	//shared String info => "("+(meta?.string else "<null>")+")"+supers.string+string+components.string;
	shared String detailedInfo => "********************************************************\n"
			+info+"\n"
			+"    instances :    "+instances.string+"\n"
			+"    inheritings :  "+inheritings.string+"\n"
			+"    composites :   "+composites.string+"\n"
			+"********************************************************\n";
	
	
	shared Boolean isMetaToAdd(){
		if(exists meta) {
			for(Vertex superVertex in supers){
				if(exists localBaseSuperMeta=superVertex.meta) {
					if(superVertex==meta){
						return false;
					}
				}
			}
			return true;
		}
		return false;
	}
	
	shared {Vertex*} allInheritings => {this}.chain(inheritings).flatMap((inheriting) => inheriting.allInheritings);
	
	shared {Vertex*} allInstances => allInheritings.flatMap((inheriting) => inheriting.instances);

	
	shared {Vertex *} getInheritingComposites(Vertex origin) => InheritanceComputer(origin).getInheritings(this);
	
	shared {<Vertex|Projection> *} getDesignatings(Vertex origin) => InheritanceComputer(origin).getDesignatings(this);
	
	shared Vertex?[][] projections(Integer pos) {
		return [for (i->component in components.indexed) 
		if(i!=pos)
		component?.descendantsToProject() else [component]
		];
	}
	
	Vertex?[] descendantsToProject()=> allInstances.sequence();
	
	shared {Vertex?[]*} findCombinations(Integer pos) {
		return [for (Vertex?[] combination in computeCartesian(projections(pos))) 
			if (isEligible(combination))
			combination
		];
	}
	
	shared {Vertex?[]*} computeCartesian(Vertex?[][] combinations) {
		Integer n = combinations.size;
		variable Integer solutions = 1;
		for (Vertex?[] vector in combinations) {
			solutions *= vector.size;
		}
		ArrayList<Vertex?[]> allCombinations = ArrayList<Vertex?[]>(solutions);
		for (i in 0..solutions) {
			ArrayList<Vertex?> combination = ArrayList<Vertex?>(n);
			variable Integer j = 1;
			for (Vertex?[] vec in combinations) {
				combination.add(vec[((i / j) % vec.size)]);
				j *= vec.size;
			}
			allCombinations.set(i,combination.sequence());
		}
		return allCombinations;
	}
	
	Boolean isEligible(Vertex?[] combination){
		return true;
		//this.unambigousFirst(getAllInheritingsSnapshotWithoutRoot().filter(next -> ((GenericImpl) next).inheritsFrom(((GenericImpl) next).filterToProjectVertex(components, pos)))) == null)
	}	
}

