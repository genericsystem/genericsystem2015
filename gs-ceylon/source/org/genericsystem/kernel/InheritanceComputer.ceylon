import ceylon.collection {
	HashMap,
	LinkedList,
	ArrayList
}

class Printable(Iterable<Vertex|Projection> iterable) satisfies Iterable<Vertex|Projection> {
	shared actual Iterator<Vertex|Projection> iterator() => iterable.iterator();
	string => this.map((Vertex|Projection signature)=>signature.info).string;
}

class InheritanceComputer(Vertex origin) extends HashMap<Vertex, {Vertex *}>(){
	
	shared {<Vertex|Projection>*} getDesignatings(Vertex localBase){
		return Printable(getInheritings(localBase).flatMap((Vertex composite) => project(localBase,composite)));
	}
	
	shared {Vertex *} getInheritings(Vertex localBase) {
		Inheritings inheritings=Inheritings(localBase);
		inheritings.addComposites(localBase.getComposites(origin));
		for(Vertex superVertex in localBase.supers){
			inheritings.addComposites(getInheritingsFromCache(superVertex));
		}
		if(localBase.isMetaToAdd()){
			assert (exists localBaseMeta = localBase.meta );
			inheritings.addComposites(getInheritingsFromCache(localBaseMeta));
		}  
		return inheritings;  
		
	}
	
	{Vertex *} getInheritingsFromCache(Vertex superVertex) {
		variable {Vertex *}? result = get(superVertex);
		if (exists _result = result){
			return _result;
		} else {
			{Vertex *} _result= getInheritings(superVertex);
			put(superVertex, result = _result);
			return _result;
		}
	}
	
	class Inheritings(Vertex localBase) extends LinkedList<Vertex>()  {
		shared void addComposites({Vertex*} vertices){
			for(Vertex composite in vertices) {
				if(!isAlreadyOverrided(composite)){
					add(composite);
				}
			}
		}
		
		Boolean isAlreadyOverrided(Vertex composite) {
			for(Vertex alreadySelected in this) {
				if(alreadySelected.inheritsFrom(composite)){
					for(Vertex? component in composite.components){
						if(exists component) {
							if(!localBase.inheritsFrom(component) && !localBase.instanceOf(component)){
								if(origin.level==component.level){
									return false;
								}
							}
						}
					}
					return true;
				}
			}
			return false;
		}
		string => this.map((Vertex|Projection signature)=>signature.info).string;
	}
	
	shared {<Vertex|Projection> *} project(Vertex localBase,Vertex origin){	
		ArrayList<Vertex|Projection> projections = ArrayList<Vertex|Projection>();
		ArrayList<Vertex?> newComponents = ArrayList<Vertex?>();
		newComponents.addAll(origin.components);
		variable Boolean isProjection=false;
		variable Integer? indexOfBase=null;
		for(i->Vertex? component in origin.components.indexed){
			if(exists component){
				if(localBase.instanceOf(component)){
					newComponents.set(i,localBase);
					isProjection=true;
					indexOfBase=i;
				}
				break;//project on first elligible position for now
			}		
		}
		
		if(exists index = indexOfBase){
			for(i->Vertex? component in origin.components.indexed){
				if(i!=index){
					
				}
			}
			
		}
		if(isProjection) {
			projections.add(Projection(origin, newComponents.sequence()));
		}
		else {
			projections.add(origin);
		}
		return projections;
	}	
}	
