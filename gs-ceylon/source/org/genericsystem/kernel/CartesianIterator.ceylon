import ceylon.collection {
	ArrayList
}

class CartesianIterator({Vertex?*}[] iterables) satisfies Iterator<Vertex?[]>{
	
	ArrayList<Vertex?> values=ArrayList<Vertex?>(iterables.size);
	ArrayList<Iterator<Vertex?>> iterators=ArrayList<Iterator<Vertex?>>(iterables.size);
	
	shared actual Vertex?[]|Finished next() {
		variable Integer cursor=-1;
		for (i in 0..iterables.size-1){
			Iterator<Vertex?>? it = iterators.get(i);
			assert (exists it);
			Vertex?|Finished vertex = it.next();
			if (is Vertex? vertex) {
				cursor=i;
				values.set(i,vertex);
				break;
			}
		}
		if(cursor==-1){
			return finished;
		}
		
		for ( i in cursor + 1.. iterables.size-1) {
			Iterable<Vertex?>? iterable = iterables.get(i);
			assert (exists iterable);
			Iterator<Vertex?> it = iterable.iterator();
			iterators.set(i,it);
			Vertex?|Finished vertex = it.next();
			switch (vertex)
			case(is Vertex?) {
				values.set(i,vertex);
			}
			case(is Finished) {
				return finished;
			}
		}
		return [for(vertex in values)vertex];	
	}
}

