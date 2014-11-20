class Snapshot(Vertex|Finished first,Callable<Vertex|Finished,[Vertex]> advance) satisfies {Vertex*} {
	shared actual Iterator<Vertex> iterator() {
		variable Vertex|Finished  nextV = first;
		object iterator satisfies Iterator<Vertex> {
			shared actual Vertex|Finished next() {
				Vertex|Finished next = nextV;
				if (is Vertex next) {
					nextV=advance(next);
				}
				return next;
			}
		}
		return iterator;
	}
	
	string => this.map((Vertex|Projection signature)=>signature.info).string;
}