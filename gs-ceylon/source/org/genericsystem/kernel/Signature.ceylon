interface Signature {
	shared formal Vertex? meta;
	shared formal Vertex[] supers;
	shared formal Value? content;
	shared formal Vertex?[] components;
	shared default String info => "("+(meta?.string else "<null>")+")"+supers.string+string+components.string;	
}