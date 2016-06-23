/*
	This functions return the predicted values
	(either classes or probabilities) for a tree
	The code is based on the one employed in 'TWIX' by Popatov.

	'pred_imptree' is the function called by R using 'Call'
	
	'getListElement' and 'pred_value' are auxiliary functions:
		'pred.value':			Predicting the vlaues for an oservations
		'getListElement':	Returning the element inside a named 
												array matching the provided name
*/


#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>

/*
	Input:	list:	Named array
					str:	String to be matched to the names of the array
	
	Output: elmt:	Object stored at index matching 'str' in the names
									of the array; NULL if no match is attained
*/
SEXP getListElement(SEXP list, const char *str) {
/*
	Initializing the returned element 'elmt' with NULL
	and extracting the names of the array 
*/
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;
/*
	Iteration over the array
*/
    for (i = 0; i < length(list); i++) {
/*
	When the names associated with the index matches the provided name 'str'
	'elmt' is assigned the value stored in the array at that index
	and breaks the iteration
*/
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
            elmt = VECTOR_ELT(list, i);
            break;
        }
    }
    return elmt;
}

/*
	The following function does the cimbing within a tree. It consideres the 
	(Sub-)Tree's root node and descends into that node, which is labeled
	the same as the according value in the given observation for the splitting
	variable defined in the node. In case such a daughter node exists, the
	function is self-called from within that daughter node.
	In case the node is a leaf the prediction is returned.

	Input:	data:	data of one observation containing all feature variable
		tree:	(Sub-)Tree structure
		xvar:	names of the feature variabes
		catlev:	named list containing all category names
				for all feature variables

	Final Output: 	A list containing probabilities (upper and lower)
*/

SEXP pred_value(SEXP data, SEXP tree, SEXP xvar, SEXP catlev) {
    int i=0,j=0,id;
    const char *type;
/*
	link method with storage names within a list
*/
    type = "Probs";
    SEXP knot = R_NilValue;
    int lxvar = LENGTH(xvar);
/*
	First part of the tree structure (name of split variable and its values)
*/
    SEXP rest = VECTOR_ELT(tree, 0);
/*
	Tree structure (Knot or list of Knots)
*/
    SEXP knots = VECTOR_ELT(tree, 1);
/*
	Name of splitting variable
*/
    SEXP rest_spvar = VECTOR_ELT(rest, 0);
/*
	Name of possible splitpoints (They are in the same order as the knots are)
	Currently ununsed, may be important in the future	

    SEXP names = VECTOR_ELT(rest, 2);
*/
/*
	Dealing with the case when the maximal tree consists of just a single node
*/
    if (strcmp(CHAR(STRING_ELT(rest_spvar,0)), "<None>") == 0) {
        knot = knots;
    } else {

/*
	Obtaining the knot within a knotlist according to the value in
	the provided observation for the splitting variable in the knot
*/

        for (i=0; i<lxvar; i++) {
            if (strcmp(CHAR(STRING_ELT(rest_spvar,0)), CHAR(STRING_ELT(xvar, i))) == 0) {
                j=i;
            }
        }
        SEXP clev = getListElement(catlev,CHAR(STRING_ELT(xvar, j)));
        id = REAL(data)[j];
        for (i=0; i < LENGTH(knots); i++) {
            if (strcmp(CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(knots, i), 0),0)), CHAR(STRING_ELT(clev, id-1))) == 0) {
                knot = VECTOR_ELT(VECTOR_ELT(knots, i), 1);
                break;
            }
        }
    }
/*
	If no knot could be found, i.e we are in a leaf,
	then class/probabilities of the leaf is extracted.
*/
    if (knot == R_NilValue) {
        return(getListElement(rest,type));
    } else {
/*
	Daughter node is root node of a non-empty subtree.
	Then call function again with daughter node as root
*/
        if (LENGTH(knot) == 2) {
            return(pred_value(data, knot, xvar, catlev));
        } else {
/*
	This happens when maximal tree is just a node.
	Extracting the probabilities like in a leaf
*/
            return(getListElement(knot,type));
        }
    }
}

/*
	main function to be called from R.
	It initiates calls to 'pred_value' for each observation provided

	Input:	DATA:	data.frame with observations as row
				and feature variable in columns
		TREE:	Complete Tree structure
		XVAR:	names of all feature variabes
		cat:	named list containing all category names
				for all feature variables
	Final Output: 	A list containing probabilities (upper and lower)

*/

SEXP pred_imptree( SEXP DATA, SEXP TREE, SEXP XVAR, SEXP cat) {

    SEXP tree_ans, tmp_data;
    int NDATA = LENGTH(DATA);
    tree_ans = PROTECT(allocVector(VECSXP,NDATA));
    int j,k=0;
/*
	Iteration over all rows (observations)
*/
    for (j=0; j < NDATA; j++) {
/*
	Extracting the data of a sinlge observation and protecting it
*/
        tmp_data = PROTECT(coerceVector(VECTOR_ELT(DATA,j),REALSXP));
/*
	Obtaining the prediction of it and assigning to list
*/
        SET_VECTOR_ELT(tree_ans, k, pred_value(tmp_data,TREE,XVAR,cat));
/*
	Freeing the memory allocated by the observation for reuse
*/
        UNPROTECT(1);
        k++;
    }
    UNPROTECT(1);
    return(tree_ans);

}
