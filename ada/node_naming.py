"""
This module contains facilities for extracting traceability-comments Ada program.
"""

__copyright__ = "Copyright (c) 2020, Galois, Inc."

import logging
from typing import Callable, Dict, NewType, Optional, Set
import urllib.parse
import re

import libadalang as lal

from ada_visitor import AdaVisitor

logger = logging.getLogger('ada')

# Type to abstract over what nodes we put in the graph. Can be turned into a
# concrete or abstract class if the complexity ever demands it.
GraphNode = lal.Name

def warn_about_node(node: lal.AdaNode) -> None:
    """
    Emits a warning because we could not resolve the definition site for the
    given node.
    """
    name = node.text
    loc = node.full_sloc_image[:-2]
    logger.warning(f"Could not resolve the name {name} as it appears in {loc}.")

def safe_xref(node: lal.AdaNode) -> Optional[lal.DefiningName]:
    """
    p_gnat_xref fails catastrophically (dereferences a null pointer), this
    wraps it up nicely.
    """
    try:
        return node.p_gnat_xref()
    except lal.PropertyError:
        warn_about_node(node)
        return None

NodeDisplayName = NewType("NodeDisplayName", str)
NodeFile = NewType("NodeFile", str)
NodeIdentifier = NewType("NodeIdentifier", str)
NodeKey = NewType("NodeKey", str)
NodeURI = NewType("NodeURI", str)

def get_node_display_name(node: GraphNode) -> NodeDisplayName:
    """Computes the name to display for a node."""
    return NodeDisplayName(node.text)

def get_node_file(node: GraphNode) -> Optional[NodeFile]:
    """"
    Returns the name of the file within which this node was defined, assuming
    we succeed to resolve the reference.
    """
    xref = safe_xref(node)
    # NOTE: if we need the full path, we can use:
    #   xref.p_basic_decl.unit.filename
    # NOTE: full_sloc_image returns "file.ada:<line>:<column>: " but we just
    # want the filename
    if xref is None:
        return None
    return NodeFile(xref.p_basic_decl.full_sloc_image.split(":")[0])

def get_node_key(node: GraphNode) -> NodeKey:
    """
    Computes a key we can use for identifying this node uniquely.
    """
    xref = safe_xref(node)
    if xref is None:
        return NodeKey(str(node))
    return NodeKey(str(xref))

def get_node_identifier(node: GraphNode) -> NodeIdentifier:
    """
    Computes the identifier to use in the database.
    """
    return NodeURI(f'SWCOM_{node.doc_name.lower()}')

def get_node_uri(node: GraphNode) -> NodeURI:
    """Computes the URI to use for a node."""
    # NOTE: we need some encoding scheme, because Ada allows operator
    # overloading, so the functions may be called "&" for instance.
    encode = urllib.parse.quote_plus
    xref = node.p_gnat_xref()
    if not xref:
        return NodeURI(f'SWCOM_{encode(node.p_relative_name.p_canonical_text)}')
        # raise Exception(f"The reference to node {node} could not be resolved.")
    return NodeURI(f'SWCOM_{encode(xref.p_basic_decl.p_canonical_fully_qualified_name)}')    
