import pytest

import semtk3
import rack


@pytest.mark.xfail(reason="See discussion on RACK#193")
def test_invariant_nodegroups(rack_url: str) -> None:
    # There is some duplication here with rack.run_count_query, but not a lot.
    semtk3.SEMTK3_CONN_OVERRIDE = rack.sparql_connection(rack_url, rack.DEFAULT_DATA_GRAPH, None)
    semtk_table = semtk3.count_by_id("query nonuniqueIdentifiers")
    assert 0 == int(semtk_table.get_rows()[0][0])
