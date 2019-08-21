package org.exp.cc.model.persistence;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Query criteria.
 */
public class QueryCriteria {
    private final List<Map<String, QueryFields>> query;

    @JsonCreator
    public QueryCriteria(@JsonProperty("query") final List<Map<String, QueryFields>> query) {
        this.query = query;
    }

    public List<Map<String, QueryFields>> getQuery() {
        return query;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final QueryCriteria that = (QueryCriteria) o;
        return Objects.equals(query, that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(query);
    }

}
