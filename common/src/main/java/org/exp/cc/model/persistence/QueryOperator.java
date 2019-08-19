package org.exp.cc.model.persistence;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Logical query model.
 */
public class QueryOperator {
    private final List<Map<String, Map<String, Object>>> andOperator;
    private final List<Map<String, Map<String, Object>>> orOperator;

    @JsonCreator
    public QueryOperator(@JsonProperty("$and") final List<Map<String, Map<String, Object>>> andOperator,
                         @JsonProperty("$or") final List<Map<String, Map<String, Object>>> orOperator) {
        this.andOperator = andOperator;
        this.orOperator = orOperator;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final QueryOperator that = (QueryOperator) o;
        return Objects.equals(andOperator, that.andOperator) &&
                Objects.equals(orOperator, that.orOperator);
    }

    @Override
    public int hashCode() {
        return Objects.hash(andOperator, orOperator);
    }

}
