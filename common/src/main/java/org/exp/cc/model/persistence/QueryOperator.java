package org.exp.cc.model.persistence;

import com.fasterxml.jackson.annotation.JsonCreator;

import java.util.Map;
import java.util.Objects;

/**
 * Query operator
 */
public class QueryOperator {
    private final Map<String, Object> operator;

    @JsonCreator
    public QueryOperator(final Map<String, Object> operator) {
        this.operator = operator;
    }

    public Map<String, Object> getOperator() {
        return operator;
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
        return Objects.equals(operator, that.operator);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator);
    }
}
