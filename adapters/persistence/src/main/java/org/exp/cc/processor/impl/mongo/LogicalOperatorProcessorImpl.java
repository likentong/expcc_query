package org.exp.cc.processor.impl.mongo;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.LogicalOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.exp.cc.processor.LogicalOperatorProcessor;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Query processor.
 */
@Component
public class LogicalOperatorProcessorImpl implements LogicalOperatorProcessor {
    private final ComparisonOperatorProcessor comparisonOperatorProcessor;

    private final Map<String, Function<QueryFields, Criteria>> logicalOperatorProcessor =
            ImmutableMap.<String, Function<QueryFields, Criteria>>builder()
                    .put(LogicalOperator.AND.getOperator(), this::andLogicalProcessor)
                    .put(LogicalOperator.OR.getOperator(), this::orLogicalProcessor)
                    .build();

    public LogicalOperatorProcessorImpl(ComparisonOperatorProcessor comparisonOperatorProcessor) {
        this.comparisonOperatorProcessor = comparisonOperatorProcessor;
    }

    @Override
    public Criteria generateCriteria(final QueryCriteria queryCriteria) {
        checkArgument(queryCriteria != null, "queryCriteria cannot be null.");
        checkArgument(queryCriteria.getQuery() != null, "queryCriteria query cannot be null.");

        final Criteria[] criteria = queryCriteria.getQuery()
                .stream()
                .filter(logicalQueries -> logicalQueries.size() != 0)
                .map(logicalQueries -> {
                    //does not support nested operators
                    if (logicalQueries.size() > 1) {
                        throw new InvalidQueryException(String.format("Nested operators not supported. Found [%s]", String.join(",", logicalQueries.keySet())));
                    }

                    final List<String> validLogicalOperator = Arrays.stream(LogicalOperator.values())
                            .map(LogicalOperator::getOperator)
                            .collect(Collectors.toList());

                    //check logical operator is in enum
                    logicalQueries.keySet()
                            .stream()
                            .filter(logicalOperator -> !validLogicalOperator.contains(logicalOperator))
                            .findAny()
                            .ifPresent(operator -> {
                                throw new InvalidQueryException(String.format("%s logical operator not supported.", operator));
                            });

                    final List<Criteria> logicalCriteria = new ArrayList<>();

                    logicalQueries.entrySet()
                            .stream()
                            .filter(entrySet -> !entrySet.getValue().getFields().isEmpty())
                            .forEach(entrySet -> {
                                if (this.logicalOperatorProcessor.get(entrySet.getKey()) == null) {
                                    throw new ApplicationRuntimeException(String.format("Logical operator processor not found for %s", entrySet.getKey()));
                                }

                                logicalCriteria.add(this.logicalOperatorProcessor.get(entrySet.getKey()).apply(entrySet.getValue()));
                            });

                    return logicalCriteria;
                })
                .filter(element -> !element.isEmpty())
                .flatMap(Collection::stream)
                .toArray(Criteria[]::new);

        if (criteria.length == 0) {
            return new Criteria();
        } else if (criteria.length == 1) {
            return criteria[0];
        }

        return new Criteria().andOperator(criteria);
    }

    private Criteria andLogicalProcessor(final QueryFields queryFields) {
        final Criteria[] criteria = this.comparisonOperatorProcessor.generateCriteria(queryFields);
        return (criteria.length == 1) ? criteria[0] : new Criteria().andOperator(criteria);
    }

    private Criteria orLogicalProcessor(final QueryFields queryFields) {
        final Criteria[] criteria = this.comparisonOperatorProcessor.generateCriteria(queryFields);
        return (criteria.length == 1) ? criteria[0] : new Criteria().orOperator(criteria);
    }

}
