package org.exp.cc.processor.mongo.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.LogicalOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.mongo.ComparisonOperatorProcessor;
import org.exp.cc.processor.mongo.LogicalOperatorProcessor;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Query processor.
 */
@Component
public class MongoLogicalOperatorProcessorImpl implements LogicalOperatorProcessor {
    private final ComparisonOperatorProcessor comparisonOperatorProcessor;

    private final Map<String, Function<QueryFields, Criteria>> logicalOperatorProcessor =
            ImmutableMap.of(
                    LogicalOperator.AND.getOperator(), this::andLogicalProcessor,
                    LogicalOperator.OR.getOperator(), this::orLogicalProcessor
            );

    public MongoLogicalOperatorProcessorImpl(MongoComparisonOperatorProcessor comparisonOperatorProcessor) {
        this.comparisonOperatorProcessor = comparisonOperatorProcessor;
    }

    @Override
    public Criteria generateCriteria(final QueryCriteria queryCriteria) {
        final Criteria[] criteria = queryCriteria.getQuery()
                .stream()
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

                    logicalQueries.forEach((key, value) -> {
                        if (this.logicalOperatorProcessor.get(key) == null) {
                            throw new ApplicationRuntimeException(String.format("Logical operator processor not found for %s", key));
                        }

                        logicalCriteria.add(this.logicalOperatorProcessor.get(key).apply(value));
                    });

                    return logicalCriteria;
                })
                .flatMap(Collection::stream)
                .toArray(Criteria[]::new);

        return new Criteria().andOperator(criteria);
    }

    private Criteria andLogicalProcessor(final QueryFields queryFields) {
        return new Criteria().andOperator(this.comparisonOperatorProcessor.generateCriteria(queryFields));
    }

    private Criteria orLogicalProcessor(final QueryFields queryFields) {
        return new Criteria().orOperator(this.comparisonOperatorProcessor.generateCriteria(queryFields));
    }

}
