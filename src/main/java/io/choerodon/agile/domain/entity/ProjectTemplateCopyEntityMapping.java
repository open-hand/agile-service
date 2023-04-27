package io.choerodon.agile.domain.entity;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author superlee
 * @since 2023/4/27
 */
public class ProjectTemplateCopyEntityMapping {

    public static final String AGILE_ISSUE = "agile_issue";

    private String table;

    private Map<String, Map<Long, Long>> sourceTargetMapping = new HashMap<>();

    public ProjectTemplateCopyEntityMapping put(String table, Long sourceId, Long targetId) {
        sourceTargetMapping.computeIfAbsent(table, k -> new HashMap<>()).put(sourceId, targetId);
        return this;
    }

    public Map<Long, Long> getByTable(String table) {
        return sourceTargetMapping.get(table);
    }

    public Long getByTableAndSourceId(String table, Long sourceId) {
        return sourceTargetMapping.getOrDefault(table, Collections.emptyMap()).get(sourceId);
    }

    public String getTable() {
        return table;
    }

    public ProjectTemplateCopyEntityMapping setTable(String table) {
        this.table = table;
        return this;
    }

}
