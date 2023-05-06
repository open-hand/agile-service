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
    public static final String AGILE_PRODUCT_VERSION = "agile_product_version";
    public static final String AGILE_ISSUE_COMPONENT = "agile_issue_component";
    public static final String AGILE_ISSUE_LABEL = "agile_issue_label";
    public static final String WF_ISSUE_EXTEND = "wf_issue_extend";
    public static final String WF_ISSUE_SNAPSHOT = "wf_issue_snapshot";
    public static final String WF_SNAPSHOT = "wf_snapshot";
    public static final String AGILE_ISSUE_LINK_TYPE = "agile_issue_link_type";

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
}
